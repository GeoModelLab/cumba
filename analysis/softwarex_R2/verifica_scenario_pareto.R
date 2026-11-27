# Verifica 4/4 - esperimento di scenario, fronte di Pareto e Table 3, con cumba 0.1.0 (CRAN)
# Eseguire dalla cartella cumba_R_package. Richiede: cumba, dplyr, lubridate, lme4.
# RERUN = FALSE: legge i risultati salvati (analysis/data/results_irrigation_optimizer_calibrated.rds), pochi secondi.
# RERUN = TRUE : riesegue le 256 x 22 = 5632 simulazioni con il pacchetto installato (circa 30-60 minuti).
RERUN <- FALSE
library(cumba); library(dplyr); library(lubridate); library(lme4)
data(cumbaParameters)
cat("versione cumba:", as.character(packageVersion("cumba")), " | RERUN =", RERUN, "\n")

## ---- impostazioni dell'esperimento (come in analysis/original_scripts/optimizeTomatoCultivation.R) ----
LEVELS <- c(0.6, 0.7, 0.8, 0.9)           # wsLevel per ciascuna fase
DOYS   <- c(110, 115, 120, 125)           # date di trapianto
par_sc <- cumbaParameters
par_sc$CycleLength$value          <- cumbaParameters$CycleLength$value + 150   # 1260 gradi-giorno
par_sc$FieldCapacity$value        <- 0.35
par_sc$WiltingPoint$value         <- 0.15
par_sc$FruitWaterContentMax$value <- 0.905
TURN_MIN <- 2L; STOP_CYCLE <- 90; EFFICIENCY <- 0.9

run_design <- function() {
  weather <- read.csv("analysis/private/weather_foggia.csv") |>
    mutate(Tx = TMAX, Tn = TMIN, P = RAIN, Site = "Foggia", Lat = 41,
           DATE = as.Date(MDATE, format = "%m/%d/%Y")) |>
    filter(year(DATE) >= 2000)
  g <- expand.grid(td = DOYS, ri = LEVELS, r = LEVELS, v = LEVELS)
  bind_rows(lapply(seq_len(nrow(g)), function(i) {
    sim <- cumba_scenario(weather, par_sc, estimateRad = TRUE, estimateET0 = TRUE,
      irrigationStrategy = list(vegetative   = list(wsLevel = g$v[i],  turnMin = TURN_MIN),
                                reproductive = list(wsLevel = g$r[i],  turnMin = TURN_MIN),
                                ripening     = list(wsLevel = g$ri[i], turnMin = TURN_MIN)),
      irrigationStopCycle = STOP_CYCLE, transplantingDOY = g$td[i],
      irrigationEfficiency = EFFICIENCY, fullOut = TRUE)
    irr <- sim |> group_by(year) |> filter(cycleCompletion < 100) |>
      summarise(n_irrigation = sum(irrigation > 0), irrigation = sum(irrigation), .groups = "drop")
    # raccolta = primo giorno con ciclo completato al 100%
    sim |> group_by(year) |> filter(cycleCompletion >= 100) |> slice_head(n = 1) |> ungroup() |>
      transmute(year, yield = fruitFreshWeightAct / 100, brix = brixAct, harvestDOY = doy) |>
      left_join(irr, by = "year") |>
      mutate(iwue = yield / irrigation, vegetativeWS = g$v[i], reproductiveWS = g$r[i],
             ripeningWS = g$ri[i], transplantingDOY = g$td[i])
  }))
}
res <- if (RERUN) run_design() else readRDS("analysis/data/results_irrigation_optimizer_calibrated.rds")
cat("simulazioni annuali:", nrow(res), "| anni:", length(unique(res$year)), "\n")

## ---- medie su 22 anni per strategia ----
agg <- res |> group_by(vegetativeWS, reproductiveWS, ripeningWS, transplantingDOY) |>
  summarise(yield = mean(yield), brix = mean(brix), irrigation = mean(irrigation),
            events = mean(n_irrigation), iwue = mean(iwue), .groups = "drop")

## ---- fronte di Pareto: 5 obiettivi (max resa, Brix, IWUE; min irrigazione, eventi) ----
O <- cbind(agg$yield, agg$brix, agg$iwue, -agg$irrigation, -agg$events)
agg$pareto <- !sapply(seq_len(nrow(O)), function(i)
  any(apply(O, 1, function(o) all(o >= O[i, ]) && any(o > O[i, ]))))
front <- agg[agg$pareto, ]
cat("\n## FRONTE DI PARETO\nstrategie:", nrow(agg), "| non dominate:", nrow(front), "\n")
cat("per wsLevel vegetativo:   ", paste(names(table(front$vegetativeWS)),   table(front$vegetativeWS),   sep = ":", collapse = "  "), "\n")
cat("per wsLevel riproduttivo: ", paste(names(table(front$reproductiveWS)), table(front$reproductiveWS), sep = ":", collapse = "  "), "\n")
cat("per wsLevel maturazione:  ", paste(names(table(front$ripeningWS)),     table(front$ripeningWS),     sep = ":", collapse = "  "), "\n")
cat("per data di trapianto:    ", paste(names(table(front$transplantingDOY)), table(front$transplantingDOY), sep = ":", collapse = "  "), "\n")
rng <- function(x, d) sprintf(paste0("%.", d, "f-%.", d, "f"), min(x), max(x))
cat(sprintf("intervalli, tutte le 256: resa %s | Brix %s | irrigazione %s | eventi %s | IWUE %s\n",
    rng(agg$yield, 1), rng(agg$brix, 2), rng(agg$irrigation, 0), rng(agg$events, 1), rng(agg$iwue, 3)))
cat(sprintf("intervalli, fronte:       resa %s | Brix %s | irrigazione %s | eventi %s | IWUE %s\n",
    rng(front$yield, 1), rng(front$brix, 2), rng(front$irrigation, 0), rng(front$events, 1), rng(front$iwue, 3)))

## ---- punto ideale e compromesso (normalizzazione min-max sul fronte, pesi uguali) ----
n01 <- function(x) (x - min(x)) / (max(x) - min(x))
Z <- cbind(n01(front$yield), n01(front$brix), n01(front$iwue), 1 - n01(front$irrigation), 1 - n01(front$events))
front$dist <- sqrt(rowSums((1 - Z)^2))
row <- function(d, lab) cat(sprintf("%-28s %.1f/%.1f/%.1f DOY %d | resa %.1f | Brix %.2f | irrig. %.0f mm | eventi %.1f | IWUE %.3f\n",
  lab, d$vegetativeWS, d$reproductiveWS, d$ripeningWS, d$transplantingDOY, d$yield, d$brix, d$irrigation, d$events, d$iwue))
cat("\n## TABLE 3 (strategie scelte sul fronte)\n")
row(front[which.max(front$yield), ],      "Resa massima")
row(front[which.max(front$iwue), ],       "IWUE massima")
row(front[which.min(front$dist), ],       "Compromesso (punto ideale)")
row(front[which.max(front$brix), ],       "Brix massimo")
row(front[which.min(front$irrigation), ], "Acqua minima")
cat("distanza del compromesso dal punto ideale:", round(min(front$dist), 3),
    "| seconda strategia piu' vicina:", round(sort(front$dist)[2], 3), "\n")

## ---- effetti per fase (modello misto, anno come effetto casuale; tutte le 256) ----
d <- res |> mutate(across(c(vegetativeWS, reproductiveWS, ripeningWS, transplantingDOY), factor))
f <- function(y) fixef(lmer(as.formula(paste(y, "~ vegetativeWS + reproductiveWS + ripeningWS + transplantingDOY + (1|year)")), data = d))
fy <- f("yield"); fb <- f("brix"); fi <- f("irrigation")
cat("\n## EFFETTI wsLevel 0.9 rispetto a 0.6\n")
cat(sprintf("resa (Mg/ha):      vegetativa %+.2f | riproduttiva %+.2f | maturazione %+.2f\n", fy["vegetativeWS0.9"], fy["reproductiveWS0.9"], fy["ripeningWS0.9"]))
cat(sprintf("Brix (gradi):      vegetativa %+.3f | riproduttiva %+.3f | maturazione %+.3f\n", fb["vegetativeWS0.9"], fb["reproductiveWS0.9"], fb["ripeningWS0.9"]))
cat(sprintf("irrigazione (mm):  vegetativa %+.1f | riproduttiva %+.1f | maturazione %+.1f\n", fi["vegetativeWS0.9"], fi["reproductiveWS0.9"], fi["ripeningWS0.9"]))
cat(sprintf("trapianto DOY 125 rispetto a 110: resa %+.2f Mg/ha | Brix %+.3f\n", fy["transplantingDOY125"], fb["transplantingDOY125"]))

dir.create("analysis/output", showWarnings = FALSE)
write.csv(agg, "analysis/output/strategie_medie_pareto.csv", row.names = FALSE)
cat("\nsalvato: analysis/output/strategie_medie_pareto.csv\n")
