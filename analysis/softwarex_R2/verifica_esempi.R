# Verifica 2/4 - esempi 3.1 e 3.2 del paper SoftwareX, con cumba 0.1.0 (CRAN)
# Eseguire dalla cartella cumba_R_package, in una sessione R nuova, SENZA devtools::load_all().
# Usa solo il pacchetto: nessun file esterno.
library(cumba); library(dplyr)
data(tomatoFoggia); data(cumbaParameters)
cat("versione cumba:", as.character(packageVersion("cumba")), "\n")

## ---- Esempio 3.1 (nuova versione proposta per il paper) --------------------
# capacita' di campo e punto di appassimento misurati (m3 m-3), uno per stagione
soil <- data.frame(
  year = c(2005, 2006, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
  FC   = c(0.320, 0.340, 0.334, 0.384, 0.383, 0.39125, 0.385, 0.380, 0.380),
  WP   = c(0.140, 0.1092, 0.140, 0.110, 0.202, 0.1002, 0.180, 0.110, 0.232))

results <- lapply(soil$year, function(y) {
  p <- cumbaParameters
  p$FieldCapacity$value <- soil$FC[soil$year == y]
  p$WiltingPoint$value  <- soil$WP[soil$year == y]
  cumba_experiment(
    weather              = subset(tomatoFoggia$weather, format(DATE, "%Y") == y),
    param                = p,
    irrigation_df        = subset(tomatoFoggia$irrigation, YEAR == y),
    estimateRad          = TRUE,
    estimateET0          = TRUE,
    irrigationEfficiency = 1)     # volumi osservati applicati per intero
}) |> bind_rows()

harvest <- results |>
  group_by(site, year, experiment) |> slice_tail(n = 1) |> ungroup()
merged <- harvest |> mutate(experiment = as.integer(experiment)) |>
  inner_join(tomatoFoggia$production, by = c("experiment" = "ID"))

# suddivisione calibrazione / validazione (seme 42, 70% per anno)
set.seed(42)
calib_ids <- tomatoFoggia$management |> group_by(YEAR) |>
  slice_sample(prop = 0.70) |> ungroup() |> pull(ID)
cat("ID calibrazione:", sort(calib_ids), "\n")
merged$subset <- ifelse(merged$experiment %in% calib_ids, "calibrazione", "validazione")

dir.create("analysis/output", showWarnings = FALSE)
write.csv(merged[, c("experiment", "year", "yield", "yield_ref", "brix", "brix_ref", "subset")],
          "analysis/output/obs_sim.csv", row.names = FALSE)   # dati dei pannelli B e D di Figure 5

cat("\n## ESEMPIO 3.1 -> deve dare Table 2\n")
print(as.data.frame(merged |> group_by(subset) |> summarise(
  n = n(),
  r_resa = round(cor(yield, yield_ref), 3),
  nRMSE_resa = round(100 * sqrt(mean((yield - yield_ref)^2)) / mean(yield_ref), 1),
  RMSE_resa = round(sqrt(mean((yield - yield_ref)^2)), 2),
  bias_resa = round(mean(yield - yield_ref), 2),
  r_brix = round(cor(brix, brix_ref), 3),
  nRMSE_brix = round(100 * sqrt(mean((brix - brix_ref)^2)) / mean(brix_ref), 1),
  RMSE_brix = round(sqrt(mean((brix - brix_ref)^2)), 3),
  bias_brix = round(mean(brix - brix_ref), 3))))
rf <- merged[merged$experiment %in% tomatoFoggia$management$ID[tomatoFoggia$management$IRR_M == "0%"], ]
cat(sprintf("asciutto (n=%d): resa simulata %s (media %.1f, mediana %.1f) | osservata media %.1f Mg/ha\n",
            nrow(rf), paste(round(rf$yield, 1), collapse = ", "), mean(rf$yield), median(rf$yield), mean(rf$yield_ref)))

## ---- Esempio 3.2 (versione R2) --------------------------------------------
strategies <- list(
  full_irr = list(vegetative   = list(wsLevel = 1.0, turnMin = 4),
                  reproductive = list(wsLevel = 1.0, turnMin = 4),
                  ripening     = list(wsLevel = 1.0, turnMin = 4)),
  DI_30    = list(vegetative   = list(wsLevel = 0.7, turnMin = 4),
                  reproductive = list(wsLevel = 0.7, turnMin = 4),
                  ripening     = list(wsLevel = 0.7, turnMin = 4)),
  RDI      = list(vegetative   = list(wsLevel = 0.7, turnMin = 4),
                  reproductive = list(wsLevel = 1.0, turnMin = 4),
                  ripening     = list(wsLevel = 0.5, turnMin = 4)))
wx <- tomatoFoggia$weather[tomatoFoggia$weather$Site == "Foggia", ]
out <- lapply(names(strategies), function(nm) {
  r <- cumba_scenario(weather = wx, param = cumbaParameters,
                      irrigationStrategy = strategies[[nm]], transplantingDOY = 120,
                      estimateRad = TRUE, estimateET0 = TRUE, fullOut = FALSE)
  h <- r |> group_by(site, year) |> slice_tail(n = 1) |> ungroup()
  data.frame(strategy = nm, yield_tha = round(mean(h$yield), 1), brix = round(mean(h$brix), 2),
             irr_mm_season = round(mean(tapply(r$irrigation, r$year, sum, na.rm = TRUE))))
})
cat("\n## ESEMPIO 3.2\n"); print(do.call(rbind, out))
