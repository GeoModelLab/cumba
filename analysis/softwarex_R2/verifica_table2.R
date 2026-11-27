# Verifica indipendente: Table 2 del paper SoftwareX con cumba 0.1.0 (CRAN)
# Da eseguire nella cartella cumba_R_package. Richiede: cumba, dplyr, lubridate, readxl.
library(cumba); library(dplyr); library(lubridate); library(readxl)
data(tomatoFoggia); data(cumbaParameters)
cat("versione cumba:", as.character(packageVersion("cumba")), "\n")

soil <- read_excel("data-raw/Dataset_Carucci_et_al.xlsx", sheet = "Soil")[, c("ID", "FC", "WP")]

# stessa suddivisione degli script (seme 42, 70% per anno)
set.seed(42)
exp_years <- tomatoFoggia$irrigation |> group_by(ID) |> slice_head() |> ungroup() |> select(ID, YEAR)
calib_ids <- exp_years |> group_by(YEAR) |> slice_sample(prop = 0.70) |> ungroup() |> pull(ID)

run_all <- function(per_exp_soil, eff) {
  do.call(rbind, lapply(1:32, function(i) {
    p <- cumbaParameters
    if (per_exp_soil) {
      p$FieldCapacity$value <- soil$FC[soil$ID == i] / 100
      p$WiltingPoint$value  <- soil$WP[soil$ID == i] / 100
    }
    yr <- tomatoFoggia$irrigation$YEAR[tomatoFoggia$irrigation$ID == i][1]
    tail(cumba_experiment(
      tomatoFoggia$weather[year(tomatoFoggia$weather$DATE) == yr, ], p,
      irrigation_df = tomatoFoggia$irrigation[tomatoFoggia$irrigation$ID == i, ],
      irrigationEfficiency = eff), 1)
  })) |>
    mutate(experiment = as.integer(experiment)) |>
    inner_join(tomatoFoggia$production, by = c("experiment" = "ID"))
}

show <- function(h, label) {
  cat("\n##", label, "\n")
  for (s in list(list("calibrazione", calib_ids), list("validazione", setdiff(1:32, calib_ids)))) {
    d <- h[h$experiment %in% s[[2]], ]
    cat(sprintf("%-13s n=%d  resa: r=%.3f RMSE=%.2f bias=%.2f | Brix: r=%.3f RMSE=%.3f\n",
                s[[1]], nrow(d),
                cor(d$yield, d$yield_ref), sqrt(mean((d$yield - d$yield_ref)^2)), mean(d$yield - d$yield_ref),
                cor(d$brix, d$brix_ref), sqrt(mean((d$brix - d$brix_ref)^2))))
  }
}

# A) impostazioni degli script: attesi i valori di Table 2
#    (resa r 0.904 / 0.943, RMSE 12.11 / 12.08, bias -2.50 / -1.23)
show(run_all(TRUE, 1), "A) suolo per esperimento, efficienza 1")

# B) esempio 3.1 come stampato nel paper: suolo di default, efficienza 0.9
show(run_all(FALSE, 0.9), "B) suolo di default, efficienza 0.9 (esempio 3.1)")
