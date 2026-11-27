# Verifica 3/4 - umidita' del suolo (riga SWC di Table 2), con cumba 0.1.0 (CRAN)
# Eseguire dalla cartella cumba_R_package. Richiede: cumba, dplyr, lubridate, readxl.
# Stessa logica di analysis/original_scripts/cumba_figures_paper.R (sezione 6).
library(cumba); library(dplyr); library(lubridate); library(readxl)
data(tomatoFoggia); data(cumbaParameters)
cat("versione cumba:", as.character(packageVersion("cumba")), "\n")

swc_obs <- read_excel("analysis/data/SoilWaterContent.xlsx") |>
  mutate(doy = yday(as.Date(paste(YEAR, MONTH, DAY, sep = "-")))) |>
  select(experiment = ID, year = YEAR, doy, swc = SWC)
soil <- read_excel("data-raw/Dataset_Carucci_et_al.xlsx", sheet = "Soil")[, c("ID", "FC", "WP")]

swc_exps <- c(27, 28, 29, 30, 32); years_swc <- c(2017, 2017, 2018, 2018, 2018)
SENSOR_DEPTH <- 60    # cm, come nello script delle figure
sim <- do.call(rbind, lapply(seq_along(swc_exps), function(i) {
  p <- cumbaParameters
  p$FieldCapacity$value    <- soil$FC[soil$ID == swc_exps[i]] / 100
  p$WiltingPoint$value     <- soil$WP[soil$ID == swc_exps[i]] / 100
  p$SoilWaterInitial$value <- 100
  cumba_experiment(tomatoFoggia$weather[year(tomatoFoggia$weather$DATE) == years_swc[i], ], p,
    irrigation_df = tomatoFoggia$irrigation[tomatoFoggia$irrigation$ID == swc_exps[i], ],
    irrigationEfficiency = 1, fullOut = TRUE)
}))
rdm <- cumbaParameters$RootDepthMax$value
d <- sim |> mutate(
    layer3_total  = rdm - rootState,
    layer3_sensor = pmax(pmin(SENSOR_DEPTH - rootState, layer3_total), 0),
    wc3mm_sensor  = ifelse(layer3_total > 0, wc3mm * layer3_sensor / layer3_total, 0),
    swc_sim       = (wc1mm + wc2mm + wc3mm_sensor) / (SENSOR_DEPTH * 10)) |>
  inner_join(swc_obs, by = c("experiment", "year", "doy")) |>
  filter(!is.na(swc)) |> mutate(obs = swc / 100)

cat(sprintf("\n## SWC in m3 m-3 (5 esperimenti)\nn = %d | r = %.3f | R2 = %.3f | RMSE = %.4f | nRMSE = %.1f%% della media osservata | bias = %.4f\n",
  nrow(d), cor(d$swc_sim, d$obs), cor(d$swc_sim, d$obs)^2, sqrt(mean((d$swc_sim - d$obs)^2)),
  100 * sqrt(mean((d$swc_sim - d$obs)^2)) / mean(d$obs), mean(d$swc_sim - d$obs)))

# valore 17.1% della versione R1: osservato e simulato riscalati ciascuno fra il proprio
# minimo e massimo, per esperimento (0-100)
a <- d |> group_by(experiment) |> mutate(
    awc_obs = (obs - min(obs)) / (max(obs) - min(obs)) * 100,
    awc_sim = pmin(pmax((swc_sim - min(swc_sim)) / (max(swc_sim) - min(swc_sim)) * 100, 0), 100)) |> ungroup()
cat(sprintf("scala riscalata 0-100: r = %.3f | R2 = %.3f | RMSE = %.1f%%\n",
            cor(a$awc_sim, a$awc_obs), cor(a$awc_sim, a$awc_obs)^2, sqrt(mean((a$awc_sim - a$awc_obs)^2))))
cat("n per esperimento:\n"); print(table(d$experiment))
