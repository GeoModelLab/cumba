# =============================================================================
# CUMBA Tomato Model — Publication Figures (FINAL, dynamic parameter version)
# =============================================================================
# Figures:
#   Fig. A — Temporal dynamics per irrigation strategy (wide panel)
#   Fig. B — Scatter: fresh fruit yield (calib / valid)
#   Fig. C — Scatter: Brix index (calib / valid)
#   Fig. D — SWC dynamics (per experiment + global scatter)
#
# KEY CHANGE vs previous versions:
#   - Parameters are NO LONGER hardcoded by name. Whatever calibration script
#     produced the RDS (GA with 16 parameters, BOBYQA+Nelder-Mead with 7,
#     or any other subset), this script reads names(best_params_named) and
#     assigns ALL of them automatically. Nothing to edit here when the
#     calibrated parameter set changes — only the upstream calibration script
#     needs to change.
#   - Any parameter NOT found in best_params_named is set to its "Fixed
#     parameters" default below (literature / measured values for Capitanata).
#   - FC/WP columns: this version uses the RAW (uncorrected) FC/WP from the
#     Soil sheet, matching the BOBYQA/Nelder-Mead calibration script (doc 9).
#     If your calibration instead used FC_corr/WP_corr, change the two lines
#     marked "FC/WP SOURCE" below accordingly.
#
# Earlier fixes (kept):
#   - GOF labels: two separate geom_label (no "a" in legend)
#   - Trajectories: cycle_pct normalisation (no dropoff)
#   - Brix line starts from first valid value (no line from zero)
#   - facet_wrap(~ IRR_M) — was incorrectly ~ year
#   - group_by(IRR_M) only — year removed from traj_data and harvest_agg
#   - swc masking uses par$CycleLength$value (not hardcoded 1330)
#   - Pearson r + significance stars, no MAE
#   - Units: Mg ha⁻¹
#   - SWC: stacked bars, single shared legend top
# =============================================================================


# =============================================================================
# 0. Packages
# =============================================================================
library(tidyverse)
library(lubridate)
library(readxl)
library(devtools)
library(cumba)
library(hydroGOF)
library(patchwork)
library(purrr)

fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE)

save_fig <- function(plot, filename, width_cm, height_cm) {
  path <- file.path(fig_dir, filename)
  ggsave(path, plot = plot,
         width = width_cm, height = height_cm, units = "cm",
         dpi = 300, device = cairo_pdf)
  message("Saved PDF: ", path)
}

save_png <- function(plot, filename, width_cm, height_cm) {
  path <- file.path(fig_dir, filename)
  ggsave(path, plot = plot,
         width = width_cm, height = height_cm, units = "cm",
         dpi = 600)
  message("Saved PNG: ", path)
}


# =============================================================================
# 1. Shared theme and palette
# =============================================================================
theme_pub <- function(base_size = 13) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      axis.line        = element_line(colour = "grey40", linewidth = 0.35),
      axis.ticks       = element_line(colour = "grey40", linewidth = 0.30),
      axis.text        = element_text(colour = "grey20", size = rel(0.90)),
      axis.title       = element_text(colour = "grey10", size = rel(0.95)),
      strip.background = element_blank(),
      strip.text       = element_text(face = "plain", size = rel(0.85)),
      panel.spacing    = unit(0.7, "lines"),
      legend.position  = "bottom",
      legend.key.width = unit(1.2, "cm"),
      legend.text      = element_text(size = rel(0.85)),
      legend.title     = element_blank(),
      plot.title       = element_text(face = "bold", hjust = 0, size = rel(1.05)),
      plot.subtitle    = element_text(colour = "grey40", hjust = 0, size = rel(0.85)),
      plot.caption     = element_text(colour = "grey50", size = rel(0.72), hjust = 1)
    )
}

COL_CALIB <- "slateblue4"
COL_VALID <- "tomato4"
COL_SIM   <- "slateblue4"
COL_BRIX  <- "tomato3"
COL_SWC   <- "#B8860B"
COL_PREC  <- "#5B8DB8"
COL_IRR   <- "#2CA25F"
COL_OBS   <- "slateblue4"

FILLS <- c(Calibration = COL_CALIB, Validation = COL_VALID)


# =============================================================================
# 2. Data loading
# =============================================================================
swc_obs <- read_excel(
  "C:\\Users\\Administrator\\OneDrive - CREA\\Desktop\\model dev\\cumba\\cumba_R_package\\testFiles\\SoilWaterContent.xlsx"
) |>
  janitor::clean_names() |>
  mutate(
    date = as.Date(paste0(year, "-", month, "-", day)),
    doy  = yday(date)
  )

excel_file <- "C:\\Users\\Administrator\\OneDrive - CREA\\Desktop\\model dev\\cumba\\cumba_R_package\\testFiles\\Dataset_Carucci_et_al.xlsx"
all_sheets    <- lapply(excel_sheets(excel_file), \(s) read_excel(excel_file, sheet = s))
weather       <- all_sheets[[4]]
irrigation    <- all_sheets[[7]]
ids           <- all_sheets[[2]]

soil_file <- "C:\\Users\\Administrator\\OneDrive - CREA\\Desktop\\model dev\\cumba\\cumba_R_package\\testFiles\\Dataset_Carucci_et_al.xlsx"
soil      <- read_excel(soil_file, sheet = "Soil")

weather$Date  <- as.Date(weather$DATE)
weather$Site  <- "Foggia"
weather       <- weather |> rename(Rad = RAD) |> mutate(Lat = 41)
irrigation_df <- irrigation |> left_join(ids)
irrigation_df$Site <- "Foggia"

lastDay <- cumba::tomatoFoggia$weather |>
  mutate(doy = yday(Date), YEAR = year(Date)) |>
  group_by(YEAR) |>
  slice_tail() |>
  left_join(cumba::tomatoFoggia$management) |>
  dplyr::select(ID, YEAR, doy)

yields_all <- cumba::tomatoFoggia$production |> left_join(lastDay)


# =============================================================================
# 3. Model parameters — read DYNAMICALLY from GA/optimisation RDS
# =============================================================================
par <- cumba::cumbaParameters

# ── UPDATE FILENAME AFTER EACH NEW CALIBRATION ─────────────────────────────
ga_rds <- readRDS(file.path(inputDir, "ga_result_06_19_26.rds"))
# ───────────────────────────────────────────────────────────────────────────

if (is.null(ga_rds$best_params_named)) {
  stop("best_params_named not found in RDS — re-run calibration script")
}
best <- ga_rds$best_params_named


# --- Dynamically assign every calibrated parameter found in the RDS --------
# No hardcoded parameter names: works for any calibration script, any subset.
unmatched <- character(0)
for (nm in names(best)) {
  if (nm %in% names(par)) {
    par[[nm]]$value <- best[[nm]]
  } else {
    unmatched <- c(unmatched, nm)
  }
}
if (length(unmatched) > 0) {
  warning(sprintf(
    "Parameter(s) in RDS not found in cumba::cumbaParameters and were skipped: %s",
    paste(unmatched, collapse = ", ")
  ))
}

cat(sprintf("\nCalibrated parameters loaded from RDS (n = %d):\n", length(best)))
for (nm in names(best)) cat(sprintf("  %-36s = %.4f\n", nm, best[[nm]]))

cat(sprintf("\nFixed parameters (not in this calibration run, n = %d):\n",
            length(setdiff(names(par), names(best)))))
for (nm in setdiff(names(par), names(best)))
  cat(sprintf("  %-36s = %s\n", nm, format(par[[nm]]$value)))


# =============================================================================
# 4. Calibration / validation split
# =============================================================================
set.seed(42)
experiments_all <- unique(soil$ID)

exp_years <- irrigation_df |>
  filter(ID %in% experiments_all) |>
  group_by(ID) |> slice_head() |> ungroup() |>
  dplyr::select(ID, YEAR)

calib_ids <- exp_years |>
  group_by(YEAR) |>
  slice_sample(prop = 0.70) |>
  ungroup() |> pull(ID)

valid_ids <- setdiff(experiments_all, calib_ids)


# =============================================================================
# 5. Run simulations
# =============================================================================
devtools::load_all(quiet = TRUE)

results <- vector("list", length(experiments_all))
names(results) <- as.character(experiments_all)

for (iexp in experiments_all) {
  thisExp  <- soil |> filter(ID == iexp)
  thisYear <- irrigation_df |> filter(ID == iexp) |> slice_head() |> pull(YEAR)
  
  cumba_par <- par
  # FC/WP SOURCE: raw (uncorrected) columns, matching BOBYQA/Nelder-Mead script.
  # Switch to thisExp$FC_corr / thisExp$WP_corr if your calibration used those.
  cumba_par$FieldCapacity$value <- thisExp$FC / 100
  cumba_par$WiltingPoint$value  <- thisExp$WP / 100
  
  results[[as.character(iexp)]] <- cumba_experiment(
    weather |> mutate(year = year(Date)) |> filter(year == thisYear),
    cumba_par,
    estimateRad = TRUE, estimateET0 = TRUE,
    irrigationEfficiency = 1,
    irrigation_df |> filter(ID == iexp),
    fullOut = T
  )
}

sim_all <- do.call(rbind, results)

out_all <- sim_all |>
  mutate(
    wc3     = if_else(!is.na(wc3), wc3, 0),
    swc_sim = (wc1mm + wc2mm) / 2
  ) |>
  left_join(swc_obs,    by = c("year", "doy", "experiment" = "id")) |>
  left_join(yields_all, by = c("experiment" = "ID", "doy")) |>
  mutate(
    subset = if_else(experiment %in% calib_ids, "Calibration", "Validation"),
    # Use calibrated CycleLength for SWC masking (not hardcoded 1330)
    swc    = if_else(gddState / par$CycleLength$value <= 0.7, swc, NA_real_)
  )


  


# =============================================================================
# 6. SWC subset
# =============================================================================
swc_exps  <- c(27L, 28L, 29L, 30L, 32L)
years_swc <- c(2017L, 2017L, 2018L, 2018L, 2018L)

outputs_swc <- vector("list", length(swc_exps))
for (i in seq_along(swc_exps)) {
  cumba_par_swc <- par
  thisExp_swc   <- soil |> filter(ID == swc_exps[i])
  cumba_par_swc$FieldCapacity$value    <- thisExp_swc$FC / 100
  cumba_par_swc$WiltingPoint$value     <- thisExp_swc$WP / 100
  cumba_par_swc$SoilWaterInitial$value <- 100
  
  outputs_swc[[i]] <- cumba_experiment(
    weather |> mutate(year = year(Date)) |> filter(year == years_swc[i]),
    cumba_par_swc,
    estimateRad = TRUE, estimateET0 = TRUE,
    irrigation_df |> filter(ID == swc_exps[i]),
    irrigationEfficiency = 1,
    fullOut = TRUE
  )
}

yields_swc <- cumba::tomatoFoggia$production |>
  filter(ID %in% swc_exps) |> left_join(lastDay)

# =============================================================================
# SWC simulato vs sonda TDR
# =============================================================================
# Struttura layer del modello (rootDepthMax ~ 88 cm):
#   Layer 1: 0-3 cm             wc1mm (mm), wc1 (m3/m3)
#   Layer 2: 3-rootState cm     wc2mm (mm), wc2 (m3/m3)  [cresce con le radici]
#   Layer 3: rootState-88 cm    wc3mm (mm), wc3 (m3/m3)  [non radicata]
#
# Con rootState_max ~ 36 cm, la sonda misura anche parte della layer 3.
# AWC%: osservato e simulato normalizzati sulla STESSA scala (range osservato)
# cosi' un offset in swc_sim dovuto a profondita' sbagliata e' visibile.

# TDR sensor depth = 70 cm (documented)
# Layer 3 contribution: rootState–70 cm included proportionally
SENSOR_DEPTH <- 60
out_swc <- do.call(rbind, outputs_swc) |>
  mutate(
    wc3   = if_else(!is.na(wc3),   wc3,   0),
    wc3mm = if_else(!is.na(wc3mm), wc3mm, 0),
    layer3_total  = par$RootDepthMax$value - rootState,
    layer3_sensor = pmax(pmin(SENSOR_DEPTH - rootState, layer3_total), 0),
    wc3mm_sensor  = if_else(layer3_total > 0,
                            wc3mm * layer3_sensor / layer3_total, 0),
    swc_sim       = (wc1mm + wc2mm + wc3mm_sensor) / (SENSOR_DEPTH * 10)
  ) |>
  left_join(swc_obs,    by = c("year", "doy", "experiment" = "id")) |>
  left_join(yields_swc, by = c("experiment" = "ID", "doy")) |>
  # AWC% normalised independently on observed and simulated min/max per experiment.
  # Both scaled on their own range → removes absolute offset, compares dynamics.
  group_by(experiment) |>
  mutate(
    fc_obs  = max(swc / 100, na.rm = TRUE),
    wp_obs  = min(swc / 100, na.rm = TRUE),
    fc_sim  = max(if_else(!is.na(swc), swc_sim, NA_real_), na.rm = TRUE),
    wp_sim  = min(if_else(!is.na(swc), swc_sim, NA_real_), na.rm = TRUE),
    awc_obs = if_else((fc_obs - wp_obs) > 0,
                      (swc / 100 - wp_obs) / (fc_obs - wp_obs) * 100,
                      NA_real_),
    # awc_sim clipped to 0-100: values outside range occur when swc_sim
    # exceeds the observed min/max range (acceptable, shown as boundary)
    awc_sim = if_else((fc_sim - wp_sim) > 0,
                      pmin(pmax(
                        (swc_sim - wp_sim) / (fc_sim - wp_sim) * 100,
                        0), 100),
                      NA_real_)
  ) |>
  ungroup()




# =============================================================================
# 7. GOF helpers
# =============================================================================
gof_stats <- function(obs, sim) {
  keep <- !is.na(obs) & !is.na(sim)
  obs  <- obs[keep]; sim <- sim[keep]
  if (length(obs) < 2) return(list(r2 = NA, rmse = NA, mae = NA, n = length(obs)))
  list(r2   = summary(lm(sim ~ obs))$r.squared,
       rmse = hydroGOF::rmse(sim, obs),
       mae  = mean(abs(sim - obs)),
       n    = length(obs))
}

sig_stars <- function(p) {
  dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                   p < 0.05  ~ "*",   TRUE       ~ "ns")
}

gof_label_r <- function(obs, sim, unit = "") {
  keep <- !is.na(obs) & !is.na(sim)
  obs  <- obs[keep]; sim <- sim[keep]
  ct   <- cor.test(obs, sim, method = "pearson")
  sprintf("r = %.3f%s\nRMSE = %.2f%s",
          ct$estimate, sig_stars(ct$p.value),
          hydroGOF::rmse(sim, obs), unit)
}

harvest_all <- out_all |>
  group_by(experiment) |> slice_tail() |> ungroup() |>
  filter(!is.na(yield_ref), !is.na(brix_ref))

# SWC GOF on AWC%
swc_gof_per_exp <- out_swc |>
  filter(!is.na(awc_obs)) |>
  group_by(experiment) |>
  summarise(label = gof_label_r(awc_obs, awc_sim, unit = " %"),
            .groups = "drop")

lab_swc_global <- gof_label_r(
  out_swc$awc_obs[!is.na(out_swc$awc_obs)],
  out_swc$awc_sim[!is.na(out_swc$awc_obs)],
  unit = " %")

lab_yield_calib_r <- gof_label_r(
  harvest_all$yield_ref[harvest_all$subset == "Calibration"],
  harvest_all$fruitFreshWeightAct[harvest_all$subset == "Calibration"] * 0.01,
  unit = " Mg ha\u207B\u00b9")
lab_yield_valid_r <- gof_label_r(
  harvest_all$yield_ref[harvest_all$subset == "Validation"],
  harvest_all$fruitFreshWeightAct[harvest_all$subset == "Validation"] * 0.01,
  unit = " Mg ha\u207B\u00b9")
lab_brix_calib_r <- gof_label_r(
  harvest_all$brix_ref[harvest_all$subset == "Calibration"],
  harvest_all$brixAct[harvest_all$subset  == "Calibration"],
  unit = " \u00b0Brix")
lab_brix_valid_r <- gof_label_r(
  harvest_all$brix_ref[harvest_all$subset == "Validation"],
  harvest_all$brixAct[harvest_all$subset  == "Validation"],
  unit = " \u00b0Brix")


# =============================================================================
# 8. DIAGNOSTICS
# =============================================================================
nrmse <- function(obs, sim)
  sqrt(mean((sim - obs)^2, na.rm = TRUE)) / mean(obs, na.rm = TRUE)

cat("\n── YIELD CALIBRATION ──\n")
s <- gof_stats(harvest_all$yield_ref[harvest_all$subset == "Calibration"],
               harvest_all$fruitFreshWeightAct[harvest_all$subset == "Calibration"] * 0.01)
cat(sprintf("n=%d  R²=%.3f  RMSE=%.2f  MAE=%.2f Mg/ha\n", s$n, s$r2, s$rmse, s$mae))

cat("\n── YIELD VALIDATION ──\n")
s <- gof_stats(harvest_all$yield_ref[harvest_all$subset == "Validation"],
               harvest_all$fruitFreshWeightAct[harvest_all$subset == "Validation"] * 0.01)
cat(sprintf("n=%d  R²=%.3f  RMSE=%.2f  MAE=%.2f Mg/ha\n", s$n, s$r2, s$rmse, s$mae))

cat("\n── BRIX CALIBRATION ──\n")
s <- gof_stats(harvest_all$brix_ref[harvest_all$subset == "Calibration"],
               harvest_all$brixAct[harvest_all$subset == "Calibration"])
cat(sprintf("n=%d  R²=%.3f  RMSE=%.3f  MAE=%.3f °Brix\n", s$n, s$r2, s$rmse, s$mae))

cat("\n── BRIX VALIDATION ──\n")
s <- gof_stats(harvest_all$brix_ref[harvest_all$subset == "Validation"],
               harvest_all$brixAct[harvest_all$subset == "Validation"])
cat(sprintf("n=%d  R²=%.3f  RMSE=%.3f  MAE=%.3f °Brix\n", s$n, s$r2, s$rmse, s$mae))

cat("\n── SWC GLOBAL (AWC%) ──\n")
s <- gof_stats(out_swc$awc_obs[!is.na(out_swc$awc_obs)],
               out_swc$awc_sim[!is.na(out_swc$awc_obs)])
cat(sprintf("n=%d  R²=%.3f  RMSE=%.2f%%  MAE=%.2f%%\n", s$n, s$r2, s$rmse, s$mae))
cat("SWC in m³/m³:\n")
s2 <- gof_stats(out_swc$swc[!is.na(out_swc$awc_obs)] / 100,
                out_swc$swc_sim[!is.na(out_swc$awc_obs)])
cat(sprintf("  R²=%.3f  RMSE=%.4f  MAE=%.4f m³/m³\n", s2$r2, s2$rmse, s2$mae))

cat("\n── BIAS ──\n")
cat(sprintf("Yield calib: %+.2f Mg/ha\n",
            mean(harvest_all$fruitFreshWeightAct[harvest_all$subset=="Calibration"]*0.01,na.rm=T) -
              mean(harvest_all$yield_ref[harvest_all$subset=="Calibration"],na.rm=T)))
cat(sprintf("Yield valid: %+.2f Mg/ha\n",
            mean(harvest_all$fruitFreshWeightAct[harvest_all$subset=="Validation"]*0.01,na.rm=T) -
              mean(harvest_all$yield_ref[harvest_all$subset=="Validation"],na.rm=T)))
cat(sprintf("Brix  calib: %+.3f °Brix\n",
            mean(harvest_all$brixAct[harvest_all$subset=="Calibration"],na.rm=T) -
              mean(harvest_all$brix_ref[harvest_all$subset=="Calibration"],na.rm=T)))
cat(sprintf("Brix  valid: %+.3f °Brix\n",
            mean(harvest_all$brixAct[harvest_all$subset=="Validation"],na.rm=T) -
              mean(harvest_all$brix_ref[harvest_all$subset=="Validation"],na.rm=T)))

cat("\n── nRMSE ──\n")
cat(sprintf("Yield calib: %.1f%%\n",
            nrmse(harvest_all$yield_ref[harvest_all$subset=="Calibration"],
                  harvest_all$fruitFreshWeightAct[harvest_all$subset=="Calibration"]*0.01)*100))
cat(sprintf("Yield valid: %.1f%%\n",
            nrmse(harvest_all$yield_ref[harvest_all$subset=="Validation"],
                  harvest_all$fruitFreshWeightAct[harvest_all$subset=="Validation"]*0.01)*100))
cat(sprintf("Brix  calib: %.1f%%\n",
            nrmse(harvest_all$brix_ref[harvest_all$subset=="Calibration"],
                  harvest_all$brixAct[harvest_all$subset=="Calibration"])*100))
cat(sprintf("Brix  valid: %.1f%%\n",
            nrmse(harvest_all$brix_ref[harvest_all$subset=="Validation"],
                  harvest_all$brixAct[harvest_all$subset=="Validation"])*100))

cat("\n── RMSE/SD ──\n")
rmse_y <- sqrt(mean((harvest_all$fruitFreshWeightAct[harvest_all$subset=="Calibration"]*0.01 -
                       harvest_all$yield_ref[harvest_all$subset=="Calibration"])^2,na.rm=T))
rmse_b <- sqrt(mean((harvest_all$brixAct[harvest_all$subset=="Calibration"] -
                       harvest_all$brix_ref[harvest_all$subset=="Calibration"])^2,na.rm=T))
cat(sprintf("Yield: %.2f  Brix: %.2f\n",
            rmse_y / mean(harvest_all$yield_ref_sd, na.rm=T),
            rmse_b / mean(harvest_all$brix_ref_sd,  na.rm=T)))

cat("\n── Pearson r ──\n")
r_y <- cor(harvest_all$fruitFreshWeightAct[harvest_all$subset=="Calibration"]*0.01,
           harvest_all$yield_ref[harvest_all$subset=="Calibration"], use="complete.obs")
r_b <- cor(harvest_all$brixAct[harvest_all$subset=="Calibration"],
           harvest_all$brix_ref[harvest_all$subset=="Calibration"], use="complete.obs")
cat(sprintf("r_yield=%.3f  r_brix=%.3f\n", r_y, r_b))

r_y_v <- cor(harvest_all$fruitFreshWeightAct[harvest_all$subset=="Validation"]*0.01,
             harvest_all$yield_ref[harvest_all$subset=="Validation"], use="complete.obs")
r_b_v <- cor(harvest_all$brixAct[harvest_all$subset=="Validation"],
             harvest_all$brix_ref[harvest_all$subset=="Validation"], use="complete.obs")
cat(sprintf("r_yield_valid=%.3f  r_brix_valid=%.3f\n", r_y_v, r_b_v))

nrmse_y_c <- nrmse(harvest_all$yield_ref[harvest_all$subset=="Calibration"],
                   harvest_all$fruitFreshWeightAct[harvest_all$subset=="Calibration"]*0.01)
nrmse_b_c <- nrmse(harvest_all$brix_ref[harvest_all$subset=="Calibration"],
                   harvest_all$brixAct[harvest_all$subset=="Calibration"])
obj_fun <- ((nrmse_y_c*0.2 + (1-r_y)*0.8) + (nrmse_b_c*0.2 + (1-r_b)*0.8)) / 2
cat(sprintf("obj_fun = %.4f\n", obj_fun))


# =============================================================================
# 9. Fig. B — Scatter: Fresh fruit yield
# =============================================================================
yield_data <- harvest_all |>
  filter(!is.na(yield_ref)) |>
  mutate(sim_yield = fruitFreshWeightAct * 0.01)

yield_ann <- tibble(
  subset = c("Calibration", "Validation"),
  label  = c(lab_yield_calib_r, lab_yield_valid_r),
  vjust  = c(1.15, 2.5)
)

rng_yield <- range(c(yield_data$yield_ref - yield_data$yield_ref_sd,
                     yield_data$yield_ref + yield_data$yield_ref_sd,
                     yield_data$sim_yield),
                   na.rm = TRUE)

fig_yield <- ggplot(yield_data,
                    aes(x = yield_ref, y = sim_yield, colour = subset)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_smooth(aes(group = subset, fill = subset),
              method = "lm", formula = y ~ x, se = FALSE,
              alpha = 0.12, linewidth = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(xmin = yield_ref - yield_ref_sd,
                    xmax = yield_ref + yield_ref_sd),
                orientation = "y", height = 0, linewidth = 0.35) +
  geom_point(shape = 21, fill = NA, size = 2.8, stroke = 0.8) +
  geom_label(data = yield_ann |> filter(subset == "Calibration"),
             aes(x = -Inf, y = Inf, label = label, vjust = vjust),
             hjust = -0.05, fill = "white", colour = COL_CALIB,
             label.size = 0.2, label.padding = unit(0.25, "lines"),
             size = 4.0, family = "mono", inherit.aes = FALSE) +
  geom_label(data = yield_ann |> filter(subset == "Validation"),
             aes(x = -Inf, y = Inf, label = label, vjust = vjust),
             hjust = -0.05, fill = "white", colour = COL_VALID,
             label.size = 0.2, label.padding = unit(0.25, "lines"),
             size = 4.0, family = "mono", inherit.aes = FALSE) +
  scale_colour_manual(values = FILLS) +
  scale_fill_manual(values   = FILLS) +
  scale_x_continuous(limits = rng_yield) +
  scale_y_continuous(limits = rng_yield) +
  labs(tag = "B",
       x   = "Observed (Mg ha\u207B\u00b9)",
       y   = "Simulated (Mg ha\u207B\u00b9)") +
  theme_pub() +
  theme(legend.position = c(0.82, 0.15))


# =============================================================================
# 10. Fig. C — Scatter: Brix index
# =============================================================================
brix_data <- harvest_all |> filter(!is.na(brix_ref))

brix_ann <- tibble(
  subset = c("Calibration", "Validation"),
  label  = c(lab_brix_calib_r, lab_brix_valid_r),
  vjust  = c(1.15, 2.5)
)
rng <- range(c(brix_data$brix_ref - brix_data$brix_ref_sd,
               brix_data$brix_ref + brix_data$brix_ref_sd,
               brix_data$brixAct),
             na.rm = TRUE)

fig_brix <- ggplot(brix_data,
                   aes(x = brix_ref, y = brixAct, colour = subset)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_smooth(aes(group = subset, fill = subset),
              method = "lm", formula = y ~ x, se = FALSE,
              alpha = 0.12, linewidth = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(xmin = brix_ref - brix_ref_sd,
                    xmax = brix_ref + brix_ref_sd),
                orientation = "y", height = 0, linewidth = 0.35) +
  geom_point(shape = 21, fill = NA, size = 2.8, stroke = 0.8) +
  geom_label(data = brix_ann |> filter(subset == "Calibration"),
             aes(x = -Inf, y = Inf, label = label, vjust = vjust),
             hjust = -0.05, fill = "white", colour = COL_CALIB,
             label.size = 0.2, label.padding = unit(0.25, "lines"),
             size = 4.0, family = "mono", inherit.aes = FALSE) +
  geom_label(data = brix_ann |> filter(subset == "Validation"),
             aes(x = -Inf, y = Inf, label = label, vjust = vjust),
             hjust = -0.05, fill = "white", colour = COL_VALID,
             label.size = 0.2, label.padding = unit(0.25, "lines"),
             size = 4.0, family = "mono", inherit.aes = FALSE) +
  scale_colour_manual(values = FILLS) +
  scale_fill_manual(values   = FILLS) +
  scale_x_continuous(limits = rng) +
  scale_y_continuous(limits = rng) +
  labs(tag = "C",
       x   = "Observed (\u00b0Brix)",
       y   = "Simulated (\u00b0Brix)") +
  theme_pub() +
  theme(legend.position = c(0.82, 0.15))
fig_brix

# =============================================================================
# 11. Fig. A — Temporal dynamics per irrigation strategy
# =============================================================================
K_YIELD <- 1
K_BRIX  <- 20

strategy_labels <- c(
  "0%"       = "rainfed",
  "50%"      = "50%",
  "75%"      = "75%",
  "100%"     = "100%",
  "507550%"  = "50-75-50%",
  "5010050%" = "50-100-50%",
  "7510075%" = "75-100-75%",
  "7550%"    = "75-50%"
)

strategy_order <- c(
  "rainfed", "50%", "50-75-50%", "50-100-50%",
  "75-50%",  "75%", "75-100-75%", "100%"
)

irr_strategy <- irrigation_df |>
  group_by(ID) |> slice_head(n = 1) |> ungroup() |>
  select(ID, IRR_M) |> mutate(ID = as.numeric(ID))

out_all_dyn <- out_all |>
  select(-any_of("IRR_M")) |>
  mutate(experiment = as.numeric(experiment)) |>
  left_join(irr_strategy, by = c("experiment" = "ID")) |>
  mutate(IRR_M = recode(IRR_M, !!!strategy_labels),
         IRR_M = factor(IRR_M, levels = strategy_order))

stopifnot("IRR_M join failed" = !all(is.na(out_all_dyn$IRR_M)))

harvest_all_dyn <- harvest_all |>
  select(-any_of("IRR_M")) |>
  mutate(experiment = as.numeric(experiment)) |>
  left_join(irr_strategy, by = c("experiment" = "ID")) |>
  mutate(IRR_M = recode(IRR_M, !!!strategy_labels),
         IRR_M = factor(IRR_M, levels = strategy_order))

stopifnot("IRR_M join failed (harvest)" = !all(is.na(harvest_all_dyn$IRR_M)))

out_all_dyn_norm <- out_all_dyn |>
  filter(cycleCompletion < 100) |>
  group_by(experiment) |>
  mutate(
    doy_start = min(doy),
    doy_end   = max(doy),
    cycle_pct = if_else(doy_end > doy_start,
                        (doy - doy_start) / (doy_end - doy_start) * 100, 0)
  ) |>
  ungroup()

interp_exp <- function(df) {
  grid <- seq(0, 100, by = 1)
  first_valid_pct <- df$cycle_pct[which(df$brixAct > 0)[1]]
  if (is.na(first_valid_pct)) first_valid_pct <- 0
  yield_interp <- approx(df$cycle_pct, df$fruitFreshWeightAct * 0.01,
                         xout = grid, rule = 2)$y
  brix_interp  <- approx(df$cycle_pct, df$brixAct,
                         xout = grid, rule = 2)$y
  brix_interp[grid < first_valid_pct] <- NA
  tibble(cycle_pct    = grid,
         yield_interp = yield_interp,
         brix_interp  = brix_interp,
         IRR_M        = df$IRR_M[1])
}

traj_norm <- out_all_dyn_norm |>
  group_by(experiment) |>
  group_modify(~ interp_exp(.x)) |>
  ungroup() |>
  mutate(IRR_M = factor(IRR_M, levels = strategy_order))

traj_data <- traj_norm |>
  group_by(IRR_M, cycle_pct) |>
  summarise(
    y_p10 = quantile(yield_interp * K_YIELD, 0.10, na.rm = TRUE),
    y_p50 = quantile(yield_interp * K_YIELD, 0.50, na.rm = TRUE),
    y_p90 = quantile(yield_interp * K_YIELD, 0.90, na.rm = TRUE),
    b_p10 = quantile(brix_interp  * K_BRIX,  0.10, na.rm = TRUE),
    b_p50 = quantile(brix_interp  * K_BRIX,  0.50, na.rm = TRUE),
    b_p90 = quantile(brix_interp  * K_BRIX,  0.90, na.rm = TRUE),
    .groups = "drop"
  )

harvest_agg <- harvest_all_dyn |>
  filter(!is.na(yield_ref), !is.na(brix_ref)) |>
  group_by(IRR_M) |>
  summarise(
    yield_mean = mean(yield_ref, na.rm = TRUE),
    yield_sd   = sqrt(var(yield_ref, na.rm = TRUE) +
                        mean(yield_ref_sd^2, na.rm = TRUE)),
    brix_mean  = mean(brix_ref,  na.rm = TRUE),
    brix_sd    = sqrt(var(brix_ref, na.rm = TRUE) +
                        mean(brix_ref_sd^2,  na.rm = TRUE)),
    .groups    = "drop"
  ) |>
  mutate(cycle_pct = 100)

cat("\n── harvest_agg ──\n")
print(harvest_agg |> select(IRR_M, yield_mean, brix_mean))

brix_breaks <- seq(4, 8, by = 1) * K_BRIX
brix_labels <- brix_breaks / K_BRIX

fig_dynamics <- ggplot(traj_data, aes(x = cycle_pct)) +
  geom_ribbon(aes(ymin = y_p10, ymax = y_p90),
              fill = COL_SIM, alpha = 0.15) +
  geom_line(aes(y = y_p50,
                colour = "Simulated yield (Mg ha\u207B\u00b9)"),
            linewidth = 0.9) +
  geom_ribbon(aes(ymin = b_p10, ymax = b_p90),
              fill = COL_BRIX, alpha = 0.15) +
  geom_line(aes(y = b_p50,
                colour = "Simulated Brix (\u00b0)"),
            linewidth = 0.9) +
  geom_errorbar(
    data = harvest_agg,
    aes(x = cycle_pct,
        ymin = (yield_mean - yield_sd) * K_YIELD,
        ymax = (yield_mean + yield_sd) * K_YIELD),
    width = 1.5, linewidth = 0.4, colour = COL_SIM,
    inherit.aes = FALSE) +
  geom_point(
    data = harvest_agg,
    aes(x = cycle_pct, y = yield_mean * K_YIELD),
    shape = 21, fill = NA, colour = COL_SIM,
    size = 3.2, stroke = 0.8, inherit.aes = FALSE) +
  geom_errorbar(
    data = harvest_agg,
    aes(x = cycle_pct,
        ymin = (brix_mean - brix_sd) * K_BRIX,
        ymax = (brix_mean + brix_sd) * K_BRIX),
    width = 1.5, linewidth = 0.4, colour = COL_BRIX,
    inherit.aes = FALSE) +
  geom_point(
    data = harvest_agg,
    aes(x = cycle_pct, y = brix_mean * K_BRIX),
    shape = 23, fill = NA, colour = COL_BRIX,
    size = 3.2, stroke = 0.8, inherit.aes = FALSE) +
  scale_colour_manual(
    name   = NULL,
    values = c("Simulated yield (Mg ha\u207B\u00b9)" = COL_SIM,
               "Simulated Brix (\u00b0)"             = COL_BRIX)) +
  scale_y_continuous(
    name     = "Brix (\u00b0)",
    breaks   = brix_breaks,
    labels   = brix_labels,
    sec.axis = sec_axis(~ . / K_YIELD,
                        name   = "Fresh fruit yield (Mg ha\u207B\u00b9)",
                        breaks = seq(0, 160, by = 20))) +
  scale_x_continuous(
    name   = "Cycle completion (%)",
    limits = c(0, 101),
    breaks = seq(0, 100, by = 25),
    labels = c("0", "25", "50", "75", "100")) +
  guides(colour = guide_legend(order = 1,
                               override.aes = list(linewidth = 1.2))) +
  labs(tag = "A") +
  facet_wrap(~ IRR_M, ncol = 8) +
  theme_pub() +
  theme(legend.position = "top", legend.box = "horizontal")


# =============================================================================
# 12. Fig. D — SWC dynamics
# =============================================================================
# K_WATER scales water inputs (mm) to AWC% axis (0-100)
# 1 mm water shown as 1% AWC — purely visual scaling
K_WATER <- 1.0

swc_meta <- data.frame(exp_id = swc_exps, year = years_swc) |>
  left_join(
    irrigation_df |> group_by(ID) |> slice_head(n = 1) |> ungroup() |>
      select(ID, IRR_M) |> mutate(ID = as.numeric(ID)),
    by = c("exp_id" = "ID")
  ) |>
  mutate(IRR_M       = recode(IRR_M, !!!strategy_labels),
         strip_label = paste0(year, " \u00b7 ", IRR_M)) |>
  select(exp_id, strip_label)

make_swc_panel <- function(exp_id, gof_df) {
  df    <- out_swc |> filter(experiment == exp_id)
  label <- swc_meta$strip_label[swc_meta$exp_id == exp_id]
  df    <- df |> mutate(irr_stack = (p + irrigation) * K_WATER,
                        p_stack   = p * K_WATER)
  ggplot(df, aes(x = doy)) +
    geom_col(aes(y = irr_stack, fill = "Irrigation (mm)"),
             alpha = 0.6, width = 0.8) +
    geom_col(aes(y = p_stack, fill = "Precipitation (mm)"),
             alpha = 0.6, width = 0.8) +
    geom_point(aes(y = awc_obs, colour = "Observed"),
               shape = 21, fill = "white",
               size = 1.8, stroke = 0.6, na.rm = TRUE) +
    geom_line(aes(y = awc_sim, colour = "Simulated"),
              linewidth = 0.85) +
    scale_fill_manual(name   = NULL,
                      values = c("Precipitation (mm)" = COL_PREC,
                                 "Irrigation (mm)"    = COL_IRR)) +
    scale_colour_manual(name   = NULL,
                        values = c(Simulated = COL_OBS, Observed = COL_OBS)) +
    scale_y_continuous(
      name     = "AWC (%)",
      sec.axis = sec_axis(~ . / K_WATER,
                          name = "Water input (mm)")) +
    scale_x_continuous(limits = c(120, 230)) +
    labs(x = "Day of year (DOY)", title = label) +
    theme_pub(base_size = 10) +
    theme(legend.position = "bottom", legend.box = "horizontal",
          legend.key.size = unit(0.35, "cm"),
          legend.text     = element_text(size = 10),
          plot.title      = element_text(size = 11, face = "plain",
                                         hjust = 0.5))
}

swc_panels <- lapply(sort(unique(out_swc$experiment)), make_swc_panel,
                     gof_df = swc_gof_per_exp)

panel_scatter <- ggplot(
  out_swc |> filter(!is.na(awc_obs)),
  aes(x = awc_obs, y = awc_sim)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x,
              colour = COL_SWC, fill = COL_SWC,
              alpha = 0.15, linewidth = 0.7) +
  geom_point(aes(fill = factor(experiment)),
             shape = 21, colour = "white",
             size = 2.0, stroke = 0.4, show.legend = FALSE) +
  annotate("label",
           x = -Inf, y = Inf, label = lab_swc_global,
           hjust = -0.05, vjust = 1.15, size = 3.5,
           colour = "grey15", family = "mono", fill = "white",
           label.size = 0.2, label.padding = unit(0.25, "lines")) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(x = "Observed AWC (%)",
       y = "Simulated AWC (%)") +
  theme_pub(base_size = 10)

fig_swc <- wrap_plots(c(swc_panels, list(panel_scatter)), ncol = 2,
                      guides = "collect") +
  plot_annotation(theme = theme_pub()) &
  theme(legend.position     = "top",
        legend.box          = "horizontal",
        legend.key.size     = unit(0.4, "cm"),
        legend.text         = element_text(size = 11),
        legend.justification = "center")



# =============================================================================
# 13. Assemble: A / (B | C)
# =============================================================================
fig_full <- fig_dynamics / (fig_yield | fig_brix) +
  plot_layout(heights = c(0.6, 1)) 


# =============================================================================
# 14. Export
# =============================================================================
save_fig(fig_dynamics, "figA_temporal_dynamics.pdf", 36, 10)
save_fig(fig_yield,    "figB_scatter_yield.pdf",       9,  9)
save_fig(fig_brix,     "figC_scatter_brix.pdf",        9,  9)
save_fig(fig_full,     "fig_calibration_full.pdf",    30, 22)
save_fig(fig_swc,      "figD_swc_dynamics.pdf",       26, 18)

save_png(fig_dynamics, "figA_temporal_dynamics.png", 36, 10)
save_png(fig_yield,    "figB_scatter_yield.png",       9,  9)
save_png(fig_brix,     "figC_scatter_brix.png",        9,  9)
save_png(fig_full,     "fig_calibration_full.png",    28, 17)
save_png(fig_swc,      "figD_swc_dynamics.png",       26, 18)

message("\nAll figures written to: ", normalizePath(fig_dir))
