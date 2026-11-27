# =============================================================================
# CUMBA — Full parameter optimisation via BOBYQA + Nelder-Mead
# =============================================================================
# Strategy: optimise ALL parameters (except Tbase, Tmax fixed constraints)
#   then screen which ones move far from default → keep those free
#   those near default → fix at default for parsimony
#
# Fixed hard constraints:
#   Tbase = 10  (literature)
#   Tmax  = 33  (physiological ceiling, must be > Topt)
#
# All other parameters free within package min/max bounds
# Starting point: package defaults (cumbaParameters$x$value)
# =============================================================================

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(devtools)
library(nloptr)
library(parallel)
devtools::load_all(quiet = TRUE)

inputDir <- "testFiles"

# =============================================================================
# 1. Load data
# =============================================================================
excel_file <- "C:\\Users\\Administrator\\OneDrive - CREA\\Desktop\\model dev\\cumba\\cumba_R_package\\testFiles\\Dataset_Carucci_et_al.xlsx"

all_sheets    <- lapply(excel_sheets(excel_file), \(s) read_excel(excel_file, sheet = s))
weather       <- all_sheets[[4]]
irrigation    <- all_sheets[[7]]
ids           <- all_sheets[[2]]
soil          <- read_excel(excel_file, sheet = "Soil")

weather$Date  <- as.Date(weather$DATE)
weather$Site  <- "Foggia"
weather       <- weather |> rename(Rad = RAD) |> mutate(Lat = 41)
irrigation_df <- irrigation |> left_join(ids)
irrigation_df$Site <- "Foggia"

lastDay <- cumba::tomatoFoggia$weather |>
  mutate(doy = yday(Date), YEAR = year(Date)) |>
  group_by(YEAR) |> slice_tail() |>
  left_join(cumba::tomatoFoggia$management) |>
  dplyr::select(ID, YEAR, doy)

yields_all <- cumba::tomatoFoggia$production |> left_join(lastDay)

# =============================================================================
# 2. Calibration / validation split (set.seed = 42)
# =============================================================================
set.seed(42)
experiments_all <- unique(soil$ID)

exp_years <- irrigation_df |>
  filter(ID %in% experiments_all) |>
  group_by(ID) |> slice_head() |> ungroup() |>
  dplyr::select(ID, YEAR)

calib_ids <- exp_years |>
  group_by(YEAR) |> slice_sample(prop = 0.70) |>
  ungroup() |> pull(ID)

valid_ids <- setdiff(experiments_all, calib_ids)

cat(sprintf("\nCalibration: n=%d  IDs: %s\n",
            length(calib_ids), paste(sort(calib_ids), collapse = ", ")))
cat(sprintf("Validation:  n=%d  IDs: %s\n",
            length(valid_ids), paste(sort(valid_ids), collapse = ", ")))

# =============================================================================
# 3. Base parameters — only hard constraints fixed
# =============================================================================
cumba_par_base <- cumba::cumbaParameters

# =============================================================================
# 4. All free parameters — names, bounds, starting values from package defaults
# =============================================================================
# Parameters to optimise (all except Tbase and Tmax)
par_names <- c(
  "Theat",                       # °C         default: 40   min: 34  max: 40
  "RUE",                         # g MJ-1     default: 3.5  min: 2.5 max: 3.8
  "k0",                          # -          default: 4    min: 3   max: 5
  "FruitWaterContentDecreaseMax" # %          default: 0.0025 min: 0.001 max: 0.004
  )

# Extract bounds and defaults directly from cumbaParameters
lower_par <- sapply(par_names, function(nm) cumba_par_base[[nm]]$min)
upper_par <- sapply(par_names, function(nm) cumba_par_base[[nm]]$max)
start_par <- sapply(par_names, function(nm) cumba_par_base[[nm]]$value)



cat(sprintf("\nOptimising %d parameters\n", length(par_names)))
cat("Parameter         | Default  | Start    | Min      | Max\n")
cat(paste(rep("-", 65), collapse = ""), "\n")
for (i in seq_along(par_names))
  cat(sprintf("%-26s| %-9.4f| %-9.4f| %-9.4f| %.4f\n",
              par_names[i],
              cumba_par_base[[par_names[i]]]$value,
              start_par[i], lower_par[i], upper_par[i]))

# =============================================================================
# 5. Objective function
# =============================================================================
iter_count <- 0L
best_so_far <- 9999

loss_function <- function(params) {
  
  if (any(params < lower_par) || any(params > upper_par)) return(9999)
  
  # Topt must be < Tmax (33) — enforce constraint
  
  cumba_par <- cumba_par_base
  for (i in seq_along(par_names))
    cumba_par[[par_names[i]]]$value <- params[i]
  
  results <- parLapply(cl, calib_ids, function(iexp) {
    thisExp  <- soil |> filter(ID == iexp)
    thisYear <- irrigation_df |> filter(ID == iexp) |> slice_head() |> pull(YEAR)
    par_exp  <- cumba_par
    par_exp$FieldCapacity$value <- thisExp$FC / 100
    par_exp$WiltingPoint$value  <- thisExp$WP / 100
    tryCatch(
      cumba_experiment(
        weather |> mutate(year = year(Date)) |> filter(year == thisYear),
        par_exp,
        estimateRad = TRUE, estimateET0 = TRUE,
        irrigationEfficiency = 1,
        irrigation_df |> filter(ID == iexp),
        fullOut = TRUE
      ),
      error = function(e) NULL
    )
  })
  names(results) <- as.character(calib_ids)
  
  results <- Filter(Negate(is.null), results)
  if (length(results) < 5) return(9999)
  
  # Penalty: cycle < 100 days
  cycle_days <- do.call(rbind, results) |>
    filter(cycleCompletion > 0) |>
    group_by(experiment) |>
    summarise(n_days = n(), .groups = "drop")
  if (nrow(cycle_days) == 0 || any(cycle_days$n_days < 100)) return(9999)
  
  out_calib <- do.call(rbind, results) |>
    left_join(yields_all, by = c("experiment" = "ID", "doy")) |>
    group_by(experiment) |> slice_tail(n = 1) |> ungroup() |>
    filter(!is.na(yield_ref), !is.na(brix_ref))
  
  if (nrow(out_calib) < 5) return(9999)
  
  yield_sim <- out_calib$fruitFreshWeightAct * 0.01
  yield_obs <- out_calib$yield_ref
  brix_sim  <- out_calib$brixAct
  brix_obs  <- out_calib$brix_ref
  
  nrmse_yield <- sqrt(mean((yield_sim - yield_obs)^2)) / mean(yield_obs)
  nrmse_brix  <- sqrt(mean((brix_sim  - brix_obs)^2))  / mean(brix_obs)
  r_yield     <- cor(yield_sim, yield_obs, method = "pearson")
  r_brix      <- cor(brix_sim,  brix_obs,  method = "pearson")
  
  if (any(is.na(c(r_yield, r_brix)))) return(9999)
  
  obj_yield <- nrmse_yield * 0.2 + (1 - r_yield) * 0.8
  obj_brix  <- nrmse_brix  * 0.2 + (1 - r_brix)  * 0.8
  obj_fun   <- (obj_yield + obj_brix) / 2
  
  iter_count  <<- iter_count + 1L
  flag <- if (obj_fun < best_so_far) { best_so_far <<- obj_fun; " ***" } else ""
  cat(sprintf(
    "[%4d] r: y=%.3f b=%.3f | nRMSE: y=%.3f b=%.3f | obj=%.4f%s\n",
    iter_count, r_yield, r_brix, nrmse_yield, nrmse_brix, obj_fun, flag
  ))
  
  obj_fun
}

# =============================================================================
# 6. Optimisation — Phase 1 BOBYQA, Phase 2 Nelder-Mead, Phase 3 multi-start
# =============================================================================

n_cores <- max(1L, detectCores() - 1L)
cat(sprintf("Cores: %d\n", n_cores))

# Setup PSOCK cluster (Windows-compatible)
cl <- makeCluster(n_cores, type = "PSOCK")

# Export all objects needed inside parLapply workers
clusterExport(cl, varlist = c(
  "soil", "irrigation_df", "weather", "yields_all",
  "calib_ids", "lower_par", "upper_par"
))
clusterEvalQ(cl, {
  library(tidyverse)
  library(lubridate)
  library(cumba)
  library(nloptr)
})

# Phase 1: BOBYQA
cat("\n=== Phase 1: BOBYQA ===\n")
res1 <- bobyqa(
  x0      = start_par,
  fn      = loss_function,
  lower   = lower_par,
  upper   = upper_par,
  control = list(maxeval = 10000, ftol_rel = 1e-8)
)
cat(sprintf("\nBOBYQA: obj=%.4f  (target: 0.1822)\n", res1$value))

# Phase 2: Nelder-Mead from BOBYQA solution
cat("\n=== Phase 2: Nelder-Mead ===\n")
iter_count <- 0L
res2 <- neldermead(
  x0      = res1$par,
  fn      = loss_function,
  lower   = lower_par,
  upper   = upper_par,
  control = list(maxeval = 5000, ftol_rel = 1e-8, xtol_rel = 1e-8)
)
cat(sprintf("\nNelder-Mead: obj=%.4f\n", res2$value))

# Phase 3: multi-start perturbations
cat("\n=== Phase 3: Multi-start (15 perturbations) ===\n")
best_val <- min(res1$value, res2$value)
best_par <- if (res1$value < res2$value) res1$par else res2$par

set.seed(123)
starts <- lapply(1:10, function(k) {
  p <- best_par + runif(length(best_par), -0.1, 0.1) * (upper_par - lower_par)
  pmax(pmin(p, upper_par), lower_par)
})

# Multi-start: run sequentially (simplex is fast enough per start)
for (k in seq_along(starts)) {
  iter_count <- 0L
  res_k <- bobyqa(
    x0      = starts[[k]],
    fn      = loss_function,
    lower   = lower_par,
    upper   = upper_par,
    control = list(maxeval = 5000, ftol_rel = 1e-8)
  )
  cat(sprintf("  Start %2d: obj=%.4f%s\n", k, res_k$value,
              if (res_k$value < best_val) " ***NEW BEST***" else ""))
  if (res_k$value < best_val) {
    best_val <- res_k$value
    best_par <- res_k$par
  }
}

stopCluster(cl)
cat("Cluster stopped.\n")

# =============================================================================
# 7. Results — sensitivity screening
# =============================================================================
names(best_par) <- par_names

cat("\n=== Best parameters (vs default) ===\n")
cat(sprintf("%-28s  %9s  %9s  %9s  %8s\n",
            "Parameter", "Default", "Best", "Change%", "Range%"))
cat(paste(rep("-", 75), collapse=""), "\n")

defaults <- sapply(par_names, function(nm) cumba_par_base[[nm]]$value)
ranges   <- upper_par - lower_par

sensitivity_tbl <- tibble(
  parameter = par_names,
  default   = defaults,
  best      = best_par,
  change_pct = (best_par - defaults) / abs(defaults) * 100,
  range_pct  = (best_par - lower_par) / ranges * 100
) |> arrange(desc(abs(change_pct)))

for (i in seq_len(nrow(sensitivity_tbl))) {
  r <- sensitivity_tbl[i, ]
  cat(sprintf("%-28s  %9.4f  %9.4f  %+8.1f%%  %7.1f%%\n",
              r$parameter, r$default, r$best, r$change_pct, r$range_pct))
}

cat(sprintf("\nBest obj_fun: %.4f  (target: 0.1822)\n", best_val))

# Parameters that moved < 5% from default → candidates for fixing
insensitive <- sensitivity_tbl |> filter(abs(change_pct) < 5)
sensitive   <- sensitivity_tbl |> filter(abs(change_pct) >= 5)
cat(sprintf("\nSensitive   (|change| >= 5%%): %s\n",
            paste(sensitive$parameter, collapse = ", ")))
cat(sprintf("Insensitive (|change| <  5%%): %s\n",
            paste(insensitive$parameter, collapse = ", ")))

# =============================================================================
# 8. Save
# =============================================================================
results_list <- list(
  best_params_named = best_par,
  best_params       = matrix(best_par, nrow = 1),
  par_names         = par_names,
  lower_par         = lower_par,
  upper_par         = upper_par,
  sensitivity_tbl   = sensitivity_tbl,
  obj_fun           = best_val,
  calib_ids         = calib_ids,
  valid_ids         = valid_ids,
  date_run          = Sys.time()
)

out_file <- file.path(inputDir,
                      paste0("ga_result_", format(Sys.Date(), "%m_%d_%y"), ".rds"))
saveRDS(results_list, out_file)
cat(sprintf("Saved: %s\n", out_file))

# =============================================================================
# 9. GOF check
# =============================================================================
cumba_par_best <- cumba_par_base
for (i in seq_along(par_names))
  cumba_par_best[[par_names[i]]]$value <- best_par[i]

check_gof <- function(exp_ids, label) {
  res <- vector("list", length(exp_ids))
  names(res) <- as.character(exp_ids)
  for (iexp in exp_ids) {
    thisExp  <- soil |> filter(ID == iexp)
    thisYear <- irrigation_df |> filter(ID == iexp) |> slice_head() |> pull(YEAR)
    par_exp  <- cumba_par_best
    par_exp$FieldCapacity$value <- thisExp$FC / 100
    par_exp$WiltingPoint$value  <- thisExp$WP / 100
    res[[as.character(iexp)]] <- cumba_experiment(
      weather |> mutate(year = year(Date)) |> filter(year == thisYear),
      par_exp, estimateRad = TRUE, estimateET0 = TRUE,
      irrigationEfficiency = 1,
      irrigation_df |> filter(ID == iexp), fullOut = T
    )
  }
  out <- do.call(rbind, res) |>
    left_join(yields_all, by = c("experiment" = "ID", "doy")) |>
    group_by(experiment) |> slice_tail(n = 1) |> ungroup() |>
    filter(!is.na(yield_ref), !is.na(brix_ref))
  y_sim <- out$fruitFreshWeightAct * 0.01; y_obs <- out$yield_ref
  b_sim <- out$brixAct;                    b_obs <- out$brix_ref
  cat(sprintf("\n── %s (n=%d) ──\n", label, nrow(out)))
  cat(sprintf("  Yield: r=%.3f  RMSE=%.2f  nRMSE=%.1f%%  bias=%+.2f Mg/ha\n",
              cor(y_sim, y_obs), sqrt(mean((y_sim-y_obs)^2)),
              sqrt(mean((y_sim-y_obs)^2))/mean(y_obs)*100, mean(y_sim)-mean(y_obs)))
  cat(sprintf("  Brix:  r=%.3f  RMSE=%.3f  nRMSE=%.1f%%  bias=%+.3f °Brix\n",
              cor(b_sim, b_obs), sqrt(mean((b_sim-b_obs)^2)),
              sqrt(mean((b_sim-b_obs)^2))/mean(b_obs)*100, mean(b_sim)-mean(b_obs)))
}

cat("\n=== GOF check ===\n")
check_gof(calib_ids, "Calibration")
check_gof(valid_ids, "Validation")
cat("\nDone.\n")
