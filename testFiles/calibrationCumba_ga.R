# =============================================================================
# CUMBA — Genetic Algorithm Calibration
# =============================================================================
# Best result to beat: obj_fun = 0.1822 (ga_result_06_19_26.rds)
#   r_yield=0.862  RMSE_yield=16.0  nRMSE_yield=19.2%  bias=-5.4
#   r_brix=0.749   RMSE_brix=0.381  nRMSE_brix=7.3%    bias=-0.05
#   r_yield_valid=0.942  r_brix_valid=0.761
#
# Calibrated parameters (13):
#   RUE, FruitWaterContentMax, FruitWaterContentDecreaseMax,
#   RootIncrease, WaterStressSensitivity, HalfIntSenescence, RootDepthMax,
#   CycleLength, DepletionFraction, HalfIntGrowth, KcMax, FIntMax, Topt
#
# Fixed (1): Tmax = 33
#
# Soil hydraulic parameters: FC / WP from Dataset_Carucci_et_al.xlsx
#   (hydraulic correction already embedded in dataset)
# Objective function: weighted average of nRMSE + (1-r) for yield and Brix
# Split: 70/30 stratified by year (set.seed = 42)
# =============================================================================

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(devtools)
library(GA)
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

soil <- read_excel(excel_file, sheet = "Soil")

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
# 2. Calibration / validation split (stratified by year, set.seed = 42)
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
# 3. Base parameters — fixed values
# =============================================================================
cumba_par_base <- cumba::cumbaParameters

# Literature / measured — never calibrated
cumba_par_base$Tbase$value                <- 10
cumba_par_base$Topt$value                 <- 24   # default; overridden by GA
cumba_par_base$Tmax$value                 <- 33   # FIXED: physiological ceiling
cumba_par_base$TransplantingLag$value     <- 9
cumba_par_base$InitialInt$value           <- 0.001
cumba_par_base$SoilWaterInitial$value     <- 100
cumba_par_base$FruitWaterContentMin$value <- 0.8
cumba_par_base$FloweringLag$value         <- 30
cumba_par_base$k0$value                   <- 4
cumba_par_base$FruitWaterContentInc$value <- 0.14
cumba_par_base$FloweringMax$value         <- 41
cumba_par_base$Tcold$value                <- 5
cumba_par_base$RootDepthInitial$value     <- 5

cat(sprintf("\nFixed: Tmax = %.0f°C\n", cumba_par_base$Tmax$value))

# =============================================================================
# 4. Parameter bounds — tightened around best 06/19 values
# =============================================================================
par_names <- c(
  "RUE",                          # best: 3.437
  "FruitWaterContentMax",         # best: 0.920
  "FruitWaterContentDecreaseMax", # best: 0.00250
  "RootIncrease",                 # best: 0.342
  "WaterStressSensitivity",       # best: 3.686
  "HalfIntSenescence",            # best: 96.12
  "RootDepthMax",                 # best: 90.76
  "CycleLength",                  # best: 1142.8
  "DepletionFraction",            # best: 63.45
  "HalfIntGrowth",                # best: 18.40
  "KcMax",                        # best: 1.191
  "FIntMax",                      # best: 0.927
  "Topt"                          # best: 24 (was fixed)
)

lower_par <- c(
  3.2,    # RUE
  0.90,   # FruitWaterContentMax
  0.001,  # FruitWaterContentDecreaseMax
  0.25,   # RootIncrease
  2.5,    # WaterStressSensitivity
  88.0,   # HalfIntSenescence
  80.0,   # RootDepthMax
  1050.0, # CycleLength
  55.0,   # DepletionFraction
  14.0,   # HalfIntGrowth
  1.05,   # KcMax
  0.82,   # FIntMax
  22.0    # Topt
)

upper_par <- c(
  3.8,    # RUE
  0.95,   # FruitWaterContentMax
  0.005,  # FruitWaterContentDecreaseMax
  0.50,   # RootIncrease
  5.5,    # WaterStressSensitivity
  108.0,  # HalfIntSenescence
  98.0,   # RootDepthMax
  1250.0, # CycleLength
  70.0,   # DepletionFraction
  25.0,   # HalfIntGrowth
  1.35,   # KcMax
  0.98,   # FIntMax
  28.0    # Topt
)

cat("\nParameter bounds:\n")
for (i in seq_along(par_names))
  cat(sprintf("  %-36s [%.4f – %.4f]   best: see header\n",
              par_names[i], lower_par[i], upper_par[i]))

# =============================================================================
# 5. Objective function
# =============================================================================
loss_function_ga <- function(params) {
  
  cumba_par <- cumba_par_base
  cumba_par$RUE$value                          <- params[1]
  cumba_par$FruitWaterContentMax$value         <- params[2]
  cumba_par$FruitWaterContentDecreaseMax$value <- params[3]
  cumba_par$RootIncrease$value                 <- params[4]
  cumba_par$WaterStressSensitivity$value       <- params[5]
  cumba_par$HalfIntSenescence$value            <- params[6]
  cumba_par$RootDepthMax$value                 <- params[7]
  cumba_par$CycleLength$value                  <- params[8]
  cumba_par$DepletionFraction$value            <- params[9]
  cumba_par$HalfIntGrowth$value                <- params[10]
  cumba_par$KcMax$value                        <- params[11]
  cumba_par$FIntMax$value                      <- params[12]
  cumba_par$Topt$value                         <- params[13]
  
  results <- vector("list", length(calib_ids))
  names(results) <- as.character(calib_ids)
  
  for (iexp in calib_ids) {
    thisExp  <- soil |> filter(ID == iexp)
    thisYear <- irrigation_df |> filter(ID == iexp) |> slice_head() |> pull(YEAR)
    
    par_exp <- cumba_par
    par_exp$FieldCapacity$value <- thisExp$FC / 100
    par_exp$WiltingPoint$value  <- thisExp$WP / 100
    
    tryCatch({
      results[[as.character(iexp)]] <- cumba_experiment(
        weather |> mutate(year = year(Date)) |> filter(year == thisYear),
        par_exp,
        estimateRad = TRUE, estimateET0 = TRUE,
        irrigation_df |> filter(ID == iexp),
        fullOut = TRUE
      )
    }, error = function(e) {
      message(sprintf("  Experiment %d failed: %s", iexp, e$message))
      results[[as.character(iexp)]] <<- NULL
    })
  }
  
  results <- Filter(Negate(is.null), results)
  if (length(results) < 5) return(9999)
  
  # Penalty: any cycle shorter than 100 days
  cycle_days <- do.call(rbind, results) |>
    filter(cycleCompletion > 0) |>
    group_by(experiment) |>
    summarise(n_days = n(), .groups = "drop")
  if (nrow(cycle_days) == 0 || any(cycle_days$n_days < 100)) {
    cat(sprintf("  PENALTY: min cycle = %d days\n",
                if (nrow(cycle_days) > 0) min(cycle_days$n_days) else 0))
    return(9999)
  }
  
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
  
  cat(sprintf(
    "nRMSE: y=%.3f b=%.3f | r: y=%.3f b=%.3f | obj=%.4f  [cycle: min=%dd mean=%dd]\n",
    nrmse_yield, nrmse_brix, r_yield, r_brix, obj_fun,
    min(cycle_days$n_days), round(mean(cycle_days$n_days))
  ))
  
  obj_fun
}

# =============================================================================
# 6. Run GA
# =============================================================================
cat("\n=== Starting GA calibration ===\n")
cat(sprintf("Parameters: %d | Calibration experiments: %d\n",
            length(par_names), length(calib_ids)))

n_cores <- detectCores() - 1
cat(sprintf("Cores: %d\n", n_cores))

ga_result <- ga(
  type       = "real-valued",
  fitness    = function(params) -loss_function_ga(params),
  lower      = lower_par,
  upper      = upper_par,
  popSize    = 100,
  maxiter    = 200,
  pmutation  = 0.15,
  pcrossover = 0.8,
  elitism    = 5,
  seed       = 42,
  monitor    = TRUE,
  parallel   = n_cores
)

# =============================================================================
# 7. Extract and save results
# =============================================================================
best_params        <- as.numeric(ga_result@solution[1, ])
names(best_params) <- par_names

cat("\n=== Best parameters ===\n")
for (nm in par_names)
  cat(sprintf("  %-36s = %.6f\n", nm, best_params[[nm]]))
cat(sprintf("\nBest obj_fun: %.4f\n", -ga_result@fitnessValue))
cat(sprintf("Target:       0.1822\n"))

results_list <- list(
  best_params       = ga_result@solution,
  best_params_named = best_params,
  ga_result_summary = summary(ga_result),
  par_names         = par_names,
  lower_par         = lower_par,
  upper_par         = upper_par,
  calib_ids         = calib_ids,
  valid_ids         = valid_ids,
  date_run          = Sys.time()
)

out_file <- file.path(
  inputDir,
  paste0("ga_result_", format(Sys.Date(), "%m_%d_%y"), ".rds")
)
saveRDS(results_list, out_file)
cat(sprintf("Saved: %s\n", out_file))

# =============================================================================
# 8. GOF check on calibration and validation
# =============================================================================
cumba_par_best <- cumba_par_base
cumba_par_best$RUE$value                          <- best_params[["RUE"]]
cumba_par_best$FruitWaterContentMax$value         <- best_params[["FruitWaterContentMax"]]
cumba_par_best$FruitWaterContentDecreaseMax$value <- best_params[["FruitWaterContentDecreaseMax"]]
cumba_par_best$RootIncrease$value                 <- best_params[["RootIncrease"]]
cumba_par_best$WaterStressSensitivity$value       <- best_params[["WaterStressSensitivity"]]
cumba_par_best$HalfIntSenescence$value            <- best_params[["HalfIntSenescence"]]
cumba_par_best$RootDepthMax$value                 <- best_params[["RootDepthMax"]]
cumba_par_best$CycleLength$value                  <- best_params[["CycleLength"]]
cumba_par_best$DepletionFraction$value            <- best_params[["DepletionFraction"]]
cumba_par_best$HalfIntGrowth$value                <- best_params[["HalfIntGrowth"]]
cumba_par_best$KcMax$value                        <- best_params[["KcMax"]]
cumba_par_best$FIntMax$value                      <- best_params[["FIntMax"]]
cumba_par_best$Topt$value                         <- best_params[["Topt"]]

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
      irrigation_df |> filter(ID == iexp), fullOut = FALSE
    )
  }
  
  out <- do.call(rbind, res) |>
    left_join(yields_all, by = c("experiment" = "ID", "doy")) |>
    group_by(experiment) |> slice_tail(n = 1) |> ungroup() |>
    filter(!is.na(yield_ref), !is.na(brix_ref))
  
  y_sim <- out$fruitFreshWeightAct * 0.01
  y_obs <- out$yield_ref
  b_sim <- out$brixAct
  b_obs <- out$brix_ref
  
  cat(sprintf("\n── %s (n=%d) ──\n", label, nrow(out)))
  cat(sprintf("  Yield: r=%.3f  RMSE=%.2f  nRMSE=%.1f%%  bias=%+.2f Mg/ha\n",
              cor(y_sim, y_obs), sqrt(mean((y_sim-y_obs)^2)),
              sqrt(mean((y_sim-y_obs)^2))/mean(y_obs)*100,
              mean(y_sim) - mean(y_obs)))
  cat(sprintf("  Brix:  r=%.3f  RMSE=%.3f  nRMSE=%.1f%%  bias=%+.3f °Brix\n",
              cor(b_sim, b_obs), sqrt(mean((b_sim-b_obs)^2)),
              sqrt(mean((b_sim-b_obs)^2))/mean(b_obs)*100,
              mean(b_sim) - mean(b_obs)))
}

cat("\n=== GOF check ===\n")
check_gof(calib_ids, "Calibration")
check_gof(valid_ids, "Validation")
cat("\nDone.\n")