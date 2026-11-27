# =============================================================================
# check_pareto.R — verify the Pareto front of the irrigation optimisation.
#
# Run from the cumba_R_package project root:
#     source("testFiles/check_pareto.R")
#
# It rebuilds results_agg exactly as testFiles/optimizeTomatoCultivation.R does,
# re-runs the same psel() call, and tests the one assumption that the current
# code makes implicitly: that psel() returns its rows in the same order as the
# data frame it was given.
# =============================================================================

library(dplyr)
library(rPref)

KEY <- c("vegetativeWS", "reproductiveWS", "ripeningWS", "transplantingDOY")

results_df <- readRDS("testFiles/results_irrigation_optimizer_calibrated.rds")

results_agg <- results_df |>
  group_by(vegetativeWS, reproductiveWS, ripeningWS, transplantingDOY) |>
  summarise(across(c(yield, brix, irrigation, n_irrigation, iwue),
                   list(mean = \(x) mean(x, na.rm = TRUE),
                        sd   = \(x) sd(x,   na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

pref <- high(yield_mean) * high(brix_mean) * low(irrigation_mean) *
        high(iwue_mean) * low(n_irrigation_mean)

pareto_levels <- psel(results_agg, pref, top = nrow(results_agg))

cat("\n=== 1. the assumption behind `results_agg$pareto_level <- pareto_levels$.level` ===\n")
same_order <- identical(as.data.frame(results_agg[, KEY]),
                        as.data.frame(pareto_levels[, KEY]))
cat("rows of psel() are in the same order as results_agg :", same_order, "\n")
cat("(if FALSE, assigning .level by position mislabels every row)\n")

# --- what the current code produces -----------------------------------------
agg_pos <- results_agg
agg_pos$pareto_level <- pareto_levels$.level
front_pos <- filter(agg_pos, pareto_level == 1L)

# --- what the join produces (the fix) ---------------------------------------
agg_fix <- results_agg |>
  left_join(pareto_levels |>
              select(all_of(KEY), pareto_level = .level),
            by = KEY)
front_fix <- filter(agg_fix, pareto_level == 1L)

cat("\n=== 2. front size ===\n")
cat("positional assignment :", nrow(front_pos), "strategies\n")
cat("join on the 4 keys    :", nrow(front_fix), "strategies\n")
cat("(the two agree in SIZE, which is why the bug is invisible in the counts)\n")

cat("\n=== 3. composition by vegetative wsLevel ===\n")
cat("positional :"); print(table(front_pos$vegetativeWS))
cat("join       :"); print(table(front_fix$vegetativeWS))

cat("\n=== 4. the decisive case: the strategy with the highest mean yield ===\n")
best <- results_agg |> slice_max(yield_mean, n = 1, with_ties = FALSE)
cat(sprintf("veg=%.1f rep=%.1f rip=%.1f DOY=%d  yield=%.2f\n",
            best$vegetativeWS, best$reproductiveWS, best$ripeningWS,
            best$transplantingDOY, best$yield_mean))
cat("ties on this yield value          :", sum(results_agg$yield_mean == best$yield_mean), "\n")
lv_pos <- agg_pos |> semi_join(best, by = KEY) |> pull(pareto_level)
lv_fix <- agg_fix |> semi_join(best, by = KEY) |> pull(pareto_level)
cat("pareto_level, positional assignment:", lv_pos, "\n")
cat("pareto_level, join (what psel said):", lv_fix, "\n")
cat("It is the unique maximum of one objective, so nothing can dominate it:\n")
cat("  a correct front MUST contain it.\n")

cat("\n=== 5. single-objective optima within the front ===\n")
show <- function(df, lab) {
  f <- function(d, what) sprintf("%-14s veg=%.1f rep=%.1f rip=%.1f DOY=%d | yield=%6.2f brix=%.2f irr=%3.0f n=%4.1f iwue=%.3f",
        what, d$vegetativeWS, d$reproductiveWS, d$ripeningWS, d$transplantingDOY,
        d$yield_mean, d$brix_mean, d$irrigation_mean, d$n_irrigation_mean, d$iwue_mean)
  cat("--", lab, "\n")
  cat(" ", f(slice_max(df, yield_mean, n = 1, with_ties = FALSE), "max yield"),  "\n")
  cat(" ", f(slice_max(df, brix_mean,  n = 1, with_ties = FALSE), "max brix"),   "\n")
  cat(" ", f(slice_min(df, irrigation_mean, n = 1, with_ties = FALSE), "min irrigation"), "\n")
  cat(" ", f(slice_max(df, iwue_mean,  n = 1, with_ties = FALSE), "max iwue"),   "\n")
}
show(front_pos, "positional assignment (what the paper reports)")
show(front_fix, "join on the 4 keys (correct)")

cat("\n=== 6. fix ===\n")
cat("replace\n")
cat("    results_agg$pareto_level <- pareto_levels$.level\n")
cat("with\n")
cat("    results_agg <- results_agg |>\n")
cat("      left_join(pareto_levels |>\n")
cat("                  select(all_of(c('vegetativeWS','reproductiveWS',\n")
cat("                                  'ripeningWS','transplantingDOY')),\n")
cat("                         pareto_level = .level),\n")
cat("                by = c('vegetativeWS','reproductiveWS',\n")
cat("                       'ripeningWS','transplantingDOY'))\n")

invisible(list(positional = front_pos, fixed = front_fix))
