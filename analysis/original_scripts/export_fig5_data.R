# =============================================================================
# export_fig5_data.R — export the data needed to redraw Figure 5 of the
# SoftwareX revision (timeline + irrigation strategies, yield above, Brix below).
#
# HOW TO RUN  (from the cumba_R_package project root, in RStudio):
#     source("testFiles/export_fig5_data.R")
#
# It reuses your own companion-paper script, so the calibrated parameters,
# the treatment labels and the set.seed(42) calibration/validation split are
# EXACTLY the ones behind the published figures — nothing is re-derived here.
#
# It writes three small files into manuscript/figures/ and prints a summary.
# Note: sourcing the figure script also regenerates the companion figures;
# that is harmless. If `out_all` is already in your environment you can skip
# the source() line and run the rest on its own.
# =============================================================================

if (!exists("out_all")) {
  source("testFiles/cumba_figures_paper.R")
}

stopifnot(exists("out_all"))

out_dir <- "manuscript/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. the daily simulated series joined to the observations, with the subset flag
readr::write_csv(as.data.frame(out_all),
                 file.path(out_dir, "fig5_out_all.csv"), na = "")

# 2. which experiments went into calibration
readr::write_csv(data.frame(ID = calib_ids),
                 file.path(out_dir, "fig5_calib_ids.csv"))

# 3. the treatment-label lookup and the cycle length used to express time as %
if (exists("strategy_labels")) {
  readr::write_csv(data.frame(code  = names(strategy_labels),
                              label = unname(strategy_labels)),
                   file.path(out_dir, "fig5_strategy_labels.csv"))
}
readr::write_csv(data.frame(CycleLength = par$CycleLength$value),
                 file.path(out_dir, "fig5_cycle_length.csv"))

cat("\n--- exported ---\n")
cat("rows in out_all      :", nrow(out_all), "\n")
cat("columns              :", paste(names(out_all), collapse = ", "), "\n")
cat("experiments          :", length(unique(out_all$experiment)), "\n")
cat("calibration / total  :", length(calib_ids), "/",
    length(unique(out_all$experiment)), "\n")
cat("files written to     :", normalizePath(out_dir), "\n")
