#' cumba: Carbon Use Model for Yield and Brix Assessment in Processing Tomato
#'
#' A daily time-step crop model for processing tomato that simulates phenology,
#' a three-layer soil-water balance, and fresh-fruit yield and Brix. Two entry
#' points are provided: \code{\link{cumba_experiment}}, which replays observed
#' irrigation schedules (for calibration), and \code{\link{cumba_scenario}},
#' which schedules deficit irrigation automatically when the crop water
#' status (1 minus the water stress index) falls below phase-specific
#' thresholds.
#'
#' @keywords internal
"_PACKAGE"
