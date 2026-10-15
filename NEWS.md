# cumba 0.1.0

* Initial release.
* `cumba_experiment()`: runs the CUMBA (Carbon Use Model for Brix Assessment)
  model replaying a user-supplied irrigation schedule, for calibration and
  validation against field observations.
* `cumba_scenario()`: runs the model with automatic deficit-irrigation
  scheduling, triggered from phase-specific soil-water-status thresholds
  (`wsLevel`) with a minimum turn interval and an end-of-cycle cut-off.
* Bundled datasets: `tomatoFoggia` (32 field experiments, nine seasons,
  Capitanata plain) and `cumbaParameters` (default parameter set).
