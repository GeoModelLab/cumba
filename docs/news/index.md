# Changelog

## cumba 0.1.0

- Initial release.
- [`cumba_experiment()`](https://GeoModelLab.github.io/cumba/reference/cumba_experiment.md):
  runs the CUMBA (Carbon Use Model for Yield and Brix Assessment) model
  replaying a user-supplied irrigation schedule, for calibration and
  validation against field observations.
- [`cumba_scenario()`](https://GeoModelLab.github.io/cumba/reference/cumba_scenario.md):
  runs the model with automatic deficit-irrigation scheduling, triggered
  when the crop water status (`ws = 1 - WSI`) falls below phase-specific
  thresholds (`wsLevel`), with a minimum turn interval and an optional
  end-of-cycle cut-off.
- Bundled datasets: `tomatoFoggia` (32 field experiments, nine seasons,
  Capitanata plain) and `cumbaParameters` (model parameters; RUE, Theat,
  k0 and FruitWaterContentDecreaseMax calibrated on the 18 calibration
  experiments of `tomatoFoggia`).
