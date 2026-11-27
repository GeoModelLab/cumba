# Analyses behind the cumba papers

This folder holds the scripts and data that produce the numbers of the SoftwareX
article (second revision, November 2026) and of the research paper on the CUMBA
agronomic assistant. It is not part of the R package (`^analysis$` is in
`.Rbuildignore`).

Everything here was run with **cumba 0.1.0 as published on CRAN**
(`install.packages("cumba")`), which is identical to commit `4801854` of this
repository. Run the scripts from the repository root, in a fresh R session,
without `devtools::load_all()`.

## What is here

| Path | Content |
|---|---|
| `softwarex_R2/verifica_table2.R` | Table 2, yield and Brix: metrics on the calibration and validation subsets |
| `softwarex_R2/verifica_esempi.R` | Examples of Sections 3.1 and 3.2 as printed in the article; writes `output/obs_sim.csv` |
| `softwarex_R2/verifica_swc.R` | Table 2, soil water content |
| `softwarex_R2/verifica_scenario_pareto.R` | Scenario experiment: Pareto front, Table 3, phase effects; writes `output/strategie_medie_pareto.csv` |
| `softwarex_R2/make_fig5.py` | Figure 5 of the article, from the two files in `output/` |
| `original_scripts/` | The scripts as they were run for the research paper (calibration, figures, scenario). They contain absolute paths of the machine they were run on |
| `data/ga_result_06_19_26.rds` | Calibrated parameters (RUE, Theat, k0, FruitWaterContentDecreaseMax) and the calibration/validation split |
| `data/results_irrigation_optimizer_calibrated.rds` | Scenario experiment: 5,632 seasonal results (256 strategies x 22 years) |
| `data/SoilWaterContent.xlsx` | Soil water content measured in five experiments (2017-2018) |
| `output/` | Files written by the scripts above, and Figure 5 |
| `private/` | Not tracked. Place `weather_foggia.csv` here to re-run the scenario experiment |

The field capacity and wilting point of each experiment are read from the sheet
`Soil` of `data-raw/Dataset_Carucci_et_al.xlsx`.

## Settings that the numbers depend on

**Model evaluation (Table 2).** Field capacity and wilting point measured in
each season's field; observed irrigation volumes applied in full
(`irrigationEfficiency = 1`); calibration/validation split drawn with
`set.seed(42)`, 70% of the experiments within each season (18 and 14
experiments). With the package default soil and `irrigationEfficiency = 0.9`
the metrics are worse (yield RMSE about 18 instead of 12 Mg/ha).

**Calibration.** Four parameters, optimised with the `nloptr` package (BOBYQA,
Nelder-Mead refinement, ten restarts) on the 18 calibration experiments; see
`original_scripts/cumba_calibration_GA_6.R`. The file name says "GA" for
historical reasons: no genetic algorithm is involved.

**Scenario experiment (Table 3, Figure 5A and C).** Four `wsLevel` values (0.6
to 0.9) in each of three phases and four transplanting dates (DOY 110 to 125);
22 years of Foggia weather (2000-2021); soil with field capacity 0.35 and
wilting point 0.15; `turnMin = 2`; `irrigationEfficiency = 0.9`;
`irrigationStopCycle = 90`; `CycleLength` 1260 instead of the default 1110 (the
weather source differs from that of the field experiments and the cycle was
adjusted to reproduce their mean duration); `FruitWaterContentMax` 0.905
instead of 0.92. Yield and Brix are read on the first day with
`cycleCompletion >= 100`. With the package defaults the results differ (for
example, Brix about 4.8 instead of 5.8).

The weather series of the scenario experiment is private and is not
distributed. `verifica_scenario_pareto.R` reads the saved results by default
(`RERUN <- FALSE`); with the weather file in `private/` and `RERUN <- TRUE` it
repeats the 5,632 simulations, which reproduce the saved results to machine
precision.

## Expected results

| Quantity | Value |
|---|---|
| Yield, calibration / validation | r 0.904 / 0.943; RMSE 12.11 / 12.08 Mg/ha; bias -2.50 / -1.23 |
| Brix, calibration / validation | r 0.764 / 0.786; RMSE 0.348 / 0.475 |
| Rainfed experiments (3) | simulated 14.9, 15.4, 30.9 Mg/ha; observed mean 34.6 |
| Soil water content | n 385; r 0.829; RMSE 0.0403 m3/m3; nRMSE 11.3%; bias -0.0137 |
| Example 3.2, mean seasonal irrigation | 472, 378, 371 mm |
| Pareto front (5 objectives) | 128 non-dominated strategies of 256 |
| Compromise (closest to the ideal point) | 0.8/0.7/0.6, DOY 110: 113.7 Mg/ha, Brix 5.75, 411 mm |
| Second closest | 0.7/0.7/0.6, DOY 110: 108.9 Mg/ha, Brix 5.76, 392 mm (distance 0.855 against 0.851) |
| Effect of wsLevel 0.9 against 0.6 on yield | vegetative +11.47, reproductive +22.85, ripening +5.31 Mg/ha |
| Effect on irrigation | +70.6, +50.4, +32.3 mm |

## Known points

- The set of 128 strategies used in earlier drafts (vegetative `wsLevel` of 0.6
  or 0.7) is not the Pareto front: it shares 54 strategies with it. The
  commented block in `original_scripts/optimizeTomatoCultivation.R` explains
  how it arose.
- `cumba_experiment()` and `cumba_scenario()` compute the daily water status
  from the soil water of the previous day and of the same day respectively;
  replaying a scenario schedule through `cumba_experiment()` changes the final
  yield by about 0.5%.
- In cumba 0.1.0 the vignette `cumba-experiment` divides the simulated yield by
  10; both functions already return yield in t/ha.
