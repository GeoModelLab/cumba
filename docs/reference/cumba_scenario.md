# CUMBA model — automatic irrigation scenario runner

Runs the daily time-step CUMBA simulation in *scenario* (automatic
irrigation) mode. Unlike `cumba`, which requires an observed irrigation
schedule, `cumba_scenario` computes irrigation autonomously based on
soil-water status, phenological stage, and user-defined stress
thresholds. This is the function used by the CUMBA Shiny app for
decision support and what-if analysis.

## Usage

``` r
cumba_scenario(
  weather,
  param,
  estimateRad = TRUE,
  estimateET0 = TRUE,
  transplantingDOY = 120,
  irrigationStrategy = list(vegetative = list(wsLevel = 0.5, turnMin = 2), reproductive =
    list(wsLevel = 0.5, turnMin = 2), ripening = list(wsLevel = 0.5, turnMin = 2)),
  irrigationStopCycle = 100,
  irrigationEfficiency = 0.9,
  fullOut = FALSE,
  irrigationOverride = NULL
)
```

## Arguments

- weather:

  A `data.frame` (or tibble) of daily weather by site. See *Required
  weather columns* above.

- param:

  Model parameters. Either a *named list* where each element is itself a
  list with fields `value`, `min`, `max`, `description` (as returned by
  [`cumbaParameters`](https://GeoModelLab.github.io/cumba/reference/cumbaParameters.md)),
  or a tibble/data.frame of numeric parameter values already unpacked.
  See `cumba` for the full list of required parameter names.

- estimateRad:

  Logical. If `TRUE` (default), solar radiation is estimated from
  temperature (Hargreaves); `Lat` must be in `weather`. If `FALSE`,
  `weather` must include `Rad`.

- estimateET0:

  Logical. If `TRUE` (default), ET0 is estimated from temperature
  (Hargreaves). If `FALSE`, `weather` must include `ET0`.

- transplantingDOY:

  Integer. Day of year (1–365) of transplanting. The model starts
  accumulating GDD and running the water balance from this day. Default
  `120` (approx. 30 April).

- irrigationStrategy:

  A named list with three phenological phases — `vegetative`,
  `reproductive`, `ripening` — each containing:

  `wsLevel`

  :   Numeric (0–1). Fraction of transpirable soil water below which
      irrigation is triggered (water-stress threshold).

  `turnMin`

  :   Integer. Minimum number of days that must have elapsed since the
      last irrigation event before a new one is allowed.

  Default: all phases use `wsLevel = 0.5` and `turnMin = 2`.

- irrigationStopCycle:

  Numeric (0–100). Cycle-completion percentage beyond which automatic
  irrigation is suppressed regardless of soil-water status. Useful to
  implement a cut-off irrigation strategy near harvest. Default `100`
  (no cut-off; irrigation runs until the end of the cycle).

- irrigationEfficiency:

  Numeric (0–1). Fraction of applied water that effectively reaches the
  root zone. Default `0.9` (90%, typical for drip irrigation). The model
  computes the net soil-water deficit and divides it by this value to
  obtain the *gross* volume the farmer must apply; the `irrigation`
  column in the output reports this gross amount, while the soil water
  balance uses only the net fraction (`gross * efficiency`). Set to `1`
  to disable the efficiency correction (e.g. for sprinkler or furrow
  irrigation where efficiency is accounted for elsewhere).

- fullOut:

  Logical. If `FALSE` (default), the function returns the key agronomic
  outputs (`stage`, `swc`, `yield`, `brix`, `irrigation`, `p`) plus
  site/date identifiers. If `TRUE`, all internal state variables are
  included.

- irrigationOverride:

  A `data.frame` (or `NULL`) that lets the user manually override the
  model's automatic irrigation decision for specific dates. Must contain
  columns `date` (class `Date`) and `action` (`"skip"` or `"applied"`).
  An optional `mm` column specifies the volume when
  `action = "applied"`.

  `"skip"`

  :   Forces `irrigation = 0` on that date, even if the model would have
      irrigated.

  `"applied"`

  :   Forces `irrigation = mm` on that date, even if the model would not
      have irrigated.

  Default `NULL` (no overrides).

## Value

A `data.frame` of daily outputs. When `fullOut = FALSE` (default) the
columns are: `site`, `year`, `doy`, `p` (precipitation, mm),
`irrigation` (mm), `stage` (phenological stage label), `swc` (soil water
content, %), `yield` (fruit fresh weight, t/ha), `brix` (degrees Brix).
When `fullOut = TRUE`, all internal state variables are included.

## Details

On each simulated day the model: (1) updates the two-layer soil-water
balance; (2) computes the water-stress coefficient (`ws`) from the
fraction of transpirable soil water (FTSW); (3) decides whether to
irrigate — irrigation is applied when `ws < wsLevel` for the current
phase, at least `turnMin` days have elapsed since the last event,
`cycleCompletion < irrigationStopCycle`, and the crop is in an active
phenological phase (`phenoCode > 0`); (4) applies `irrigationEfficiency`
so that the gross applied volume equals `net deficit / efficiency` while
only the net fraction enters the soil; (5) computes biomass
accumulation, fruit growth, and Brix dynamics.

Any `irrigationOverride` entries are applied after the automatic
decision and before the water-balance update, so they correctly
propagate to soil moisture in subsequent days.

The function prints `"CUMBA running in deficit irrigation mode"` in blue
(via crayon) when called.

## Required weather columns

`weather` must contain `Site`, `Tx`, `Tn`, `P`, `DATE`. Additionally:

- if `estimateRad = TRUE` (default), `Lat` is required;

- if `estimateRad = FALSE`, `Rad` must be present;

- if `estimateET0 = FALSE`, `ET0` must be present.

## See also

`cumba` for the experiment-mode runner that requires an observed
irrigation schedule;
[`cumbaParameters`](https://GeoModelLab.github.io/cumba/reference/cumbaParameters.md)
for the default parameter set.

## Examples

``` r
weather <- data.frame(
  Site = "TestSite",
  Tx   = c(30, 32, 31),
  Tn   = c(20, 21, 19),
  P    = c(0, 5, 2),
  DATE = as.Date(c("2025-06-01", "2025-06-02", "2025-06-03")),
  Lat  = 40
)
param <- cumbaParameters()
#> Error in cumbaParameters(): could not find function "cumbaParameters"

# Default strategy: irrigate at ws < 0.5, minimum 2 days between events
if (FALSE) { # \dontrun{
result <- cumba_scenario(weather, param)
} # }

# Custom strategy: stricter thresholds, cut off irrigation at 80% cycle
if (FALSE) { # \dontrun{
result <- cumba_scenario(
  weather,
  param,
  transplantingDOY    = 120,
  irrigationStrategy  = list(
    vegetative   = list(wsLevel = 0.5, turnMin = 3),
    reproductive = list(wsLevel = 0.4, turnMin = 2),
    ripening     = list(wsLevel = 0.6, turnMin = 4)
  ),
  irrigationStopCycle = 80
)
} # }
```
