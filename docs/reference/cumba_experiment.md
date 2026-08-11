# CUMBA model — experiment runner

Runs the daily time-step CUMBA simulation for one or more
sites/experiments, computing tomato yield and Brix from weather inputs
and irrigation options. The function validates inputs, converts a
list-of-lists parameter object to a tibble of numeric values when
needed, and prints a blue message indicating it is running in
\*experiment\* mode.

## Usage

``` r
cumba_experiment(
  weather,
  param,
  estimateRad = T,
  estimateET0 = T,
  irrigation_df,
  irrigationEfficiency = 0.9,
  fullOut = F
)
```

## Arguments

- weather:

  A `data.frame` (or tibble) of daily weather by site with at least
  `Site`, `Tx`, `Tn`, `P`, `DATE` and, based on flags, either `Lat` (to
  estimate radiation) or `Rad` (provided radiation). If
  `estimateET0 = FALSE`, an `ET0` column must be present.

- param:

  Model parameters. Either:

  - a *named list* where each element is itself a list with fields
    `value`, `min`, `max`, `description`; this form is automatically
    converted to a tibble of parameter *values*; or

  - a tibble/data.frame already containing the required parameter
    values.

  The object must contain (as names/columns) all of: `Tbase`, `Topt`,
  `Tmax`, `Theat`, `Tcold`, `FIntMax`, `CycleLength`,
  `TransplantingLag`, `FloweringLag`, `HalfIntGrowth`,
  `HalfIntSenescence`, `InitialInt`, `RUE`, `KcIni`, `KcMax`,
  `RootIncrease`, `RootDepthMax`, `RootDepthInitial`, `FieldCapacity`,
  `WiltingPoint`, `DepletionFraction`, `FloweringSlope`, `FloweringMax`,
  `k0`, `FruitWaterContentMin`, `FruitWaterContentMax`,
  `FruitWaterContentInc`, `FruitWaterContentDecreaseMax`. Cardinal
  temperatures are validated so that `Tmax > Topt > Tbase`.

- estimateRad:

  Logical. If `TRUE` (default), solar radiation is estimated from
  temperature (Hargreaves) and `Lat` must be in `weather`. If `FALSE`,
  `weather` must include `Rad`.

- estimateET0:

  Logical. If `TRUE` (default), reference evapotranspiration (ET0) is
  estimated from temperature (Hargreaves). If `FALSE`, `weather` must
  include an `ET0` column.

- irrigation_df:

  A `data.frame` with the irrigation schedule for each experiment/site,
  containing columns `ID`, `Site`, `YEAR`, `DATE`, `WVOL`.

- irrigationEfficiency:

  Numeric (0–1). Fraction of the observed applied water (`WVOL`) that
  effectively reaches the root zone. Default `0.9` (90%, typical for
  drip irrigation). The `irrigation` column in the output retains the
  original gross `WVOL` values; only the soil water balance uses the net
  fraction (`WVOL * irrigationEfficiency`). Set to `1` to disable the
  correction.

- fullOut:

  Logical. If `FALSE` (default) returns key outputs; if `TRUE` returns
  all internal variables.

## Value

A `data.frame` containing the input `weather` plus the daily CUMBA
outputs per site and date; the set of columns depends on `fullOut`.

## Details

On start, the function prints `"CUMBA running in experiment mode"` in
blue (via crayon). It checks that all required columns/elements are
present and throws informative errors if anything is missing. Sites are
inferred from the unique values of `weather$Site`.

If `param` is supplied as a list-of-lists with a `value` field, it is
converted internally with
`` tibble::as_tibble(lapply(param, `[[`, "value")) ``.

## Required inputs

**weather**: must include columns `Site`, `Tx`, `Tn`, `P`, `DATE`.
Depending on the flags:

- if `estimateRad = TRUE` (default), column `Lat` is required;

- if `estimateRad = FALSE`, column `Rad` is required;

- if `estimateET0 = FALSE`, column `ET0` is required.

**irrigation\\df**: must include columns `ID`, `Site`, `YEAR`, `DATE`,
`WVOL`.

## See also

[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html),
[`blue`](http://r-lib.github.io/crayon/reference/crayon.md),
[`red`](http://r-lib.github.io/crayon/reference/crayon.md)

## Examples

``` r
# Minimal reproducible inputs (toy data)
weather <- data.frame(
  Site = "TestSite",
  Tx   = c(30, 32),
  Tn   = c(20, 21),
  P    = c(0, 5),
  DATE = as.Date(c("2025-06-01", "2025-06-02")),
  Lat  = 40
)

irrigation_df <- data.frame(
  ID = 1L,
  Site = "TestSite",
  YEAR = 2025L,
  DATE = as.Date(c("2025-06-01", "2025-06-02")),
  WVOL = c(5, 10)
)

# Parameters as list-of-lists (auto-converted):
param <- list(
  Tbase = list(value = 8, min = 0, max = 15, description = "Base T"),
  Topt  = list(value = 26, min = 15, max = 35, description = "Opt T"),
  Tmax  = list(value = 40, min = 30, max = 50, description = "Max T"),
  Theat = list(value = 35), Tcold = list(value = 5),
  FIntMax = list(value = 1), CycleLength = list(value = 120),
  TransplantingLag = list(value = 5), FloweringLag = list(value = 40),
  HalfIntGrowth = list(value = 0.5), HalfIntSenescence = list(value = 0.8),
  InitialInt = list(value = 0.05), RUE = list(value = 2.5),
  KcIni = list(value = 0.6), KcMax = list(value = 1.15),
  RootIncrease = list(value = 0.01), RootDepthMax = list(value = 1.0),
  RootDepthInitial = list(value = 0.1), FieldCapacity = list(value = 0.30),
  WiltingPoint = list(value = 0.12), DepletionFraction = list(value = 0.5),
  FloweringSlope = list(value = 1), FloweringMax = list(value = 1),
  k0 = list(value = 0.01),
  FruitWaterContentMin = list(value = 0.90),
  FruitWaterContentMax = list(value = 0.95),
  FruitWaterContentInc = list(value = 0.001),
  FruitWaterContentDecreaseMax = list(value = 0.002)
)

# Run (uncomment when the function body is implemented):
# result <- cumba_experiment(weather, param, irrigation_df = irrigation_df)
```
