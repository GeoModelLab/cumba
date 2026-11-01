# CUMBA model - experiment runner

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
  estimateRad = TRUE,
  estimateET0 = TRUE,
  irrigation_df,
  irrigationEfficiency = 0.9,
  fullOut = FALSE,
  verbose = FALSE
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

  Numeric (0-1). Fraction of the observed applied water (`WVOL`) that
  effectively reaches the root zone. Default `0.9` (90%, typical for
  drip irrigation). The `irrigation` column in the output retains the
  original gross `WVOL` values; only the soil water balance uses the net
  fraction (`WVOL * irrigationEfficiency`). Set to `1` to disable the
  correction.

- fullOut:

  Logical. If `FALSE` (default) returns key outputs; if `TRUE` returns
  all internal variables.

- verbose:

  Logical. If `TRUE`, progress messages are printed to the console;
  default `FALSE`.

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

`irrigation_df`: must include columns `ID`, `Site`, `YEAR`, `DATE`,
`WVOL`.

## See also

[`as_tibble`](https://rdrr.io/pkg/tibble/man/as_tibble.html),
[`blue`](https://rdrr.io/pkg/crayon/man/crayon.html),
[`red`](https://rdrr.io/pkg/crayon/man/crayon.html)

## Examples

``` r
data(tomatoFoggia)
data(cumbaParameters)

# Replay one growing season from the bundled field dataset
wx <- subset(tomatoFoggia$weather, format(DATE, "%Y") == "2018")
out <- cumba_experiment(wx, cumbaParameters,
                        irrigation_df = tomatoFoggia$irrigation)
head(out)
#>     site year experiment doy    p irrigation              stage      swc yield
#> 1 Foggia 2018         29 117  0.0         20 post-transplanting 30.00000     0
#> 2 Foggia 2018         29 118  0.0          0 post-transplanting 29.73897     0
#> 3 Foggia 2018         29 119  0.0          0 post-transplanting 29.50209     0
#> 4 Foggia 2018         29 120  0.0          0 post-transplanting 29.29649     0
#> 5 Foggia 2018         29 121  0.0          0 post-transplanting 29.25888     0
#> 6 Foggia 2018         29 122 24.8          0         vegetative 30.00000     0
#>   brix
#> 1    0
#> 2    0
#> 3    0
#> 4    0
#> 5    0
#> 6    0
```
