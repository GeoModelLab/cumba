# Getting started with cumba

## What is `cumba`?

**CUMBA** (Carbon Use Model for yield and Brix Assessment) is a daily
time-step crop model for **processing tomato**. Given daily weather and
an irrigation schedule, it computes:

- thermal time, phenology and root growth,
- a soil-water balance on three layers,
- carbon accumulation modulated by heat, cold and water stress,
- fresh-fruit yield and **Brix** at harvest.

The package exposes two top-level entry points:

- [`cumba_experiment()`](https://GeoModelLab.github.io/cumba/reference/cumba_experiment.md)
  — run the model on one or more **observed experiments**, using a
  user-supplied `irrigation_df`.
- [`cumba_scenario()`](https://GeoModelLab.github.io/cumba/reference/cumba_scenario.md)
  — run the model in **deficit-irrigation scenario mode**, where
  irrigation is automatically triggered when the simulated water-stress
  falls below a phase-specific threshold.

``` r

library(cumba)
```

## Default parameters

A complete default parameter set is bundled with the package:

``` r

str(cumbaParameters, max.level = 1)
#> List of 30
#>  $ CycleLength                 :List of 4
#>  $ DepletionFraction           :List of 4
#>  $ FieldCapacity               :List of 4
#>  $ FIntMax                     :List of 4
#>  $ FloweringLag                :List of 4
#>  $ FloweringMax                :List of 4
#>  $ FloweringSlope              :List of 4
#>  $ FruitWaterContentDecreaseMax:List of 4
#>  $ FruitWaterContentInc        :List of 4
#>  $ FruitWaterContentMax        :List of 4
#>  $ FruitWaterContentMin        :List of 4
#>  $ HalfIntGrowth               :List of 4
#>  $ HalfIntSenescence           :List of 4
#>  $ InitialInt                  :List of 4
#>  $ k0                          :List of 4
#>  $ KcIni                       :List of 4
#>  $ KcMax                       :List of 4
#>  $ RootDepthInitial            :List of 4
#>  $ RootDepthMax                :List of 4
#>  $ RootIncrease                :List of 4
#>  $ RUE                         :List of 4
#>  $ SoilWaterInitial            :List of 4
#>  $ Tbase                       :List of 4
#>  $ Tcold                       :List of 4
#>  $ Theat                       :List of 4
#>  $ Tmax                        :List of 4
#>  $ Topt                        :List of 4
#>  $ TransplantingLag            :List of 4
#>  $ WaterStressSensitivity      :List of 4
#>  $ WiltingPoint                :List of 4
cumbaParameters$Tbase
#> $description
#> [1] "Base temperature for growth and development"
#> 
#> $value
#> [1] 10
#> 
#> $min
#> [1] 6
#> 
#> $max
#> [1] 13
```

`cumbaParameters` is a *named list of lists* with fields `value`, `min`,
`max`, `description`. Both
[`cumba_experiment()`](https://GeoModelLab.github.io/cumba/reference/cumba_experiment.md)
and
[`cumba_scenario()`](https://GeoModelLab.github.io/cumba/reference/cumba_scenario.md)
accept this format directly and convert it internally into a tibble of
values.

## A bundled example dataset: `tomatoFoggia`

The package ships with a real-world dataset from processing-tomato field
trials in Foggia (Apulia, Italy):

``` r

data(tomatoFoggia)
names(tomatoFoggia)
#> [1] "weather"    "irrigation" "management" "production"
head(tomatoFoggia$weather)
#> # A tibble: 6 × 11
#>   DATE                   Tx    Tn     P   Rad   RHx   RHn      W Date      
#>   <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <date>    
#> 1 2005-04-28 00:00:00  27.7   7.2     0     0  91    32.5 0.0472 2005-04-28
#> 2 2005-04-29 00:00:00  23.6  10.4     0     0  91.5  35.5 0.106  2005-04-29
#> 3 2005-04-30 00:00:00  23.2   8.8     0     0  78.5  33.5 2.5    2005-04-30
#> 4 2005-05-01 00:00:00  27.6   7.4     0     0  88    25   1.6    2005-05-01
#> 5 2005-05-02 00:00:00  29.4   8.8     0     0  92    32   1.3    2005-05-02
#> 6 2005-05-03 00:00:00  32    10.8     0     0  89.5  30.5 0.0632 2005-05-03
#> # ℹ 2 more variables: Site <chr>, Lat <dbl>
head(tomatoFoggia$irrigation)
#> # A tibble: 6 × 9
#>      ID DATE                 WVOL  YEAR CV     TRANS              
#>   <dbl> <dttm>              <dbl> <dbl> <chr>  <dttm>             
#> 1     1 2005-04-28 00:00:00  27.9  2005 Ulisse 2005-04-28 00:00:00
#> 2     1 2005-05-05 00:00:00  20    2005 Ulisse 2005-04-28 00:00:00
#> 3     1 2005-05-10 00:00:00  10.5  2005 Ulisse 2005-04-28 00:00:00
#> 4     1 2005-05-17 00:00:00  10.5  2005 Ulisse 2005-04-28 00:00:00
#> 5     1 2005-05-24 00:00:00  15.8  2005 Ulisse 2005-04-28 00:00:00
#> 6     1 2005-05-30 00:00:00  17.8  2005 Ulisse 2005-04-28 00:00:00
#> # ℹ 3 more variables: HARV <dttm>, IRR_M <chr>, Site <chr>
head(tomatoFoggia$production)
#> # A tibble: 6 × 5
#>      ID yield_ref yield_ref_sd brix_ref brix_ref_sd
#>   <dbl>     <dbl>        <dbl>    <dbl>       <dbl>
#> 1     1      69.1         4.04     5.8        0.361
#> 2     2      87.3        11.1      5.2        0.300
#> 3     3      87.6         3.68     5          0.173
#> 4     4      81.0         4.90     5          0.200
#> 5     5      77.4         4.55     5.17       0.551
#> 6     6      89.4         9.22     5.3        0.100
```

## A first run

The minimal call uses the default parameters and the bundled weather and
irrigation data:

``` r

weather       <- tomatoFoggia$weather
irrigation_df <- tomatoFoggia$irrigation

out <- cumba_experiment(
  weather       = weather,
  param         = cumbaParameters,
  irrigation_df = irrigation_df,
  estimateRad   = TRUE,
  estimateET0   = TRUE,
  fullOut       = FALSE
)

head(out)
```

The output is a `data.frame` with one row per (site, year, experiment,
day-of-year). With `fullOut = FALSE` you get the *short* output:

| column       | meaning                    |
|--------------|----------------------------|
| `site`       | site name                  |
| `year`       | calendar year              |
| `experiment` | experiment id              |
| `doy`        | day of year                |
| `p`          | precipitation (mm)         |
| `irrigation` | irrigation (mm)            |
| `stage`      | phenological stage         |
| `swc`        | soil water content (%)     |
| `yield`      | fresh fruit yield (q ha-1) |
| `brix`       | actual Brix (°)            |

Set `fullOut = TRUE` to get every internal state variable (carbon,
flowering, fruit-water content, Kc, stresses, etc.).

## What’s next?

- See **Run an experiment**
  ([`vignette("cumba-experiment")`](https://GeoModelLab.github.io/cumba/articles/cumba-experiment.md))
  for a complete experiment-mode walkthrough on `tomatoFoggia`.
- See **Run a scenario**
  ([`vignette("cumba-scenario")`](https://GeoModelLab.github.io/cumba/articles/cumba-scenario.md))
  for the deficit-irrigation mode.
- See **Calibrate ET₀ and radiation**
  ([`vignette("estimate_et0_rad")`](https://GeoModelLab.github.io/cumba/articles/estimate_et0_rad.md))
  for the radiation/ET₀ workflow.
