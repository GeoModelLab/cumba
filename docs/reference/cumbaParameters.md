# Model Parameters for Cumba

This dataset contains the default model parameters used in the Cumba
package. Each element corresponds to a model parameter and includes its
description, current value, minimum, and maximum.

## Usage

``` r
cumbaParameters
```

## Format

A named list where each entry is a list with:

- description:

  Text description of the parameter

- value:

  The default value of the parameter

- min:

  The minimum allowed value

- max:

  The maximum allowed value

## Examples

``` r
names(cumbaParameters)
#>  [1] "CycleLength"                  "DepletionFraction"           
#>  [3] "FieldCapacity"                "FIntMax"                     
#>  [5] "FloweringLag"                 "FloweringMax"                
#>  [7] "FloweringSlope"               "FruitWaterContentDecreaseMax"
#>  [9] "FruitWaterContentInc"         "FruitWaterContentMax"        
#> [11] "FruitWaterContentMin"         "HalfIntGrowth"               
#> [13] "HalfIntSenescence"            "InitialInt"                  
#> [15] "k0"                           "KcIni"                       
#> [17] "KcMax"                        "RootDepthInitial"            
#> [19] "RootDepthMax"                 "RootIncrease"                
#> [21] "RUE"                          "SoilWaterInitial"            
#> [23] "Tbase"                        "Tcold"                       
#> [25] "Theat"                        "Tmax"                        
#> [27] "Topt"                         "TransplantingLag"            
#> [29] "WaterStressSensitivity"       "WiltingPoint"                
cumbaParameters$Tbase$value
#> [1] 10

```
