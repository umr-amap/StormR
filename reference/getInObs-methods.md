# Getting the number of the observations

The `getInObs()` function returns the number of the observations in a
given `storm` or `stormsList` object.

## Usage

``` r
getInObs(s, ...)

# S4 method for class 'stormsList'
getInObs(s, name, season = NULL)

# S4 method for class 'storm'
getInObs(s)
```

## Arguments

- s:

  `storm` or `stormsList` object.

- ...:

  extra argument for `stormsList`

- name:

  character. Name of the storm in capital letters.

- season:

  numeric. Cyclonic season of the `storm`. Required only if several
  `storm` in `s` object have the same name. Default value is set to
  `NULL`

## Value

Numeric vector.

## Examples

``` r
#Creating a stormsDataset
# \donttest{
sds <- defStormsDataset()
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===

#Getting storm track data for all storms near New Caledonia
sts <- defStormsList(sds=sds, loi = "New Caledonia")
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching storms from 2015 to 2021 ...
#>    -> Identifying Storms: 9 potential candidates...
#> -> Gathering storm(s) ... 
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 0.5165195 sec ===
#> 
#> SUMMARY:
#> (*) LOI: New Caledonia 
#> (*) Buffer size: 300 km
#> (*) Number of storms: 7 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         PAM - 2015 - 6 - 5 
#>         SOLO - 2015 - 1 - 15 
#>         ULA - 2016 - 5 - 9 
#>         UESI - 2020 - 2 - 16 
#>         GRETEL - 2020 - 2 - 11 
#>         LUCAS - 2021 - 2 - 16 
#>         NIRAN - 2021 - 6 - 10 
#> 

#Getting the number of the observation for the tropical cyclone Niran in the sts object
getInObs(getStorm(sts, name = "NIRAN"))
#>  [1] 50 51 52 53 54 55 56 57 58 59
getInObs(sts, name = "NIRAN")
#>  [1] 50 51 52 53 54 55 56 57 58 59
# }
```
