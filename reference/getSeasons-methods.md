# Getting cyclonic seasons of the storms

The `getSeasons()` function returns the cyclonic season of each storm in
a `storm` or `stormsList` object.

## Usage

``` r
getSeasons(s)

# S4 method for class 'storm'
getSeasons(s)

# S4 method for class 'stormsList'
getSeasons(s)
```

## Arguments

- s:

  `storm` or `stormsList` object.

## Value

numeric vector.

## Examples

``` r
# \donttest{
#Creating a stormsDataset
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
#> === DONE with run time 0.5157099 sec ===
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

#Getting the cyclonic seasons of the storms from the sts object
getSeasons(sts)
#>    PAM   SOLO    ULA   UESI GRETEL  LUCAS  NIRAN 
#>   2015   2015   2016   2020   2020   2021   2021 
# }
```
