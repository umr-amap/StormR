# Getting the buffer size

The `getBufferSize()` returns the buffer size used to generate the
buffered location of interest for a `stormsList` object.

## Usage

``` r
getBufferSize(sts)

# S4 method for class 'stormsList'
getBufferSize(sts)
```

## Arguments

- sts:

  `stormsList`

## Value

numeric.

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
#> === DONE with run time 0.5200868 sec ===
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

#Getting the buffer size from the sts object
buffsize <- getBufferSize(sts)
# }
```
