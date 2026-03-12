# Plotting storm track data

This `plotStorms()` function allows plotting storm track data stored in
a `StormsList` object.

## Usage

``` r
plotStorms(
  sts,
  names = NULL,
  category = NULL,
  xlim = NULL,
  ylim = NULL,
  labels = FALSE,
  by = 8,
  pos = 3,
  legends = "topright",
  loi = TRUE,
  dynamicPlot = FALSE
)
```

## Arguments

- sts:

  `StormsList` object

- names:

  character vector. Name(s) of the storm(s) in capital letters. If
  `names = NULL` (default setting), all storms are plotted.

- category:

  numeric vector. Category of storms to be plotted among the level in
  the windscale provided in `sts` input. If `category=NULL` (default
  setting), all storms are plotted.

- xlim:

  numeric vector. The x limits of the plot.

- ylim:

  numeric vector. The y limits of the plot.

- labels:

  logical. Whether (TRUE) or not (FALSE, default setting) to add labels
  with the name of the storm and the indices and ISO times of the
  observation.

- by:

  numeric. If `labels=TRUE`, defines the frequency at which labels are
  plotted. Default value is set to `8` which corresponds to a 24h (or
  48h) time interval between the labelled observations when observations
  are made every 3 (or 6) hours.

- pos:

  numeric. If `labels=TRUE`, defines the position of the labels, `1`
  (above the observation), `2` (on the left), `3` (below, default
  setting), and `4` (on the right).

- legends:

  character. Indicates where to plot the legend, `"topright"`(default
  setting), `"topleft"`, `"bottomleft"`, `"bottomright"`, or `"none"`
  (legend not plotted).

- loi:

  logical. Whether (TRUE, default setting) or not (FALSE) to plot the
  extent of the buffered location of interest on the map.

- dynamicPlot:

  logical. Whether (FALSE, default setting) or (TRUE) to plot the data
  dynamicaly using leaflet library

## Value

A plot of the storm track data.

## Examples

``` r
#' #Creating a stormsDataset
# \donttest{
dev.off()
#> null device 
#>           1 
sds <- defStormsDataset()
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===

# Getting storm track data for tropical cyclone Pam (2015)
pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching for PAM storm ...
#>    -> Identifying Storms: Done
#> -> Gathering storm(s) ... 
#> 
#> === DONE with run time 0.4620771 sec ===
#> 
#> SUMMARY:
#> (*) LOI: Vanuatu 
#> (*) Buffer size: 300 km
#> (*) Number of storms: 1 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         PAM - 2015 - 6 - 20 
#> 

# Plotting Pam over Vanuatu with labels every 24h
plotStorms(sts =pam, labels = TRUE)

# Plotting Pam over Vanuatu with labels every 6h on the right side of the observations
plotStorms(pam, labels = TRUE, by = 2, pos = 4)

# dynamicPlot mode

# }
```
