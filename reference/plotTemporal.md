# Plotting wind behaviour time series and summary statistics at given point locations

Plotting wind behaviour time series and summary statistics at given
point locations

## Usage

``` r
plotTemporal(data, storm, var = "speed")
```

## Arguments

- data:

  time series generated with `temporalBehaviour` with `product=TS` input

- storm:

  list of characters. Storm names. The storm must be available in `data`
  input. It can also be a vector of integer corresponding to the indices
  of storms stored in `data`input.

- var:

  character. Represent the type of variable to plot. Must be either
  `speed` or `direction`. Default value is set to `speed`.

## Value

null

## Examples

``` r
# \donttest{
sds <- defStormsDataset()
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===
st <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)

df <- data.frame(x = c(168.33, 167.17), y = c(-17.73, -15.53))
rownames(df) <- c("Port_Vila", "Luganville")

# Generate temporal series of wind on the points
TS <- temporalBehaviour(st, points = df, product = "TS", tempRes = 30, verbose = 0)

# Plot temporal series of wind speed
plotTemporal(data=TS, storm="PAM")


# Plot temporal series of wind direction
plotTemporal(data=TS, storm="PAM", var='direction')

# }
```
