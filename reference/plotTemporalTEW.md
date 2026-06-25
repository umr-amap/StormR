# Plotting TEW time series at given point locations

Plotting TEW time series at given point locations

## Usage

``` r
plotTemporalTEW(data, storm)
```

## Arguments

- data:

  time series generated with `temporalBehaviour` with `product=TS` input
  first and then `computeTEW`

- storm:

  list of characters. Storm names. The storm must be available in `data`
  input. It can also be a vector of integer corresponding to the indices
  of storms stored in `data`input.

## Value

null
