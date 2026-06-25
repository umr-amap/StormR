# Compute the topographic exposure to wind

The `computeTEW()` function allows computing topographic exposure to
wind (TEW) for either:

1.  each cell of a regular grid (i.e., a raster). See
    [`computeTEW.SpatRaster`](computeTEW.SpatRaster.md) documentation.
    or

2.  a data.frame with the coordinates of the points of interest, for a
    given tropical cyclone or a set of tropical cyclones. See
    [`computeTEW.list`](computeTEW.list.md) documentation.

## Usage

``` r
computeTEW(profiles, ...)
```

## Arguments

- profiles:

  SpatRaster or list. Wind speed and direction profiles from
  [`spatialBehaviour()`](spatialBehaviour.md) or
  [`temporalBehaviour()`](temporalBehaviour.md)

- ...:

  additional arguments depending on the type of `profiles` input
