# Compute the topographic exposure to wind

The [`computeTEW()`](computeTEW.md) function allows computing
topographic exposure to wind (TEW) for each points registred in a
data.frame for a tropical cyclone or set of tropical cyclones.

## Usage

``` r
# S3 method for class 'list'
computeTEW(profiles, points, dtm, angle = 6, threshold = 0, verbose = 2, ...)
```

## Arguments

- profiles:

  list Wind speed and direction profiles from
  [`temporalBehaviour()`](temporalBehaviour.md) function which must
  respect the following terminology : 'speed', 'direction' columns in a
  data.frame

- points:

  data.frame. Consisting of two columns names as "x" (for the longitude)
  and "y" (for the latitude), providing the coordinates in decimal
  degrees of the point locations. Same as the "points" argument of the
  [`temporalBehaviour()`](temporalBehaviour.md) function

- dtm:

  SpatRaster of the elevation data (Digital Terrain Model) for a given
  location

- angle:

  numeric. Inflection angle of the wind (in degrees). default is 6°.

- threshold:

  numeric. Minimum wind speed threshold (in m/s) requirred to compute
  exposure. default is 0

- verbose:

  numeric. Whether or not the function should display information about
  the process and/or outputs. Can be:

  - `2`, information about the processes and outputs are displayed
    (default setting),

  - `1`, information about the processes are displayed,

  - `0`, no information displayed.

- ...:

  additional arguments (not used)

## Value

the function returns a list of data.frame with one additional column for
topographic exposure to wind (TEW) for each observation or interpolated
observation and each `Storm`.
