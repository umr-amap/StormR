# Compute the topographic exposure to wind

The [`computeTEW()`](computeTEW.md) function allows computing
topographic exposure to wind (TEW) for each cell of a raster grid for a
tropical cyclone or set of tropical cyclones.

## Usage

``` r
# S3 method for class 'SpatRaster'
computeTEW(
  profiles,
  sts,
  dtm,
  angle = 6,
  threshold = 0,
  product = "TEWIntegrated",
  verbose = 2,
  ...
)
```

## Arguments

- profiles:

  SpatRaster. Wind speed and direction profiles from
  [`spatialBehaviour()`](spatialBehaviour.md) function which must
  respect the following terminology : 'STORM_Speed_N',
  'STORM_Direction_N'

- sts:

  StormsList object

- dtm:

  SpatRaster of the elevation data (Digital Terrain Model) for a given
  location

- angle:

  numeric. Inflection angle of the wind (in degrees). default is 6°.

- threshold:

  numeric. Minimum wind speed threshold (in m/s) requirred to compute
  exposure. default is 0

- product:

  character vector. Desired output statistics:

  - `"TEW1wd"`, TEW computation at each observation, one wind direction
    taken where wind speed is maximal. Good if your loi is small as wind
    is considered spatially homogeneous

  - `"TEW1wdMax"`, computes `"TEW1wd"`, then takes the maximum of the
    layers

  - `"TEW1wdMean"`, computes `"TEW1wd"`, then takes the mean of the
    layers

  - `"TEW"`, TEW computation at each observation, wind direction varies
    spatially

  - `"TEWIntegrated"`, TEW computation for maximum wind speed over the
    whole storm track for each spatial location of the raster. Use wind
    direction associated with the maximal wind speed (default)

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

the function returns one layer for topographic exposure to wind (TEW)
for each observation or interpolated observation and each `Storm`. The
names of the layer follow the following terminology, the name of the
storm in capital letters, "TEW", and the indices of the observation
separated by underscores (e.g., "PAM_TEW_41", ...)
