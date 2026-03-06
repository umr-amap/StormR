# `stormsList` object

Gather all the needed informations to model a set of storms

## Value

A `stormsList` object.

- `data`, list.

- `buffer`, numeric.

- `spatialLoi`, sf.

- `spatialLoiBuffer`, sf.

## Slots

- `data`:

  A list of `storm` (See `storm` class)

- `buffer`:

  numeric. Buffer used to extent `spatialLoi` (km)

- `spatialLoi`:

  sf object. Represents the location of interest. Projection is
  EPSG:4326

- `spatialLoiBuffer`:

  sf object. Buffer extension of `spatialLoi`

- `scale`:

  numeric. List of storm scale thresholds to use in all functions of the
  package. Default value is set to the Saffir Simpson Hurricane Scale

- `scalePalette`:

  character. Named vector containing the color hex code corresponding to
  each category in `scale` slot. Default value is the palette associated
  with the Saffir Simpson Hurricane Scale
