# `storm` object

Gather all the needed informations to model a single storm

## Value

A `storm` object.

- `name`, character.

- `season`, numeric.

- `scale`, numeric.

- `obs`, numeric.

- `obs.all`, data.frame.

## Slots

- `name`:

  character. Name of the storm

- `season`:

  numeric. Cyclonic season in which the storm has occured

- `scale`:

  numeric. Maximum scale category reached

- `obs`:

  numeric vector. Indices of observations within the location of
  interest extented with its corresponding buffer (See `stormsList`
  class)

- `obs.all`:

  data.frame. Contains all of the observations available. An observation
  is made up of several fields which are:

  - `iso.time`, Date and hours of observations (UTC)

  - `lon`, Longitude coordinate (Eastern degree)

  - `lat`, Latitude coordinate (Northern degree)

  - `msw`, Maximum Sustained Wind (m/s)

  - `scale`, Level in the chosen scale

  The following field is not mandatory but highly recommended

  - `rmw`, Radius of Maximum Wind (km)

  Also, the following fields are only mandatory to perform Holland and
  Boose models (See `Details`)

  - `pres`, Pressure at the center (pa)

  - `poci`, Pressure of the Outermost Closed Isobar (pa)
