# stormsDataset

Choose the database to use within the package's functions

## Details

The fields input must provide at least 6 mandatory fields (and at most
10) in order to benefit from all the functionalities of this package:

- A field `names`: which dimension contains the names of storms in the
  netcdf database

- A field `seasons`: which dimension contains the cyclonic seasons of
  storms in the netcdf database

- A field `isoTime`: which dimension contains the ISO times of each (3
  or 6 hourly) observations for all storms in the database

- A field `lon`: which dimension contains the longitude coordinates of
  each observations for all storms in the netcdf database

- A field `lat`: which dimension contains the latitude coordinates of
  each observations for all storms in the netcdf database

- A field `msw`: which dimension contains the maximum sustained wind
  speed of each observations for all storms in the netcdf database

The following fields are optional but highly recommended:

- A field `basin`: which dimension contains the basin location of storms
  in the netcdf database. Used to filter the storms in the netcdf
  database

- A field `rmw`: which dimension contains the radius of maximum wind
  speed of each observations for all storms in the netcdf database (See
  spatialBehaviour, temporalBehaviour)

Finally these following fields are optional but mandatory to perform
Holland model (See `spatialBehaviour`, `temporalBehaviour`)

- A field `pressure`: which dimension contains the pressure in the eye
  for of each observations for all storms in the netcdf database

- A field `poci`: which dimension contains the Pressure at the Outermost
  Closed Isobar for of each observations for all storms in the nectdf
  database

Default value is set according to the most relevant dimensions of
IBTrACS databases:
`fields = c(basin = "basin", names = "name", seasons = "season", isoTime = "iso_time", lon = "usa_lon", lat = "usa_lat", msw = "usa_wind", rmw = "usa_rmw", pressure = "usa_pres", poci = "usa_poci")`

## Slots

- `filename`:

  character. Name of the database to load. Must be either a netcdf or a
  csv file

- `fields`:

  named character vector. Dictionary that provides all the name of
  dimensions to extract from the netcdf database (See `Details`)

- `basin`:

  character. Basin name to filter the database within its boundaries. It
  must be either

  - `"NA"`: North Atlantic

  - `"SA"`: South Atlantic

  - `"EP"`: Eastern North Pacific

  - `"WP"`: Western North Pacific

  - `"SP"`: South Pacific

  - `"SI"`: South India

  - `"NI"`: North India

  - `"None"`: No particular basin

- `seasons`:

  numeric vector. Range of calendar years to filter storms. For cyclones
  that formed in one year and dissipated in the following year, the
  latter should be used

- `database`:

  list of 6 to 9 slots depending on the fields input. Each slot is
  either a 1D array of dimension (number of storms) for `names` and
  `seasons` fields, or a 2D array of dimension (Maximum number of
  observations:number of storms), for the remaining fields which are
  `isoTime`, `lon`, `lat`, `msw`, `rmw`, `pressure`, `poci`
