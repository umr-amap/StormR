# Creating a `stormsDataset` object

The `defStormsDataset()` function creates a `stormsDataset` object from
either a NetCDF or a CSV file. This is an essential first step before
other `stormR` functions can be used.

## Usage

``` r
defStormsDataset(
  filename = system.file("extdata", "test_dataset.nc", package = "StormR"),
  sep = NULL,
  fields = c(names = "name", seasons = "season", isoTime = "iso_time", lon = "usa_lon",
    lat = "usa_lat", msw = "usa_wind", basin = "basin", rmw = "usa_rmw", pressure =
    "usa_pres", poci = "usa_poci"),
  basin = NULL,
  seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
  unitConversion = c(msw = "knt2ms", rmw = "nm2km", pressure = "mb2pa", poci = "mb2pa"),
  notNamed = "NOT_NAMED",
  verbose = 1
)
```

## Arguments

- filename:

  character. Name of the NetCDF (.nc)/CSV (.csv) file. Default is the
  `test_dataset.nc` file located in the `inst/extdata` repository of the
  directory (accessible by
  `system.file("extdata", "test_dataset.nc", package = "StormR")`). This
  test dataset is extracted from the IBTrACS.SP.v04r00.nc file and
  provides all the tropical cyclones that occurred around Vanuatu from
  2015 to 2016 and around New Caledonia from 2020 to 2021.

- sep:

  character. The field separator character if `filename` is a CSV file.
  Default value is set to `NULL` which will set the separator to `","`.

- fields:

  named character vector. This argument allows to specify the
  corresponding variable names (NetCDF) or headers (CSV) in the input
  file for each field in the output `stormsDataset`. By default, the
  corresponding variable names are set up to import data from a NetCDF
  file from the IBTrACS database (Knapp et al., 2010). Corresponding
  variable names for following fields have to (mandatory fields) or can
  be (recommended or optional fields) provided:

  - `"names"`, names of the storms (mandatory),

  - `"seasons"`, years of observations (mandatory),

  - `"isoTime"`, date and time of observations (mandatory),

  - `"lon"`, longitude of the observations (mandatory),

  - `"lat"`, latitude of the observations (mandatory),

  - `"msw"`, maximum sustained wind speed (mandatory),

  - `"basin"`, name of the area where the storm originated
    (recommended),

  - `"rmw"`, radius of maximum winds: distance between the centre of the
    storm and its band of strongest winds (recommended),

  - `"pressure"`, central pressure (recommended),

  - `"poci"`, pressure of the last closed isobar (recommended)

- basin:

  character. If the basin field is provided, then storm track data will
  only be extracted for the named basin. By default `basin=NULL`,
  meaning that all storms irrespective of the basin they originated in
  are extracted. Seven basins can be used to filter the data set:

  - `"NA"`, for North Atlantic basin,

  - `"SA"`, for South Atlantic basin,

  - `"EP"`, for Eastern North Pacific basin,

  - `"WP"`, for Western North Pacific basin,

  - `"SP"`, for South Pacific basin,

  - `"SI"`, for South India basin, or

  - `"NI"`, for North India basin.

- seasons:

  numeric vector. Seasons of occurrence of the storms (e.g.,
  c(2020,2022)). In the Southern Hemisphere, the cyclone season extends
  across two consecutive years. Therefore, to capture the 2021 to 2022
  cyclone season both years should be specified, with cyclones assigned
  for the year that originated in. By default all storms occurring since
  1980 are extracted.

- unitConversion:

  named character vector. `StormR` functions use the metric system
  (international system of units), therefore `msw` has to be provided in
  \\m.s^{-1}\\, `rmw` in \\km\\, `pressure` and `poci` in \\Pa\\. By
  default
  `unitConversion=c(msw = "knt2ms", rmw = "nm2km", pressure = "mb2pa", poci = "mb2pa")`
  to meet the requirements when importing a NetCDF file from the IBTrACS
  database. This argument is mandatory even if no conversion is needed.
  If no conversion is needed then use `"None"` in the corresponding
  fields. The following unit conversions are implemented:

  For `msw`,

  - `"knt2ms"`, to convert knot to meter per second (default setting),

  - `"kmh2ms"`, to convert kilometre per hour to meter per second,

  - "`mph2ms"`, to convert miles per hour to meter per second, or

  - `"None"`, if no conversion is needed.

  For `rmw`,

  - `"nm_to_ms"`to convert nautical miles to kilometre (default
    setting), or

  - `"None"`if no conversion is needed.

  For `pressure` and `poci`,

  - "`mb2pa"`, to convert millibar to Pascal (default setting),

  - `"b2pa"`, to convert bar to Pascal,

  - `"atm2pa"`, to convert atmosphere to Pascal,

  - `"psi2pa"`, to convert psi to Pascal, or

  - `"None"`, if no conversion is needed.

- notNamed:

  character. Constant name for not named storms to remove in the
  database. Default value is "NOT_NAMED" (IBTrACS database)

- verbose:

  numeric. Whether the function should display (`= 1`) or not (`= 0`)
  information about the processes.

## Value

The `defStormsDataset()` function returns a `stormsDataset` object.

## References

Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann,
C. J. (2010). The International Best Track Archive for Climate
Stewardship (IBTrACS). Bulletin of the American Meteorological Society,
91(3), Article 3. https://doi.org/10.1175/2009bams2755.1

## Examples

``` r
# Creating a `stormsDataset` object with storms between 2010 and 2015
# in the South Pacific using the NetCDF provided with the package
SP_2015_2020_nc <- defStormsDataset(seasons = c(2010, 2015))
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===
str(SP_2015_2020_nc)
#> Formal class 'stormsDataset' [package "StormR"] with 5 slots
#>   ..@ filename: chr "/home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc"
#>   ..@ fields  : Named chr [1:10] "name" "season" "iso_time" "usa_lon" ...
#>   .. ..- attr(*, "names")= chr [1:10] "names" "seasons" "isoTime" "lon" ...
#>   ..@ basin   : chr "None"
#>   ..@ seasons : Named num [1:2] 2015 2015
#>   .. ..- attr(*, "names")= chr [1:2] "min" "max"
#>   ..@ database:List of 9
#>   .. ..$ names    : chr [1:2(1d)] "PAM" "SOLO"
#>   .. ..$ seasons  : num [1:2(1d)] 2015 2015
#>   .. ..$ isotimes : chr [1:360, 1:2] "2015-03-07 06:00:00" "2015-03-07 09:00:00" "2015-03-07 12:00:00" "2015-03-07 15:00:00" ...
#>   .. ..$ longitude: num [1:360, 1:2] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ latitude : num [1:360, 1:2] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ msw      : num [1:360, 1:2] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ rmw      : num [1:360, 1:2] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ pressure : num [1:360, 1:2] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ poci     : num [1:360, 1:2] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...

# Creating a `stormsDataset` object with storms between 2010 and 2015
# in the South Pacific using the CSV provided with the package
fileName <- system.file("extdata", "test_dataset.csv", package = "StormR")
SP_2015_2020_csv <- defStormsDataset(seasons = c(2010, 2021))
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===
str(SP_2015_2020_csv)
#> Formal class 'stormsDataset' [package "StormR"] with 5 slots
#>   ..@ filename: chr "/home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc"
#>   ..@ fields  : Named chr [1:10] "name" "season" "iso_time" "usa_lon" ...
#>   .. ..- attr(*, "names")= chr [1:10] "names" "seasons" "isoTime" "lon" ...
#>   ..@ basin   : chr "None"
#>   ..@ seasons : Named num [1:2] 2015 2021
#>   .. ..- attr(*, "names")= chr [1:2] "min" "max"
#>   ..@ database:List of 9
#>   .. ..$ names    : chr [1:9(1d)] "PAM" "SOLO" "ULA" "WINSTON" ...
#>   .. ..$ seasons  : num [1:9(1d)] 2015 2015 2016 2016 2016 ...
#>   .. ..$ isotimes : chr [1:360, 1:9] "2015-03-07 06:00:00" "2015-03-07 09:00:00" "2015-03-07 12:00:00" "2015-03-07 15:00:00" ...
#>   .. ..$ longitude: num [1:360, 1:9] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ latitude : num [1:360, 1:9] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ msw      : num [1:360, 1:9] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ rmw      : num [1:360, 1:9] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ pressure : num [1:360, 1:9] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>   .. ..$ poci     : num [1:360, 1:9] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
```
