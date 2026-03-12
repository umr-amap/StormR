# Creating a `stormsList` object

The `defStormsList()` function extracts storm track data from a
`stormsDataset` and creates a `stormsList` object based on specified
arguments relating to location of interest, seasons, and names of the
storms.

## Usage

``` r
defStormsList(
  sds,
  loi,
  seasons = c(sds@seasons["min"], sds@seasons["max"]),
  names = NULL,
  maxDist = 300,
  scale = sshs,
  scalePalette = NULL,
  removeUnder = NULL,
  removeUnnamed = NULL,
  verbose = 2
)
```

## Arguments

- sds:

  `stormsDataset` object.

- loi:

  Location of interest. Can be defined using,

  - character, a country name (e.g., "Vanuatu")

  - character, a basin name among "NA", "SA", "EP", "WP", "SP", "SI" and
    "NI"

  - numeric vector, a point coordinate (lon, lat in decimal degrees,
    e.g., c(169.5, -19.2))

  - sp (SpatialPolygon) or a sf (simple features) object (e.g., created
    from a shapefile)

- seasons:

  numeric vector. Seasons of occurrence of the storms (e.g.,
  c(2020,2022)). In the Southern Hemisphere, the cyclone season extends
  across two consecutive years. Therefore, to capture the 2021 to 2022
  cyclone season both years should be specified, with cyclones assigned
  for the year that originated in. By default all storms from `sds` are
  extracted.

- names:

  character vector. Names of specific storms (in capital letters).

- maxDist:

  numeric. Maximum distance between the location of interest and the
  storm for which track data are extracted. Default `maxDist` is set to
  300 km.

- scale:

  numeric. List of storm scale thresholds used for the database. Default
  value is set to the Saffir Simpson Hurricane Scale

- scalePalette:

  character. Named vector containing the color hex code corresponding to
  each category interval of `scale` input

- removeUnder:

  numeric. Storms reaching this maximum level or less in the scale are
  removed. Default value is set to NULL.

- removeUnnamed:

  character vector. Remove unnamed storms. Need to specify how they are
  named in the stormsDataset (e.g "UNNAMED", "NOT_NAMED", etc...)
  Default value is set to NULL (do not remove any unnamed storm).

- verbose:

  numeric. Type of information the function displays. Can be:

  - `2`, information about both the processes and the outputs are
    displayed (default value),

  - `1`, only information about the processes are displayed, or

  - `0`, nothing is displayed.

## Value

The `defStormsList()` function returns a `stormsList` object containing
track data for all storms meeting the specified criteria (e.g., name,
season, location).

## Details

The available countries for the `loi` are those provided in the
`rwolrdxtra` package. This package provide high resolution vector
country boundaries derived from Natural Earth data. More informations on
the Natural Earth data here:
[http://www.naturalearthdata.com/downloads/10m-cultural-vectors/](https://www.naturalearthdata.com/downloads/10m-cultural-vectors/).

## References

Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann,
C. J. (2010). The International Best Track Archive for Climate
Stewardship (IBTrACS). Bulletin of the American Meteorological Society,
91(3), Article 3. <doi:10.1175/2009bams2755.1>

## Examples

``` r
# \donttest{
#Creating a stormsDataset
sds <- defStormsDataset()
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===

#Getting data using country names
vanuatu.st <- defStormsList(sds = sds, loi = "Vanuatu")
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching storms from 2015 to 2021 ...
#>    -> Identifying Storms: 9 potential candidates...
#> -> Gathering storm(s) ... 
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 5.255937 sec ===
#> 
#> SUMMARY:
#> (*) LOI: Vanuatu 
#> (*) Buffer size: 300 km
#> (*) Number of storms: 7 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         PAM - 2015 - 6 - 20 
#>         SOLO - 2015 - 1 - 4 
#>         ULA - 2016 - 5 - 12 
#>         WINSTON - 2016 - 6 - 22 
#>         ZENA - 2016 - 3 - 8 
#>         UESI - 2020 - 2 - 5 
#>         LUCAS - 2021 - 2 - 9 
#> 

#Getting data using a specific point location
pt <- c(169, -19)
pam.pt <- defStormsList(sds = sds, loi = pt, names = "PAM")
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching for PAM storm ...
#>    -> Identifying Storms: Done
#> -> Gathering storm(s) ... 
#> 
#> === DONE with run time 0.03484273 sec ===
#> 
#> SUMMARY:
#> (*) LOI: 169 -19 lon-lat
#> (*) Buffer size: 300 km
#> (*) Number of storms: 1 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         PAM - 2015 - 6 - 8 
#> 

#Getting data using country and storm names
niran.nc <- defStormsList(sds = sds, loi = "New Caledonia", names = c("NIRAN"))
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching for NIRAN storm ...
#>    -> Identifying Storms: Done
#> -> Gathering storm(s) ... 
#> 
#> === DONE with run time 0.4661763 sec ===
#> 
#> SUMMARY:
#> (*) LOI: New Caledonia 
#> (*) Buffer size: 300 km
#> (*) Number of storms: 1 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         NIRAN - 2021 - 6 - 10 
#> 

#Getting data using a user defined spatial polygon
poly <- cbind(c(135, 290, 290, 135, 135),c(-60, -60, 0, 0, -60))
sp <- sf::st_polygon(list(poly))
sp <- sf::st_sfc(sp, crs = 4326)
sp <- sf::st_as_sf(sp)
sts_sp <- defStormsList(sds = sds, loi = sp)
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching storms from 2015 to 2021 ...
#>    -> Identifying Storms: 9 potential candidates...
#> -> Gathering storm(s) ... 
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 0.1364515 sec ===
#> 
#> SUMMARY:
#> (*) LOI: sf object (use getLOI function for further informations
#> (*) Buffer size: 300 km
#> (*) Number of storms: 9 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         PAM - 2015 - 6 - 57 
#>         SOLO - 2015 - 1 - 29 
#>         ULA - 2016 - 5 - 119 
#>         WINSTON - 2016 - 6 - 151 
#>         ZENA - 2016 - 3 - 21 
#>         UESI - 2020 - 2 - 67 
#>         GRETEL - 2020 - 2 - 27 
#>         LUCAS - 2021 - 2 - 49 
#>         NIRAN - 2021 - 6 - 65 
#> 

# }
```
