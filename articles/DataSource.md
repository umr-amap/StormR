# Data source

Before running `StormR` functions users have to provide a tropical storm
track dataset either as a “.nc” (NetCDF) or a “.csv” (CSV) file in which
the location and some characteristics of storms are given across their
lifespan. Using a CSV file as a dataset implies following few specific
formatting rules, please refer to the [CSV Formatting](#csv-formatting)
section below. These files are then used to create a `stormsDataset`
object using the
[`defStormsDataset()`](../reference/defStormsDataset.md) function. By
default, the arguments of
[`defStormsDataset()`](../reference/defStormsDataset.md) function are
set up to create a `stormsDataset` object using the USA fields in the
NetCDF IBTrACS database [International Best Track Archive for Climate
Stewardship](https://www.ncei.noaa.gov/products/international-best-track-archive)
(Knapp *et al.*, (2010), (2018)). This database provides a fairly
comprehensive record of worldwide tropical storms and cyclones with a
3-hours temporal resolution since 1. Other databases can be used as long
as the following fields are provided:

[TABLE]

$^{1}$ Providing the `basin` allows to filter storms in a particular
basin using the `basin` argument of the
[`defStormsDataset()`](../reference/defStormsDataset.md) function.  
$^{2}$ Providing `rmw` allows better estimates of wind speed and
direction in the `temporatlBehaviour()` and
[`spatialBehaviour()`](../reference/spatialBehaviour.md) functions. If
not provided, `rmw` is approximated using an empirical formula derived
from Willoughby *et al.* (2006) as follows,
$R_{m} = 46.4e^{( - 0.0155 \times v_{m} + 0.0169 \times {|\phi|})}$.  
$^{3}$ If the `pressure` and `poci` fields are not provided, both
Holland (1980) and Boose *et al.* (2004) wind field models cannot be
used in the `temporatlBehaviour()` and
[`spatialBehaviour()`](../reference/spatialBehaviour.md) functions.  
$^{4}$ The `sshs` field is optional, if not provided this field is
automatically filled using the `msw` field.

### Downloading a data source

If not available, a storm track data set has to be downloaded. In the
following example we illustrate how a storm track data set can be
downloaded on the IBTrACKS website. Different “.nc” (NetCDF) files
containing storm track data sets can be downloaded
[here](https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/netcdf/).
Please, follow the IBTrACS citation recommendations when using these
datasets (including citing Knapp *et al.* (2010, 2018), and providing
the access date for the later). For example, track data for all the
storms that occurred over the last three years over the different basins
world wide can be download to the working directory as follows,

``` r
download.file(url = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/netcdf/IBTrACS.last3years.v04r00.nc", destfile = "./IBTrACS_ALL_3yrs.nc")
```

### Creating a `stormsDataset`

Once a storm track data set is available a `stormsDataset` object can be
created using the
[`defStormsDataset()`](../reference/defStormsDataset.md) function as
follows,

``` r
sds <- defStormsDataset(filename = "./IBTrACS_ALL_3yrs.nc", basin = NULL, verbose = 0)
```

    ## Warning: No basin argument specified. StormR will work as expected
    ##              but cannot use basin filtering for speed-up when collecting data

``` r
str(sds)
```

    ## Formal class 'stormsDataset' [package "StormR"] with 5 slots
    ##   ..@ filename: chr "./IBTrACS_ALL_3yrs.nc"
    ##   ..@ fields  : Named chr [1:10] "name" "season" "iso_time" "usa_lon" ...
    ##   .. ..- attr(*, "names")= chr [1:10] "names" "seasons" "isoTime" "lon" ...
    ##   ..@ basin   : chr "None"
    ##   ..@ seasons : Named int [1:2] 2021 2024
    ##   .. ..- attr(*, "names")= chr [1:2] "min" "max"
    ##   ..@ database:List of 9
    ##   .. ..$ names    : chr [1:266(1d)] "IMOGEN" "ELOISE" "JOSHUA" "KIMI" ...
    ##   .. ..$ seasons  : int [1:266(1d)] 2021 2021 2021 2021 2021 2021 2021 2021 2021 2021 ...
    ##   .. ..$ isotimes : chr [1:360, 1:266] "2021-01-01 00:00:00" "2021-01-01 03:00:00" "2021-01-01 06:00:00" "2021-01-01 09:00:00" ...
    ##   .. ..$ longitude: num [1:360, 1:266] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ latitude : num [1:360, 1:266] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ msw      : num [1:360, 1:266] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ rmw      : num [1:360, 1:266] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ pressure : num [1:360, 1:266] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ poci     : num [1:360, 1:266] NA NA NA NA NA NA NA NA NA NA ...

``` r
length(unique(paste(sds@database$names, sds@database$seasons, " ")))
```

    ## [1] 257

This `stormsDataset` object contains track data for 275 storms. When
using an IBTrACKS storm track data set, default arguments can be used.
By default, data provided by USA agencies (starting with “usa\_”) are
used, but other data can be selected. For example, for the South Pacific
basin (“SP”), we can choose to use data provided by the Australian
Bureau of Meteorology (starting with “bom\_”) in the IBTrACKS data set
as follows,

``` r
sds <- defStormsDataset(
  filename = "IBTrACS_ALL_3yrs.nc",
  fields = c(
    names = "name",
    seasons = "season",
    isoTime = "iso_time",
    basin = "basin",
    lon = "bom_lon",
    lat = "bom_lat",
    msw = "bom_wind",
    rmw = "bom_rmw",
    pressure = "bom_pres",
    poci = "bom_poci"
  ),
  basin = "SP",
  verbose = 0
)
length(sds@database$names)
```

    ## [1] 30

This `stormsDataset` object contains track data for 38 storms in the
South Pacific basin.

### Unit conversion

In the IBTrACS data sets speeds are given in knots ($knt$), distances in
nautical miles ($nm$), and pressure in millibar ($mb$) but the models
implemented in the
[`temporalBehaviour()`](../reference/temporalBehaviour.md) and
[`spatialBehaviour()`](../reference/spatialBehaviour.md) functions
compute wind speed, wind direction, and summary statistics following the
international system of units (SI) or SI-derived units. Therefore, by
default, when using IBTrACS data sets, the
[`defStormsDataset()`](../reference/defStormsDataset.md) function
converts knots ($knt$) to meter per second ($m.s^{- 1}$), nautical miles
($nm$) to kilometres ($km$), and millibar ($mb$) to Pascal ($pa$).

When using another source of data than IBTrACS, the `unitConversion`
argument of the [`defStormsDataset()`](../reference/defStormsDataset.md)
function can be used to convert the data to the desired units. The
following conversions are available,

For `msw`:  
`knt2ms`: converts knot in meter per second (default setting)  
`kmh2ms`: converts kilometre per hour in meter per second  
`mph2ms`: converts miles per hour in meter per second  
`None`: no conversion  

For `rmw`:  
`nm2ms`: converts nautical miles in kilometer (default setting)  
`None`: no conversion  

For both `pressure` and `poci`:  
`mb2pa`: converts millibar in Pascal (default setting)  
`b2pa`: converts bar in Pascal  
`atm2pa`: converts atmosphere in Pascal  
`psi2pa`: converts psi in Pascal  
`None`: no conversion  

### Test data sets

A `test_dataset` is provided with the `StormR` package in two formats (a
NetCDF and a CSV file). This test data set comprises the track data of
nine storms that occurred near Vanuatu and New Caledonia between
2015-2016 and 2020-2021, respectively.

### CSV Formatting

In order to load and read a CSV file in
[`defStormsDataset()`](../reference/defStormsDataset.md), it must stick
to the following rules:  
\* As for the NetCDF file, the CSV file must contains at least a name,
season, iso times, longitude, latitude,and a maximum wind speed column.
\* Every observation (row) in the CSV must be sorted in increasing order
according to the ISO times AND joined by storm. For example, if several
storms happened during the same iso Times, the observations for the
storm that first appeared should be stacked before the observations of
the second one and so on.

In addition to these mandatory rules above, the default separator is set
to a single comma ‘,’. Nevertheless the user might decide to change it
using the `sep` parameter in
[`defStormsDataset()`](../reference/defStormsDataset.md).

Hereafter, an example where we access the data with `test_dataset.csv`.
Notice that the header of the csv and the names of columns in `fields`
input must be equals

``` r
# Header of the csv
head(read.csv(system.file("extdata", "test_dataset.csv", package = "StormR")))
```

    ##      name season              iso_time  usa_lon   usa_lat usa_wind usa_sshs
    ## 1 'LUCAS'   2021 '2021-01-29 00:00:00' 144.1000 -14.10000       20       -3
    ## 2 'LUCAS'   2021 '2021-01-29 03:00:00' 144.4675 -13.90499       20       -3
    ## 3 'LUCAS'   2021 '2021-01-29 06:00:00' 144.9000 -13.70000       20       -3
    ## 4 'LUCAS'   2021 '2021-01-29 09:00:00' 145.4351 -13.48506       22       -3
    ## 5 'LUCAS'   2021 '2021-01-29 12:00:00' 146.0000 -13.30000       25       -1
    ## 6 'LUCAS'   2021 '2021-01-29 15:00:00' 146.5149 -13.16990       25       -1
    ##   usa_rmw usa_pres usa_poci
    ## 1      35      999     1001
    ## 2      35      999     1001
    ## 3      35      999     1001
    ## 4      40     1000     1002
    ## 5      45     1002     1004
    ## 6      45     1000     1004

``` r
# Is already the default setting (in this particular case)
fields <- c(
  names = "name",
  seasons = "season",
  isoTime = "iso_time",
  lon = "usa_lon",
  lat = "usa_lat",
  msw = "usa_wind",
  sshs = "usa_sshs",
  rmw = "usa_rmw",
  pressure = "usa_pres",
  poci = "usa_poci"
)
SP_2015_2020_csv <- defStormsDataset(fields = fields, seasons = c(2015, 2020))
```

    ## === Loading data  ===
    ## Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
    ## Collecting data ...
    ## === DONE ===

## Reference

Boose, E. R., Mayra I. Serrano, and David R. Foster. 2004. “Landscape
and Regional Impacts of Hurricanes in Puerto Rico.” *Ecological
Monographs* 74 (2): 335–52. <https://doi.org/10.1890/02-4057>.

Holland, Greg J. 1980. “An Analytic Model of the Wind and Pressure
Profiles in Hurricanes.” *Monthly Weather Review* 108 (8): 1212–18.
<https://doi.org/10.1175/1520-0493(1980)108%3C1212:AAMOTW%3E2.0.CO;2>.

Knapp, Kenneth R., Howard J. Diamond, James P. Kossin, Michael C. Kruk,
and Carl J. III Schreck. 2018. “International Best Track Archive for
Climate Stewardship (IBTrACS) Project, Version 4.”
<https://doi.org/10.25921/82ty-9e16>.

Knapp, Kenneth R., Michael C. Kruk, David H. Levinson, Howard J.
Diamond, and Charles J. Neumann. 2010. “The International Best Track
Archive for Climate Stewardship (IBTrACS): Unifying Tropical Cyclone
Data.” *Bulletin of the American Meteorological Society* 91 (3): 363–76.
<https://doi.org/10.1175/2009BAMS2755.1>.

Willoughby, H. E., R. W. R. Darling, and M. E. Rahn. 2006. “Parametric
Representation of the Primary Hurricane Vortex. Part II: A New Family of
Sectionally Continuous Profiles.” *Monthly Weather Review* 134 (4):
1102–20. <https://doi.org/10.1175/MWR3106.1>.
