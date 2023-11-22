




###########################
# Unit conversion functions#
###########################

# For msw
knt2ms <- function(x) {
  return(x * knt2msC)
}

mph2ms <- function(x) {
  return(x * mph2msC)
}

kmh2ms <- function(x) {
  return(x * kmh2msC)
}

# For rmw
nm2km <- function(x) {
  return(x * nm2kmC)
}

# For pressure
b2pa <- function(x) {
  return(x * b2paC)
}

mb2pa <- function(x) {
  return(x * mb2paC)
}

psi2pa <- function(x) {
  return(x * psi2paC)
}

atm2pa <- function(x) {
  return(x * atm2paC)
}





#' stormsDataset
#'
#' Choose the database to use within the package's functions
#'
#' @slot filename character. Name of the database to load. Must be a netcdf file
#' @slot fields named character vector. Dictionary that provides all the name of
#' dimensions to extract from the netcdf database (See `Details`)
#' @slot basin character. Basin name to filter the database within its
#' boundaries. It must be either
#' \itemize{
#'   \item `"NA"`: North Atlantic
#'   \item `"SA"`: South Atlantic
#'   \item `"EP"`: Eastern North Pacific
#'   \item `"WP"`: Western North Pacific
#'   \item `"SP"`: South Pacific
#'   \item `"SI"`: South India
#'   \item `"NI"`: North India
#'   \item `"None"`: No particular basin
#' }
#' @slot seasons numeric vector. Range of calendar years to filter storms. For
#'   cyclones that formed in one year and dissipated in the following year, the
#'   latter should be used
#' @slot database list of 6 to 10 slots depending on the fields input. Each slot
#' is either a 1D array of dimension (number of storms) for `names` and
#' `seasons` fields, or a 2D array of dimension
#' (Maximum number of observations:number of storms), for the remaining fields
#' which are `isoTime`, `lon`, `lat`, `msw`, `rmw`, `pressure`, `poci`, `sshs`
#'
#' @details
#' The fields input must provide at least 6 mandatory fields (and at most 11) in
#' order to benefit from all the functionalities of this package:
#' \itemize{
#'   \item A field `names`: which dimension contains the names of storms
#'         in the netcdf database
#'   \item A field `seasons`: which dimension contains the cyclonic
#'         seasons of storms in the netcdf database
#'   \item A field `isoTime`: which dimension contains the ISO times of
#'         each (3 or 6 hourly) observations for all storms in the database
#'   \item A field `lon`: which dimension contains the longitude
#'         coordinates of each observations for all storms in
#'         the netcdf database
#'   \item A field `lat`: which dimension contains the latitude
#'         coordinates of each observations for all storms in
#'         the netcdf database
#'   \item A field `msw`: which dimension contains the maximum sustained
#'         wind speed of each observations for all storms in the netcdf
#'         database
#' }
#' The following fields are optional but highly recommended:
#' \itemize{
#' \item A field `basin`: which dimension contains the basin location of
#'        storms in the netcdf database. Used to filter the storms in the netcdf
#'        database
#'  \item A field `rmw`: which dimension contains the radius of maximum
#'        wind speed of each observations for all storms in the netcdf
#'        database (See spatialBehaviour, temporalBehaviour)
#'  \item A field `sshs`: which dimension contains the Saffir Simpson
#'        Hurricane Scale index of each observations for all storms in the
#'        netcdf database
#' }
#' Finally these following fields are optional but mandatory to perform Holland
#' model (See `spatialBehaviour`, `temporalBehaviour`)
#' \itemize{
#'   \item A field `pressure`: which dimension contains the pressure
#'         in the eye for of each observations for all storms in the netcdf
#'         database
#'   \item A field `poci`: which dimension contains the Pressure at the
#'         Outermost Closed Isobar for of each observations for all storms
#'         in the nectdf database
#' }
#'
#' Default value is set according to the most relevant dimensions of IBTrACS
#' databases:
#' `fields = c(basin = "basin", names = "name", seasons = "season", isoTime = "iso_time",
#' lon = "usa_lon", lat = "usa_lat", msw = "usa_wind", rmw = "usa_rmw", pressure = "usa_pres",
#' poci = "usa_poci", sshs = "usa_sshs")`
#'
#' @export
stormsDataset <- methods::setClass(
  "stormsDataset",
  slots = c(
    filename = "character",
    fields = "character",
    basin = "character",
    seasons = "numeric",
    database = "list"
  )
)





#' check inputs for defStormsDataset function
#'
#' @noRd
#' @param filename character
#' @param fields character vector
#' @param basin character
#' @param seasons numeric vector
#' @param unitConversion character vector
#' @param verbose numeric
#'
#' @return NULL
checkInputsdefStormsDataset <- function(filename, fields, basin, seasons, unitConversion, verbose) {
  # Checking filename input
  stopifnot("filename is missing" = !missing(filename))
  stopifnot("filename must be character" = identical(class(filename), "character"))
  stopifnot("filename must be length one" = length(filename) == 1)

  # Checking fields input
  stopifnot("fields must be character" = identical(class(fields), "character"))
  stopifnot("unitConversion must be character" = identical(class(unitConversion), "character"))

  # Mandatory fields
  stopifnot("No 'names' selection in fields" = "names" %in% names(fields))
  stopifnot("No 'seasons' selection in fields" = "seasons" %in% names(fields))
  stopifnot("No 'isoTime' selection in fields" = "isoTime" %in% names(fields))
  stopifnot("No 'lon' selection in fields" = "lon" %in% names(fields))
  stopifnot("No 'lat' selection in fields" = "lat" %in% names(fields))
  stopifnot("No 'msw' selection in fields" = "msw" %in% names(fields))
  stopifnot("No unit conversion directive for 'msw' selection in unitConversion" = "msw" %in% names(unitConversion))
  stopifnot(
    "Invalid unitConversion directive for 'msw'" =
      unitConversion["msw"] %in% c("None", "mph2ms", "knt2ms", "kmh2ms")
  )

  # Optional fields
  if (("basin" %in% names(fields)) && is.null(basin)) {
    warning("No basin argument specified. StormR will work as expected
             but cannot use basin filtering for speed-up when collecting data")
  } else if (!("basin" %in% names(fields)) && !is.null(basin)) {
    stop("No basin field in `fields` input specified. StormR will work as
          expected but cannot use basin filtering for speed-up when collecting data")
  }


  if (!("rmw" %in% names(fields))) {
    warning("No 'rmw' selection in fields, use empirical_rmw = TRUE for the forthcoming computations")
  } else {
    stopifnot("No unit conversion directive for 'rmw' selection in unitConversion" = "rmw" %in% names(unitConversion))
    stopifnot("Invalid unitConversion directive for 'msw'" = unitConversion["rmw"] %in% c("None", "nm2km"))
  }

  if (!("pressure" %in% names(fields))) {
    warning("No 'pressure' selection in fields, Cannot use Holland method for the forthcoming computations")
  } else {
    stopifnot(
      "No unit conversion directive for 'pressure' selection in unitConversion" =
        "pressure" %in% names(unitConversion)
    )
    stopifnot(
      "Invalid unitConversion directive for 'msw'" =
        unitConversion["pressure"] %in% c("None", "b2pa", "mb2pa", "psi2pa", "atm2pa")
    )
  }


  if (!("poci" %in% names(fields))) {
    warning("No 'poci' selection in fields,  Cannot use Holland method for the forthcoming computations")
  } else {
    stopifnot(
      "No unit conversion directive for 'poci' selection in unitConversion" =
        "poci" %in% names(unitConversion)
    )
    stopifnot("Invalid unitConversion directive for 'msw'" = unitConversion["poci"]
    %in% c("None", "b2pa", "mb2pa", "psi2pa", "atm2pa"))
  }

  # Checking basin input
  if (!is.null(basin)) {
    stopifnot("basin must be character" = identical(class(basin), "character"))
    stopifnot("basin must be length one" = length(basin) == 1)
    stopifnot(
      "Invalid basin input, must be either 'NA', 'SA', 'EP', 'WP', 'SP', 'SI', or 'NI'" =
        basin %in% c("NA", "SA", "EP", "WP", "SP", "SI", "NI")
    )
  }



  # Checking seasons input
  stopifnot("seasons must be numeric" = identical(class(seasons), "numeric"))
  stopifnot("seasons must be a range of calendar year" = length(seasons) == 2 & seasons[1] <= seasons[2])


  # Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1" = verbose %in% c(0, 1))
}




#' Creating a `stormsDataset` object
#'
#' The `defStormsDataset()` function creates a `stormsDataset` object from a NetCDF file.
#' This is an essential first step before other `stormR` functions can be used.
#'
#' @param filename character. Name of the NetCDF (.nc) file. Default is the `test_dataset.nc`
#' file located in the `inst/extdata` repository of the directory (accessible by
#' `system.file("extdata", "test_dataset.nc", package = "StormR")`). This test dataset is extracted
#' from the IBTrACS.SP.v04r00.nc file and provides all the tropical cyclones that occurred around Vanuatu
#' from 2015 to 2016 and around New Caledonia from 2020 to 2021.
#' @param fields named character vector. This argument allows to specify the corresponding variable names
#'  in the input NetCDF file for each field in the output `stormsDataset`. By default, the corresponding
#'  variable names are set up to import data from a NetCDF file from the IBTrACS database (Knapp et al., 2010).
#' Corresponding variable names for following fields have to (mandatory fields) or can be
#' (recommended or optional fields) provided:
#'  \itemize{
#'    \item "`names"`, names of the storms (mandatory),
#'    \item `"seasons"`, years of observations (mandatory),
#'    \item `"isoTime"`, date and time of observations (mandatory),
#'    \item `"lon"`, longitude of the observations (mandatory),
#'    \item `"lat"`, latitude of the observations (mandatory),
#'    \item `"msw"`, maximum sustained wind speed (mandatory),
#'    \item `"basin"`, name of the area where the storm originated (recommended),
#'    \item `"rmw"`, radius of maximum winds: distance between the centre of the storm and
#'        its band of strongest winds (recommended),
#'    \item `"pressure"`, central pressure (recommended),
#'    \item `"poci"`, pressure of the last closed isobar (recommended), and
#'    \item `"sshs"`, Saffir-Simpson hurricane wind scale rating based on msw (optional).
#'  }
#' @param basin character. If the basin field is provided, then storm track data will
#' only be extracted for the named basin. By default `basin=NULL`, meaning that all storms
#' irrespective of the basin they originated in are extracted.
#' Seven basins can be used to filter the data set:
#' \itemize{
#'   \item `"NA"`, for North Atlantic basin,
#'   \item `"SA"`, for South Atlantic basin,
#'   \item `"EP"`, for Eastern North Pacific basin,
#'   \item `"WP"`, for Western North Pacific basin,
#'   \item `"SP"`, for South Pacific basin,
#'   \item `"SI"`, for South India basin, or
#'   \item `"NI"`, for North India basin.
#' }
#' @param seasons numeric vector. Seasons of occurrence of the storms (e.g., c(2020,2022)). In the Southern Hemisphere,
#' the cyclone season extends across two consecutive years. Therefore, to capture the 2021 to 2022 cyclone season both
#' years should be specified, with cyclones assigned for the year that originated in.
#' By default all storms occurring since 1980 are extracted.
#' @param unitConversion named character vector. `StormR` functions use the metric system (international
#' system of units), therefore `msw` has to be provided in \eqn{m.s^{-1}}, `rmw` in \eqn{km}, `pressure`
#' and `poci` in \eqn{Pa}. By default `unitConversion=c(msw = "knt2ms", rmw = "nm2km", pressure = "mb2pa",
#' poci = "mb2pa")` to meet the requirements when importing a NetCDF file from the IBTrACS database.
#' This argument is mandatory even if no conversion is needed. If no conversion is needed then
#' use `"None"` in the corresponding fields. The following unit conversions are implemented:
#'
#'   For `msw`,
#' \itemize{
#'   \item `"knt2ms"`, to convert knot to meter per second (default setting),
#'   \item `"kmh2ms"`, to convert kilometre per hour to meter per second,
#'   \item "`mph2ms"`, to convert miles per hour to meter per second, or
#'   \item `"None"`, if no conversion is needed.
#' }
#'
#'   For `rmw`,
#' \itemize{
#'   \item `"nm_to_ms"`to convert nautical miles to kilometre (default setting), or
#'   \item `"None"`if no conversion is needed.
#'  }
#'   For `pressure` and `poci`,
#'  \itemize{
#'    \item "`mb2pa"`, to convert  millibar to Pascal  (default setting),
#'    \item `"b2pa"`, to convert bar to Pascal,
#'    \item `"atm2pa"`, to convert  atmosphere to Pascal,
#'    \item `"psi2pa"`, to convert  psi to Pascal, or
#'    \item `"None"`, if no conversion is needed.
#'  }
#'
#' @param verbose numeric. Whether the function should display (`= 1`)
#'   or not (`= 0`) information about the processes.
#' @return The `defStormsDataset()` function returns a `stormsDataset` object.
#'
#' @references
#' Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann, C. J. (2010).
#' The International Best Track Archive for Climate Stewardship (IBTrACS).
#' Bulletin of the American Meteorological Society, 91(3), Article 3. https://doi.org/10.1175/2009bams2755.1
#'
#' @examples
#' # Creating a `stormsDataset` object with storms between 2010 and 2015
#' # in the South Pacific using the NetCDF provided with the package
#' SP_2015_2020 <- defStormsDataset(seasons = c(2010, 2015))
#' str(SP_2015_2020)
#' @export
defStormsDataset <- function(filename = system.file("extdata", "test_dataset.nc", package = "StormR"),
                             fields = c(
                               names = "name",
                               seasons = "season",
                               isoTime = "iso_time",
                               lon = "usa_lon",
                               lat = "usa_lat",
                               msw = "usa_wind",
                               basin = "basin",
                               sshs = "usa_sshs",
                               rmw = "usa_rmw",
                               pressure = "usa_pres",
                               poci = "usa_poci"
                             ),
                             basin = NULL,
                             seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
                             unitConversion = c(
                               msw = "knt2ms",
                               rmw = "nm2km",
                               pressure = "mb2pa",
                               poci = "mb2pa"
                             ),
                             verbose = 1) {
  checkInputsdefStormsDataset(filename, fields, basin, seasons, unitConversion, verbose)


  if (verbose) {
    cat("=== Loading data  ===\nOpen database... ")
  }


  dataBase <- ncdf4::nc_open(filename)

  if (verbose) {
    cat(filename, "opened\nCollecting data ...\n")
  }

  lon <- ncdf4::ncvar_get(dataBase, fields["lon"])
  season <- ncdf4::ncvar_get(dataBase, fields["seasons"])

  # Get dimensions
  row <- dim(lon)[1]
  len <- dim(lon)[2]
  ind <- seq(1, len)

  # Filter by season
  ind <- which(season %in% seq(seasons[1], seasons[2], 1))
  len <- length(ind)

  if (!is.null(basin)) {
    # Filter by basin ID
    basins <- ncdf4::ncvar_get(dataBase, fields["basin"])
    indB <- which(basins[1, ] == basin)
    ind <- intersect(ind, indB)
    len <- length(ind)
  }


  if (unitConversion["msw"] == "mph2ms") {
    msw <- array(mph2ms(ncdf4::ncvar_get(dataBase, fields["msw"])[, ind]),
      dim = c(row, len)
    )
  } else if (unitConversion["msw"] == "knt2ms") {
    msw <- array(knt2ms(ncdf4::ncvar_get(dataBase, fields["msw"])[, ind]),
      dim = c(row, len)
    )
  } else if (unitConversion["msw"] == "kmh2ms") {
    msw <- array(kmh2ms(ncdf4::ncvar_get(dataBase, fields["msw"])[, ind]),
      dim = c(row, len)
    )
  } else {
    msw <- array(ncdf4::ncvar_get(dataBase, fields["msw"])[, ind],
      dim = c(row, len)
    )
  }

  # Collect data
  data <- list(
    names = ncdf4::ncvar_get(dataBase, fields["names"])[ind],
    seasons = season[ind],
    isotimes = array(ncdf4::ncvar_get(dataBase, fields["isoTime"])[, ind],
      dim = c(row, len)
    ),
    longitude = array(ncdf4::ncvar_get(dataBase, fields["lon"])[, ind],
      dim = c(row, len)
    ),
    latitude = array(ncdf4::ncvar_get(dataBase, fields["lat"])[, ind],
      dim = c(row, len)
    ),
    msw = msw
  )

  # Sort by Date
  o <- order(data$isotimes[1, ])

  data$names <- data$names[o]
  data$seasons <- data$seasons[o]
  data$isotimes <- data$isotimes[, o]
  data$longitude <- data$longitude[, o]
  data$latitude <- data$latitude[, o]
  data$msw <- data$msw[, o]

  if ("sshs" %in% names(fields)) {
    data$sshs <- array(ncdf4::ncvar_get(dataBase, fields["sshs"])[, ind], dim = c(row, len))
    data$sshs <- data$sshs[, o]
  }

  if ("rmw" %in% names(fields)) {
    if (unitConversion["rmw"] == "nm2km") {
      data$rmw <- array(nm2km(ncdf4::ncvar_get(dataBase, fields["rmw"])[, ind]), dim = c(row, len))
    } else {
      data$rmw <- array(ncdf4::ncvar_get(dataBase, fields["rmw"])[, ind], dim = c(row, len))
    }
    data$rmw <- data$rmw[, o]
  }


  if ("pressure" %in% names(fields)) {
    if (unitConversion["pressure"] == "mb2pa") {
      data$pressure <- array(mb2pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[, ind]), dim = c(row, len))
    } else if (unitConversion["pressure"] == "b2pa") {
      data$pressure <- array(b2pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[, ind]), dim = c(row, len))
    } else if (unitConversion["pressure"] == "psi2pa") {
      data$pressure <- array(psi2pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[, ind]), dim = c(row, len))
    } else if (unitConversion["pressure"] == "atm2pa") {
      data$pressure <- array(atm2pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[, ind]), dim = c(row, len))
    } else {
      data$pressure <- array(ncdf4::ncvar_get(dataBase, fields["pressure"])[, ind], dim = c(row, len))
    }
    data$pressure <- data$pressure[, o]
  }


  if ("poci" %in% names(fields)) {
    if (unitConversion["poci"] == "mb2pa") {
      data$poci <- array(mb2pa(ncdf4::ncvar_get(dataBase, fields["poci"])[, ind]), dim = c(row, len))
    } else {
      data$poci <- array(ncdf4::ncvar_get(dataBase, fields["poci"])[, ind], dim = c(row, len))
    }
    data$poci <- data$poci[, o]
  }

  ncdf4::nc_close(dataBase)



  if (verbose) {
    cat("=== DONE ===\n")
  }

  if (is.null(basin)) {
    basin <- "None"
  }


  sds <- new(
    Class = "stormsDataset",
    filename = filename,
    fields = fields,
    database = data,
    basin = basin,
    seasons = c(min = min(data$seasons, na.rm = TRUE), max = max(data$seasons, na.rm = TRUE))
  )


  return(sds)
}
