




###########################
#Unit conversion functions#
###########################

#For msw
knt_to_ms <- function(x){
  return(x * knt2ms)
}

mph_to_ms <- function(x){
  return(x * mph2ms)
}

kmh_to_ms <- function(x){
  return(x * kmh2ms)
}

#For rmw
nm_to_km <- function(x){
  return(x * nm2km)
}

#For pressure
b_to_pa <- function(x){
  return(x * b2pa)
}

mb_to_pa <- function(x){
  return(x * mb2pa)
}

psi_to_pa <- function(x){
  return(x * psi2pa)
}

atm_to_pa <- function(x){
  return(x * atm2pa)
}





#' StormsDataset
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
#' `fields = c(basin = "basin", names = "name", seasons = "season", isoTime = "iso_time", lon = "usa_lon", lat = "usa_lat", msw = "usa_wind", rmw = "usa_rmw", pressure = "usa_pres", poci = "usa_poci", sshs = "usa_sshs")`
#'
#' @export
StormsDataset <- methods::setClass(
  "StormsDataset",
  slots = c(
    filename = "character",
    fields = "character",
    basin = "character",
    seasons = "numeric",
    database = "list"
  )
)





#' check inputs for defDatabase function
#'
#' @noRd
#' @param filename character
#' @param fields character vector
#' @param basin character
#' @param seasons numeric vector
#' @param unit_conversion character vector
#' @param verbose logical
#'
#' @return NULL
checkInputsIDb <- function(filename, fields, basin, seasons, unit_conversion, verbose){

  #Checking filename input
  stopifnot("filename is missing" = !missing(filename))
  stopifnot("filename must be character" = identical(class(filename),"character"))
  stopifnot("filename must be length one" = length(filename) == 1)

  #Checking fields input
  stopifnot("fields must be character" = identical(class(fields),"character"))
  stopifnot("unit_conversion must be character" = identical(class(unit_conversion),"character"))

  #Mandatory fields
  stopifnot("No 'names' selection in fields" = "names" %in% names(fields))
  stopifnot("No 'seasons' selection in fields" = "seasons" %in% names(fields))
  stopifnot("No 'isoTime' selection in fields" = "isoTime" %in% names(fields))
  stopifnot("No 'lon' selection in fields" = "lon" %in% names(fields))
  stopifnot("No 'lat' selection in fields" = "lat" %in% names(fields))
  stopifnot("No 'msw' selection in fields" = "msw" %in% names(fields))
  stopifnot("No unit conversion directive for 'msw' selection in unit_conversion" = "msw" %in% names(unit_conversion))
  stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["msw"] %in% c("None", "mph_to_ms", "knt_to_ms", "kmh_to_ms"))

  #Optional fields
  if(("basin" %in% names(fields)) & is.null(basin)){
    warning("No basin argument specified, Cannot use basin filtering when collecting data")
  }else if(!("basin" %in% names(fields)) & !is.null(basin)){
    stop("No basin field in `fields` input specified, Cannot use basin filtering when collecting data")
  }


  if(!("rmw" %in% names(fields))){
    warning("No 'rmw' selection in fields, use empirical_rmw = TRUE for the forthcoming computations")
  }else{
    stopifnot("No unit conversion directive for 'rmw' selection in unit_conversion" = "rmw" %in% names(unit_conversion))
    stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["rmw"] %in% c("None", "nm_to_km"))
  }

  if(!("pressure" %in% names(fields))){
    warning("No 'pressure' selection in fields, Cannot use Holland method for the forthcoming computations")
  }else{
    stopifnot("No unit conversion directive for 'pressure' selection in unit_conversion" = "pressure" %in% names(unit_conversion))
    stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["pressure"] %in% c("None", "b_to_pa", "mb_to_pa", "psi_to_pa", "atm_to_pa"))
  }


  if(!("poci" %in% names(fields))){
    warning("No 'poci' selection in fields,  Cannot use Holland method for the forthcoming computations")
  }else{
    stopifnot("No unit conversion directive for 'poci' selection in unit_conversion" = "poci" %in% names(unit_conversion))
    stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["poci"] %in% c("None", "b_to_pa", "mb_to_pa", "psi_to_pa", "atm_to_pa"))
  }

  #Checking basin input
  if(!is.null(basin)){
    stopifnot("basin must be character" = identical(class(basin),"character"))
    stopifnot("basin must be length one" = length(basin) == 1)
    stopifnot("Invalid basin input, must be either 'NA', 'SA', 'EP', 'WP', 'SP', 'SI', or 'NI'" =
                basin %in% c("NA", "SA", "EP", "WP", "SP", "SI", "NI"))
  }



  #Checking seasons input
  stopifnot("seasons must be numeric" = identical(class(seasons),"numeric"))
  stopifnot("seasons must be a range of calendar year" = length(seasons) == 2 & seasons[1] <= seasons[2])


  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose),"logical"))

}




#' Creating a `StormsDataset` object
#'
#' The `defDatabase()` function creates a `StormsDataset` object from a NetCDF file. 
#' This is an essential first step before other `stormR` functions can be used.
#'
#' @param filename character. Name of the NetCDF (.nc) file. Default is the `test_dataset.nc` 
#' file located in the `inst/extdata` repository of the directory (accessible by 
#' `system.file("extdata", "test_dataset.nc", package = "StormR")`). This test dataset is extracted 
#' from the IBTrACS.SP.v04r00.nc file and provides all the tropical cyclones that occurred around Vanuatu 
#' from 2015 to 2016 and around New Caledonia from 2020 to 2021.
#' @param fields named character vector. This argument allows to specify the corresponding variable names
#'  in the input NetCDF file for each field in the output `StormsDataset`. By default, the corresponding 
#'  variable names are set up to import data from a NetCDF file from the IBTrACS database (Knapp et al., 2010). Corresponding variable 
#'  names for following fields have to (mandatory fields) or can be (recommended or optional fields) provided:
#'  \itemize{
#'    \item "`names"`, names of the storms (mandatory),
#'    \item `"seasons"`, years of observations (mandatory),
#'    \item `"isoTime"`, date and time of observations (mandatory),
#'    \item `"lon"`, longitude of the observations (mandatory),
#'    \item `"lat"`, latitude of the observations (mandatory),
#'    \item `"msw"`, maximum sustained wind speed (mandatory),
#'    \item `"basin"`, name of the area where the storm originated (recommended),
#'    \item `"rmw"`, radius of maximum winds: distance between the centre of the storm and its band of strongest winds (recommended),
#'    \item `"pressure"`, central pressure (recommended),
#'    \item `"poci"`, pressure of the last closed isobar (recommended), and
#'    \item `"sshs"`, Saffir-Simpson hurricane wind scale rating based on msw (optional).
#'  }
#' @param basin character. If the basin field is provided, then storm track data will only be extracted for the named basin.
#' By default `basin=NULL`, meaning that all storms irrespective of the basin they originated in are extracted. 
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
#' years should be specified, with cyclones assigned for the year that originated in. By default all storms occurring since 
#' 1980 are extracted.
#' @param unit_conversion named character vector. `StormR` functions use the metric system (international system of units), therefore 
#' `msw` has to be provided in \eqn{m.s^{-1}}, `rmw` in \eqn{km}, `pressure` and `poci` in \eqn{Pa}. By default
#' `unit_conversion=c(msw = "knt_to_ms", rmw = "nm_to_km", pressure = "mb_to_pa", poci = "mb_to_pa")`
#' to meet the requirements when importing a NetCDF file from the IBTrACS database.
#' This argument is mandatory even if no conversion is needed. If no conversion is needed then
#' use `"None"` in the corresponding fields. The following unit conversions are implemented:
#'
#'   For `msw`,
#' \itemize{
#'   \item `"knt_to_ms"`, to convert knot to meter per second (default setting),
#'   \item `"kmh_to_ms"`, to convert kilometre per hour to meter per second,
#'   \item "`mph_to_ms"`, to convert miles per hour to meter per second, or
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
#'    \item "`mb_to_pa"`, to convert  millibar to Pascal  (default setting),
#'    \item `"b_to_pa"`, to convert bar to Pascal,
#'    \item `"atm_to_pa"`, to convert  atmosphere to Pascal,
#'    \item `"psi_to_pa"`, to convert  psi to Pascal, or
#'    \item `"None"`, if no conversion is needed.
#'  }
#'
#' @param verbose logical. Whether (TRUE) or not (FALSE) the function should display
#'   information about the processes.
#' @return The `defDatabase()` function returns a `StormsDataset` object.
#' @example 
#' \dontrun{
#' #Creating a `StormsDataset` object with storms between 2010 and 2015 
#' #in the South Pacific using the NetCDF provided with the package
#' SP_2015_2020<-defDatabase(seasons=c(2010-2015))
#' str(SP_2015_2020)
#' }
#' @export
defDatabase <- function(filename=system.file("extdata", "test_dataset.nc", package = "StormR"),
                        fields = c(names = "name",
                                   seasons = "season",
                                   isoTime = "iso_time",
                                   lon = "usa_lon",
                                   lat = "usa_lat",
                                   msw = "usa_wind",
                                   basin = "basin",
                                   sshs = "usa_sshs",
                                   rmw = "usa_rmw",
                                   pressure = "usa_pres",
                                   poci = "usa_poci"),
                        basin = NULL,
                        seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
                        unit_conversion = c(msw = "knt_to_ms",
                                            rmw = "nm_to_km",
                                            pressure = "mb_to_pa",
                                            poci = "mb_to_pa"),
                        verbose = TRUE){

  checkInputsIDb(filename, fields, basin, seasons, unit_conversion, verbose)


  if(verbose)
    cat("=== Loading data  ===\nOpen database... ")


  dataBase <- ncdf4::nc_open(filename)

  if(verbose)
    cat(filename,"opened\nCollecting data ...\n")

  lon <- ncdf4::ncvar_get(dataBase, fields["lon"])
  season <- ncdf4::ncvar_get(dataBase, fields["seasons"])

  #Get dimensions
  row <- dim(lon)[1]
  len <- dim(lon)[2]
  ind <- seq(1,len)

  #Filter by season
  ind <- which(season %in% seq(seasons[1], seasons[2], 1))
  len <- length(ind)

  if(!is.null(basin)){
    #Filter by basin ID
    basins <- ncdf4::ncvar_get(dataBase, fields["basin"])
    indB <- which(basins[1,] == basin)
    ind <- intersect(ind,indB)
    len <- length(ind)
  }


  if(unit_conversion["msw"] == "mph_to_ms"){
    msw <- array(mph_to_ms(ncdf4::ncvar_get(dataBase, fields["msw"])[,ind]),
                 dim = c(row,len))
  }else if(unit_conversion["msw"] == "knt_to_ms"){
    msw <- array(knt_to_ms(ncdf4::ncvar_get(dataBase, fields["msw"])[,ind]),
                 dim = c(row,len))
  }else if(unit_conversion["msw"] == "kmh_to_ms"){
    msw <- array(kmh_to_ms(ncdf4::ncvar_get(dataBase, fields["msw"])[,ind]),
                 dim = c(row,len))
  }else{
    msw <- array(ncdf4::ncvar_get(dataBase, fields["msw"])[,ind],
                 dim = c(row,len))
  }

  #Collect data
  data <- list(names  = ncdf4::ncvar_get(dataBase, fields["names"])[ind],
               seasons = season[ind],
               isotimes = array(ncdf4::ncvar_get(dataBase, fields["isoTime"])[,ind],
                                dim = c(row,len)),
               longitude = array(ncdf4::ncvar_get(dataBase, fields["lon"])[,ind],
                                 dim = c(row,len)),
               latitude = array(ncdf4::ncvar_get(dataBase, fields["lat"])[,ind],
                                dim = c(row,len)),
               msw = msw)
  
  #Sort by Date
  o <- order(data$isotimes[1,])
  
  data$names <- data$names[o]
  data$seasons <- data$seasons[o]
  data$isotimes <- data$isotimes[,o]
  data$longitude <- data$longitude[,o]
  data$latitude <- data$latitude[,o]
  data$msw <- data$msw[,o]

  if("sshs" %in% names(fields)){
    data$sshs <- array(ncdf4::ncvar_get(dataBase, fields["sshs"])[,ind], dim = c(row,len))
    data$sshs <- data$sshs[,o]
  }

  if("rmw" %in% names(fields)){
    if(unit_conversion["rmw"] == "nm_to_km"){
      data$rmw <- array(nm_to_km(ncdf4::ncvar_get(dataBase, fields["rmw"])[,ind]), dim = c(row,len))
    }else{
      data$rmw <- array(ncdf4::ncvar_get(dataBase, fields["rmw"])[,ind], dim = c(row,len))
    }
    data$rmw <- data$rmw[,o]
  }


  if("pressure" %in% names(fields)){
    if(unit_conversion["pressure"] == "mb_to_pa"){
      data$pressure <- array(mb_to_pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind]), dim = c(row,len))
    }else if(unit_conversion["pressure"] == "b_to_pa"){
      data$pressure <- array(b_to_pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind]), dim = c(row,len))
    }else if(unit_conversion["pressure"] == "psi_to_pa"){
      data$pressure <- array(psi_to_pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind]), dim = c(row,len))
    }else if(unit_conversion["pressure"] == "atm_to_pa"){
      data$pressure <- array(atm_to_pa(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind]), dim = c(row,len))
    }else{
      data$pressure <- array(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind], dim = c(row,len))
    }
    data$pressure <- data$pressure[,o]
  }


  if("poci" %in% names(fields)){
    if(unit_conversion["poci"] == "mb_to_pa"){
      data$poci <- array(mb_to_pa(ncdf4::ncvar_get(dataBase, fields["poci"])[,ind]), dim = c(row,len))
    }else{
      data$poci <- array(ncdf4::ncvar_get(dataBase, fields["poci"])[,ind], dim = c(row,len))
    }
    data$poci <- data$poci[,o]
  }

  ncdf4::nc_close(dataBase)



  if(verbose)
    cat("=== DONE ===\n")

  if(is.null(basin))
    basin <- "None"


  sds <- new(Class = "StormsDataset",
             filename = filename,
             fields = fields,
             database = data,
             basin = basin,
             seasons = c(min = min(data$seasons, na.rm = T), max =  max(data$seasons, na.rm = T)))


  return(sds)


}
