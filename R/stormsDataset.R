



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





#' StormsDataset object
#'
#' Choose the database to use within the package's functions
#'
#' @slot filename character. Name of the database to load. Must be a netcdf file
#' @slot fields named character vector. Dictionary that provides all the name of
#' dimensions to extract from the netcdf database (See `Details`)
#' @slot basin character. Basin name to filter the database within its
#' boundaries. Default value is set to `NULL`. It must be either
#' \itemize{
#'   \item `"NA"`: North Atlantic
#'   \item `"SA"`: South Atlantic
#'   \item `"EP"`: Eastern North Pacific
#'   \item `"WP"`: Western North Pacific
#'   \item `"SP"`: South Pacific
#'   \item `"SI"`: South India
#'   \item `"NI"`: North India
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
#'   \item A field `basin`: which dimension contains the basin location of
#'        storms in the netcdf database. Used to filter the storms in the netcdf
#'        database
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
#' The following fields are optional but highly recommanded:
#' \itemize{
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
  if(!("basin" %in% names(fields)))
    warning("No 'basin' selection in fields, Cannot use basin filtering when collecting data")
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





#' Initialize a StormsDataset object
#'
#' @param filename character. Name of the database to load. Must be a netcdf
#'   file
#' @param fields named character vector. Dictionary that provides all the name
#'   of dimension to extract from the netcdf database (See `StormsDataSet`
#'   class)
#' @param basin character. Basin name to filter the database within its
#'   boundaries. Default value is set to `NULL`. It must be either
#' \itemize{
#'   \item `"NA"`: North Atlantic
#'   \item `"SA"`: South Atlantic
#'   \item `"EP"`: Eastern North Pacific
#'   \item `"WP"`: Western North Pacific
#'   \item `"SP"`: South Pacific
#'   \item `"SI"`: South India
#'   \item `"NI"`: North India
#' }
#' @param seasons numeric vector. Range of calendar years to filter storms. For
#'   cyclones that formed in one year and dissipated in the following year, the
#'   latter should be used. Default value is set to between `1980` and the
#'   current year
#' @param unit_conversion named character vector. Dictionary that provides the
#'   instructions to convert `msw`, `rmw`, `pressure` and/or `poci` fields in
#'   the correct units. Default value is `c(msw = "knt_to_ms", rmw = "nm_to_km",
#'   pressure = "mb_to_pa", poci = "mb_to_pa")` according to the IBTrACS
#'   databases. This input is mandatory even if no conversion is needed. In this
#'   case use `"None"` in the corresponding fields. Allowed value are: For `msw`
#'   fields
#' \itemize{
#'   \item `"knt_to_ms"`: converts knot in meter per second
#'   \item `"kmh_to_ms"`: converts kilometer per hour in meter per second
#'   \item "`mph_to_ms"`: converts miles per hour in meter per second
#'   \item `"None"`: No conversion needed
#' }
#'
#'   For `rmw` fields
#' \itemize{
#'   \item `"nm_to_ms"`: converts nautical miles in kilometer
#'   \item `"None"`: No conversion needed
#'  }
#'
#'   For both `pressure` and `poci`
#'  \itemize{
#'    \item `"b_to_pa"`: converts bar in pascal
#'    \item "`mb_to_pa"`: converts millibar in pascal
#'    \item `"atm_to_pa"`: converts atmosphere in pascal
#'    \item `"psi_to_pa"`: converts psi in pascal
#'    \item `"None"`: No conversion needed
#'  }
#'
#' @param verbose logical. Whether or not the function should display
#'   informations about the process
#' @return An object of class `StormsDataset`
#' @export
defDatabase <- function(filename,
                        fields = c(basin = "basin",
                                   names = "name",
                                   seasons = "season",
                                   isoTime = "iso_time",
                                   lon = "usa_lon",
                                   lat = "usa_lat",
                                   msw = "usa_wind",
                                   sshs = "usa_sshs",
                                   rmw = "usa_rmw",
                                   pressure = "usa_pres",
                                   poci = "usa_poci"),
                        basin = "SP",
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

  #Filter by basin ID
  basins <- ncdf4::ncvar_get(dataBase, fields["basin"])
  season <- ncdf4::ncvar_get(dataBase, fields["seasons"])

  #Get dimensions
  row <- dim(basins)[1]
  len <- dim(basins)[2]
  ind <- seq(1,len)

  ind <- which(season %in% seq(seasons[1], seasons[2], 1))
  len <- length(ind)
  
  if(!is.null(basin)){
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

  if("sshs" %in% names(fields))
    data$sshs <- array(ncdf4::ncvar_get(dataBase, fields["sshs"])[,ind], dim = c(row,len))

  if("rmw" %in% names(fields)){
    if(unit_conversion["rmw"] == "nm_to_km"){
      data$rmw <- array(nm_to_km(ncdf4::ncvar_get(dataBase, fields["rmw"])[,ind]), dim = c(row,len))
    }else{
      data$rmw <- array(ncdf4::ncvar_get(dataBase, fields["rmw"])[,ind], dim = c(row,len))
    }
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
  }
    

  if("poci" %in% names(fields)){
    if(unit_conversion["poci"] == "mb_to_pa"){
      data$poci <- array(mb_to_pa(ncdf4::ncvar_get(dataBase, fields["poci"])[,ind]), dim = c(row,len))
    }else{
      data$poci <- array(ncdf4::ncvar_get(dataBase, fields["poci"])[,ind], dim = c(row,len))
    }
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
             seasons = c(min = seasons[1], max = seasons[2]))


  return(sds)


}


