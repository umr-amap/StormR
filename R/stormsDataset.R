



###########################
#Unit conversion functions#
###########################

#For msw
knt_to_ms = function(x){
  return(x * kt2ms)
}

kmh_to_ms = function(x){
  return(x * kmh2ms)
}

#For rmw
nm_to_km = function(x){
  return(x * nm2km)
}

#For pressure
mb_to_pa = function(x){
  return(x * mb2pa)
}




#' StormsDataset object
#'
#' Choose the database to use within the package's functions
#'
#' @slot filename character. Name of the database to load. Must be a netcdf file
#' @slot fields named character vector. Dictionary that provides all the name of
#' dimensions to extract from the netcdf database (See Details)
#' @slot basin character. Basin name to filter the database within its
#' boundaries. Default value is set to NULL. It must be either
#' \itemize{
#'   \item "NA": North Atlantic
#'   \item "SA": South Atlantic
#'   \item "EP": Eastern North Pacific
#'   \item "WP": Western North Pacific
#'   \item "SP": South Pacific
#'   \item "SI": South India
#'   \item "NI": North India
#' }
#' @slot database list of 6 to 10 slots depending on the fields input. Each slot
#' is either a 1D array of dimension (number of storms), for "names" and
#' "seasons" fields, or a 2D array of dimension
#' (Maximum number of observations:number of storms), for the remaining fields
#' which are "isoTime", "lon", "lat", "msw", "rmw", "pressure", "poci", "sshs"
#'
#' @details
#' The fields input must provide at least 6 mandatory fields (and at most 11) in
#' order to benefit from all the functionalities of this package:
#' \itemize{
#'   \item A field called "name": which dimension contains the names of storms
#'         in the netcdf database
#'   \item A field called "seasons": which dimension contains the cyclonic
#'         seasons of storms in the netcdf database
#'   \item A field called "isoTime": which dimension contains the ISO times of
#'         each (3 or 6 hourly) observations for all storms in the database
#'   \item A field called "lon": which dimension contains the longitude
#'         coordinates (Eastern degree) of each observations for all storms in
#'         the netcdf database
#'   \item A field called "lat": which dimension contains the latitude
#'         coordinates (Nothern degree) of each observations for all storms in
#'         the netcdf database
#'   \item A field called "msw": which dimension contains the maximum sustained
#'         wind speed (knt) of each observations for all storms in the netcdf
#'         database
#' }
#' The following fields are optional but highly recommanded:
#' \itemize{
#'  \item A field called "basin": which dimension contains the basin location of
#'        storms in the netcdf database. Used to filter the storms in the netcdf
#'        database
#'  \item A field called "rmw": which dimension contains the radius of maximum
#'        wind speed (nm) of each observations for all storms in the netcdf
#'        database (See spatialBehaviour, temporalBehaviour)
#'  \item A field called "sshs": which dimension contains the Saffir Simpson
#'        Hurricane Scale index of each observations for all storms in the
#'        netcdf database
#' }
#' Finally these following fields are optional but mandatory to perform Holland
#' model (See spatialBehaviour, temporalBehaviour)
#' \itemize{
#'   \item A field called "pressure": which dimension contains the pressure (mb)
#'         in the eye for of each observations for all storms in the netcdf
#'         database
#'   \item A field called "poci": which dimension contains the Pressure at the
#'         Outermost Closed Isobar (mb) for of each observations for all storms
#'         in the nectdf database
#' }
#'
#' Default value is set according to the most relevant dimensions of IBTraCS
#' databases: fields = fields = c("basin" = "basin", "names" = "name",
#' "seasons" = "season", "isoTime" = "iso_time", "lon" = "usa_lon",
#' "lat" = "usa_lat", "msw" = "usa_wind", "rmw" = "usa_rmw",
#' "pressure" = "usa_pres", "poci" = "usa_poci", "sshs" = "usa_sshs")
#'
#' @export
StormsDataset <- methods::setClass(
  "StormsDataset",
  slots = c(
    filename = "character",
    fields = "character",
    basin = "character",
    database = "list"
  )
)





#' check inputs for defDatabase function
#'
#' @noRd
#' @param filename character
#' @param fields character vector
#' @param basin character
#' @param unit_conversion character vector
#' @param verbose logical
#'
#' @return NULL
checkInputsIDb <- function(filename, fields, basin, verbose){

  #Checking filename input
  stopifnot("filename must be character" = identical(class(filename),"character"))
  stopifnot("filename must be legnth one" = length(filename) == 1)

  #Checking fields input
  stopifnot("fields must be character" = identical(class(fields),"character"))
  
  #Mandatory fields
  stopifnot("No 'names' selection in fields" = "names" %in% names(fields))
  stopifnot("No 'seasons' selection in fields" = "seasons" %in% names(fields))
  stopifnot("No 'isoTime' selection in fields" = "isoTime" %in% names(fields))
  stopifnot("No 'lon' selection in fields" = "lon" %in% names(fields))
  stopifnot("No 'lat' selection in fields" = "lat" %in% names(fields))
  stopifnot("No 'msw' selection in fields" = "msw" %in% names(fields))
  stopifnot("No unit conversion directive for 'msw' selection in unit_conversion" = "msw" %in% names(unit_conversion))
  stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["msw"] %in% c("None", "knt_to_ms", "kmh_to_ms"))
  
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
    stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["pressure"] %in% c("None", "mb_to_pa"))
  }
   

  if(!("poci" %in% names(fields))){
    warning("No 'poci' selection in fields,  Cannot use Holland method for the forthcoming computations")
  }else{
    stopifnot("No unit conversion directive for 'poci' selection in unit_conversion" = "poci" %in% names(unit_conversion))
    stopifnot("Invalid unit_conversion directive for 'msw'" = unit_conversion["poci"] %in% c("None", "mb_to_pa"))
  }
    
  #Checking basin input
  if(!is.null(basin)){
    stopifnot("basin must be character" = identical(class(basin),"character"))
    stopifnot("basin must be length one" = length(length) == 1)
    stopifnot("Invalid basin input, must be either 'NA', 'SA', 'EP', 'WP', 'SP', 'SI', or 'NI'" =
                basin %in% c("NA", "SA", "EP", "WP", "SP", "SI", "NI"))
  }

  
  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose),"logical"))
  
}





#' Initialize a StormsDataset object
#'
#' @param filename character. Name of the database to load. Must be a netcdf
#' file
#' @param fields named character vector. Dictionary that provides all the name
#' of dimension to extract from the netcdf database (See StormsDataSet class)
#' @param basin character. Basin name to filter the database within its
#' boundaries. Default value is set to NULL. It must be either
#' \itemize{
#'   \item "NA": North Atlantic
#'   \item "SA": South Atlantic
#'   \item "EP": Eastern North Pacific
#'   \item "WP": Western North Pacific
#'   \item "SP": South Pacific
#'   \item "SI": South India
#'   \item "NI": North India
#' }
#' @param unit_conversion
#' @param verbose logical. Whether or not the function should display
#' informations about the process
#' @return An object of class StormsDataset
#' @export
defDatabase <- function(filename = system.file("extdata", "IBTrACS.SP.v04r00.nc", package = "StormR"),
                        fields = c("basin" = "basin",
                                   "names" = "name",
                                   "seasons" = "season",
                                   "isoTime" = "iso_time",
                                   "lon" = "usa_lon",
                                   "lat" = "usa_lat",
                                   "msw" = "usa_wind",
                                   "sshs" = "usa_sshs",
                                   "rmw" = "usa_rmw",
                                   "pressure" = "usa_pres",
                                   "poci" = "usa_poci"),
                        basin = "SP",
                        unit_conversion=c(msw = "kt_to_ms", rmw = "nm_to_km", pressure="mb_to_pa", poci="mb_to_pa"),
                        verbose = TRUE){

  checkInputsIDb(filename, fields, basin, unit_conversion, verbose)


  if(verbose)
    cat("=== Loading data  ===\nOpen database... ")


  dataBase <- ncdf4::nc_open(filename)

  if(verbose)
    cat(filename,"opened\nCollecting data ...\n")

  #Filter by basin ID
  basins <- ncdf4::ncvar_get(dataBase, fields["basin"])

  #Get dimensions
  row <- dim(basins)[1]
  len <- dim(basins)[2]
  ind <- seq(1,len)

  if(!is.null(basin)){
    ind <- which(basins[1,] == basin)
    len <- length(ind)
  }

  #Collect data
  data <- list(names  = ncdf4::ncvar_get(dataBase, fields["names"])[ind],
               seasons = ncdf4::ncvar_get(dataBase, fields["seasons"])[ind],
               isotimes = array(ncdf4::ncvar_get(dataBase, fields["isoTime"])[,ind],
                                dim = c(row,len)),
               longitude = array(ncdf4::ncvar_get(dataBase, fields["lon"])[,ind],
                                 dim = c(row,len)),
               latitude = array(ncdf4::ncvar_get(dataBase, fields["lat"])[,ind],
                                dim = c(row,len)),
               msw = array(ncdf4::ncvar_get(dataBase, fields["msw"])[,ind],
                           dim = c(row,len)))

  if("sshs" %in% names(fields))
    data$sshs <- array(ncdf4::ncvar_get(dataBase, fields["sshs"])[,ind], dim = c(row,len))

  if("rmw" %in% names(fields))
    data$rmw <- array(ncdf4::ncvar_get(dataBase, fields["rmw"])[,ind], dim = c(row,len))

  if("pressure" %in% names(fields))
    data$pressure <- array(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind], dim = c(row,len))

  if("poci" %in% names(fields))
    data$poci <- array(ncdf4::ncvar_get(dataBase, fields["poci"])[,ind], dim = c(row,len))
  ncdf4::nc_close(dataBase)



  if(verbose)
    cat("=== DONE ===\n")

  if(is.null(basin))
    basin <- "None"



  sds <- new(Class = "StormsDataset",
             filename = filename,
             fields = fields,
             database = data,
             basin = basin)


  return(sds)


}


