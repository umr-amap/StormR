




#' StormsDataset object
#'
#' Choose the database to use within the package's functions
#'
#' @slot filename character. Name of the file to be loaded
#' @slot fields named character vector. Dictionary that provides all the name of dimension to extract from the netcdf database.
#' See Details
#' @slot basin character. Basin name to filter the database within its boundaries. If NULL, no filter is performed and the
#' whole database will be collected when calling collectData method.
#' @slot database list of 12 slots (See Details)
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






checkInputsIDb <- function(filename, fields, basin, verbose){

  #Checking filename input
  stopifnot("filename must be character" = identical(class(filename),"character"))
  stopifnot("filename must be legnth one" = length(filename) == 1)

  #Checking fields input
  stopifnot("fields must be character" = identical(class(fields),"character"))
  #Mandatory fields
  stopifnot("No 'basin' selection in fields" = "basin" %in% names(fields))
  stopifnot("No 'names' selection in fields" = "names" %in% names(fields))
  stopifnot("No 'seasons' selection in fields" = "seasons" %in% names(fields))
  stopifnot("No 'isoTime' selection in fields" = "isoTime" %in% names(fields))
  stopifnot("No 'lon' selection in fields" = "lon" %in% names(fields))
  stopifnot("No 'lat' selection in fields" = "lat" %in% names(fields))
  stopifnot("No 'msw' selection in fields" = "msw" %in% names(fields))
  stopifnot("No 'sshs' selection in fields" = "sshs" %in% names(fields))
  stopifnot("No 'rmw' selection in fields" = "rmw" %in% names(fields))
  stopifnot("No 'pressure' selection in fields" = "pressure" %in% names(fields))
  stopifnot("No 'poci' selection in fields" = "poci" %in% names(fields))


  #Checking basin input
  stopifnot("basin must be character" = identical(class(basin),"character"))
  stopifnot("basin must be length one" = length(length) == 1)
  stopifnot("Invalid basin input, must be either 'NA', 'SA', 'EP', 'WP', 'SP', 'SI', 'NI', or 'ALL'" =
              basin %in% c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"))
  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose),"logical"))


}

#' Initialize a StormsDataset object
#'
#' @param filename character. Name of the file to be loaded
#' @param fields named character vector. Dictionary that provides all the name of dimension to extract from the netcdf database.
#' See Details
#' @param basin character. Basin name to filter the database within its boundaries. If NULL, no filter is performed and the
#' whole database will be collected when calling collectData method.
#' @param verbose logical.
#' @return An object of class StormsDataset
#' @export
initDatabase <- function(filename = system.file("extdata", "IBTrACS.SP.v04r00.nc", package = "StormR"),
                         fields = c("basin" = "basin",
                                    "names" = "name",
                                    "seasons" = "season",
                                    "isoTime" = "iso_time",
                                    "lon" = "usa_lon",
                                    "lat" = "usa_lat",
                                    "msw" = "usa_wind",
                                    "rmw" = "usa_rmw",
                                    "pressure" = "usa_pres",
                                    "poci" = "usa_poci",
                                    "sshs" = "usa_sshs"),
                         basin = "SP",
                         verbose = TRUE){

  checkInputsIDb(filename, fields, basin, verbose)


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

  if(!is.null(basin) & basin != "ALL"){
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
                           dim = c(row,len)),
               rmw = array(ncdf4::ncvar_get(dataBase, fields["rmw"])[,ind],
                           dim = c(row,len)),
               pres = array(ncdf4::ncvar_get(dataBase, fields["pressure"])[,ind],
                            dim = c(row,len)),
               poci = array(ncdf4::ncvar_get(dataBase, fields["poci"])[,ind],
                            dim = c(row,len)),
               sshs = array(ncdf4::ncvar_get(dataBase, fields["sshs"])[,ind],
                            dim = c(row,len)))

  ncdf4::nc_close(dataBase)



  if(verbose)
    cat("=== DONE ===\n")



  sds <- new(Class = "StormsDataset",
             filename = filename,
             fields = fields,
             database = data,
             basin = basin)


  return(sds)


}


