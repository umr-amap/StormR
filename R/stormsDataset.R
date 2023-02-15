




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
#' @slot loaded logical. Whether or not the database have already been loaded. If TRUE, it cannot be loaded anymore
#' @export
StormsDataset <- methods::setClass(
  "StormsDataset",
  slots = c(
    filename = "character",
    fields = "character",
    basin = "character",
    database = "list",
    loaded = "logical"
  )
)



#' Initialize a StormsDataset object
#'
#' @param filename character. Name of the file to be loaded
#' @param fields named character vector. Dictionary that provides all the name of dimension to extract from the netcdf database.
#' See Details
#' @param basin character. Basin name to filter the database within its boundaries. If NULL, no filter is performed and the
#' whole database will be collected when calling collectData method.
#' @return An object of class StormsDataset
#' @export
initDatabase <- function(filename = system.file("extdata", "IBTrACS.SP.v04r00.nc", package = "StormR"),
                         fields = c("sid" = "sid",
                                    "basin" = "basin",
                                    "names" = "name",
                                    "seasons" = "season",
                                    "isoTime" = "iso_time",
                                    "lon" = "usa_lon",
                                    "lat" = "usa_lat",
                                    "msw" = "usa_wind",
                                    "rmw" = "usa_rmw",
                                    "roci" = "usa_roci",
                                    "pressure" = "usa_pres",
                                    "poci" = "usa_poci",
                                    "sshs" = "usa_sshs"),
                         basin = "SP"){

  sds <- new(Class = "StormsDataset",
             filename = filename,
             fields = fields,
             database = list(),
             basin = basin,
             loaded = FALSE)

  return(sds)

}











setGeneric("collectData", function(sds, verbose = T) standardGeneric("collectData"))
setMethod("collectData",
          signature("StormsDataset"),
          definition =  function(sds, verbose = T){

            stopifnot("Data base already loaded" = !sds@loaded)

            if(verbose)
              cat("=== Loading data  ===\nOpen database... ")


            dataBase <- ncdf4::nc_open(sds@filename)

            if(verbose)
              cat(sds@filename,"opened\nCollecting data ...\n")

            #Filter by basin ID
            basin <- ncdf4::ncvar_get(dataBase, sds@fields["basin"])

            #Get dimensions
            row <- dim(basin)[1]
            len <- dim(basin)[2]
            ind <- seq(1,len)

            if(!is.null(basin)){
              ind <- which(basin[1,] == sds@basin)
              len <- length(ind)
            }

            #Get dimensions
            row <- dim(basin)[1]
            len <- dim(basin)[2]

            #Collect data
            data <- list(names  = ncdf4::ncvar_get(dataBase, sds@fields["names"])[ind],
                         seasons = ncdf4::ncvar_get(dataBase, sds@fields["seasons"])[ind],
                         isotimes = array(ncdf4::ncvar_get(dataBase, sds@fields["isoTime"])[,ind],
                                          dim = c(row,len)),
                         longitude = array(ncdf4::ncvar_get(dataBase, sds@fields["lon"])[,ind],
                                           dim = c(row,len)),
                         latitude = array(ncdf4::ncvar_get(dataBase, sds@fields["lat"])[,ind],
                                          dim = c(row,len)),
                         msw = array(ncdf4::ncvar_get(dataBase, sds@fields["msw"])[,ind],
                                     dim = c(row,len)),
                         rmw = array(ncdf4::ncvar_get(dataBase, sds@fields["rmw"])[,ind],
                                     dim = c(row,len)),
                         roci = array(ncdf4::ncvar_get(dataBase, sds@fields["roci"])[,ind],
                                      dim = c(row,len)),
                         pres = array(ncdf4::ncvar_get(dataBase, sds@fields["pressure"])[,ind],
                                      dim = c(row,len)),
                         poci = array(ncdf4::ncvar_get(dataBase, sds@fields["poci"])[,ind],
                                      dim = c(row,len)),
                         sshs = array(ncdf4::ncvar_get(dataBase, sds@fields["sshs"])[,ind],
                                      dim = c(row,len)))

            ncdf4::nc_close(dataBase)



            if(verbose)
              cat("=== DONE ===\n")

            sds@database <- data
            sds@loaded <- TRUE

            return(sds)

          })





# sds <- initDatabase(filename = "/home/baptiste/Desktop/Travail/StormR/data/IBTrACS.SP.v04r00.nc")
# sds <- collectData(sds)
