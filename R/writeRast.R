




#' Check inputs for writeRast function
#'
#' @noRd
#' @param rast SpatRaster
#' @param format character
#' @param filename character
#' @param path character
#'
#' @return NULL
checkInputsWr <- function(rast, format, filename, path){

  #Check rast input
  stopifnot("no data to write" = !missing(rast))

  #Check format input
  stopifnot("Invalid format" = format %in% c(".tiff", ".nc"))
  stopifnot("Only one format can be chosen" = length(format) == 1)

  #Check filenames
  if(!is.null(filename)){
    stopifnot("filename must be characters" = identical(class(filename), "character"))
  }

  #Check path
  stopifnot("path must be characters" = identical(class(path), "character"))

}





#' Exporting rasters to GeoTIFF or NetCDF files
#'
#' The `writeRast()` function exports rasters stored in SpatRaster objects to 
#' GeoTIFF or NetCDF files.
#'
#' @param rast `SpatRaster` object.
#' @param format character. Format of the file to export. Either `".tiff"` (GeoTIFF, default setting) or
#'   `".nc"` (NetCDF).
#' @param filename character. Output file name. By default `filename=NULL` and the name of the raster layer is used.
#' @param path character. Path to the directory where the file is exported to.
#' @returns `NULL`
#' @examples
#' \dontrun{
#' #Creating a StormsDataset
#' sds <- defDatabase()
#' 
#' #Getting storm track data for tropical cyclone Pam (2015) near Vanuatu and computing maximum sustained wind speed
#' pam <- Storms(sds = sds, loi = "Vanuatu", names = "PAM")
#' pam.msw <- spatialBehaviour(pam)
#' 
#' #Exporting maximum sustained wind speed raster layer to a GeoTIFF file
#' writeRast(pam.msw, path = paste0(tempdir(),"/"))
#'
#' #Computing power dissipation index for several storms near New Caledonia
#' sts.nc <- Storms(sds = sds, loi = "New Caledonia")
#' pdi.nc <- spatialBehaviour(sts.nc, product = "PDI")
#' 
#' #Exporting the power dissipation index raster layers to a NetCDF file
#' writeRast(pdi.nc, path = paste0(tempdir(),"/"))
#' }
#'
#' @export
writeRast <- function(rast, format = ".tiff", filename = NULL, path = "./"){


  checkInputsWr(rast, format, filename, path)

  name <- strsplit(names(rast), split = "_", fixed = TRUE)[[1]][1]
  product <- strsplit(names(rast), split = "_", fixed = TRUE)[[1]][2]

  if(format == ".nc"){
    if(product == "MSW"){
      varname <- "msw"
      unit <- "(m/s)"
      long_name <- "maximum sustained wind (m/s)"

    }else if (product == "PDI"){
      varname <- "pdi"
      unit <- "none"
      long_name <- "power dissipation index"

    }else if (product == "Exposure"){
      varname <- "exp"
      unit <- "none"
      long_name <- paste("Wind threshold exposure")

    }else if (stringr::str_detect(product,"Speed")){
      varname <- "rws"
      unit <- "(m/s)"
      long_name <- "radial wind speed"

    }
  }

  if(!is.null(filename)){
    f.name <- paste0(path, filename, format)
  }else{
    f.name <- paste0(path, names(rast), format)
  }

  if (format == ".tiff") {
    terra::writeRaster(x = rast,
                       filename = f.name,
                       overwrite = TRUE)

  } else if (format == ".nc") {
    terra::writeCDF(x = rast,
                    varname = product,
                    longname = long_name,
                    unit = unit,
                    filename = f.name,
                    overwrite = TRUE)
  }
}
