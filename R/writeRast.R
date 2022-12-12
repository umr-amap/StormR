




#' Write a SpatRast raster in the given format
#'
#' @param rast SpatRast object we wish to write
#' @param format format of the file we want to export the `rast`
#' @param filename name of the file
#' @param path the relative path where the file shoule be written
#'
#' @return NULL
#' @export


writeRast = function(rast,
                     format = ".tiff",
                     filename = NULL,
                     path = "./") {
  #Check rast input
  stopifnot("no data to write" = !missing(rast))

  #Check format input
  stopifnot("Invalid format" = format %in% c(".tiff", ".nc"))

  #Check filenames
  if(!is.null(filename)){
    stopifnot("filename must be characters" = identical(class(filename), "character"))
  }

  #Check path
  stopifnot("path must be characters" = identical(class(path), "character"))


  name = strsplit(names(rast), split = "_", fixed = TRUE)[[1]][1]
  product = strsplit(names(rast), split = "_", fixed = TRUE)[[1]][2]

  if(!is.null(filename)){
    f.name = paste(path, filename, format)
  }else{
    f.name = paste(path, names(rast), format)
  }

  if (format == ".tiff") {
    terra::writeRaster(x = rast,
                       filename = f.name,
                       overwrite = TRUE)
  } else if (format == ".nc") {
    terra::writeCDF(x = rast,
                    varname = product,
                    filename = f.name,
                    overwrite = TRUE)
  }
}
