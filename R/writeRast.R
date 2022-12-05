




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
                     filename = "rast",
                     path = "./") {
  #Check rast input
  stopifnot("no data to write" = !missing(rast))

  #Check format input
  stopifnot("Invalid format" = format %in% c(".tiff", ".nc"))

  #Check filename
  stopifnot("filename must be characters" = identical(class(filename), "character"))

  #Check path
  stopifnot("path must be characters" = identical(class(path), "character"))


  f.name = paste(path, filename, format)
  if (format == ".tiff") {
    terra::writeRaster(rast, f.name, overwrite = TRUE)
  } else if (format == "nc") {
    terra::writeCDF(rast, f.name, overwrite = TRUE)
  }
}
