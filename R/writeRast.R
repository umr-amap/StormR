


#' Save a raster in the desired format
#'
#' This function writes a SpatRast raster in the given format
#'
#' @param rast SpatRast object.
#' @param format character. Format of the file to export `rast`. Either `.tiff`
#' or `.nc`. Default value is set to `.tiff`
#' @param filename character. Name of the file. Default value is set to `NULL`,
#' in this case it will be set to `names(rast)`
#' @param path character. Relative path where the file should be written
#'
#' @returns NULL
#' @export
writeRast = function(rast,
                     format = ".tiff",
                     filename = NULL,
                     path = "./") {
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

  name = strsplit(names(rast), split = "_", fixed = TRUE)[[1]][1]
  product = strsplit(names(rast), split = "_", fixed = TRUE)[[1]][2]

  if(format == ".nc"){
    if(product == "MSW"){
      varname = "msw"
      unit = "(m/s)"
      longname = "maximum sustained wind (m/s)"

    }else if (product == "PDI"){
      varname = "pdi"
      unit = "none"
      longname = "power dissipation index"

    }else if (stringr::str_detect(product,"Exposure")){
      c = stringr::str_sub(product,9,nchar(product))
      varname = "exp"
      unit = "none"
      longname = paste("category",c,"exposure")

    }else if (stringr::str_detect(product,"profile")){
      varname = "rws"
      unit = "(m/s)"
      longname = "radial wind speed"

    }
  }

  if(!is.null(filename)){
    f.name = paste0(path, filename, format)
  }else{
    f.name = paste0(path, names(rast), format)
  }

  if (format == ".tiff") {
    terra::writeRaster(x = rast,
                       filename = f.name,
                       overwrite = TRUE)

  } else if (format == ".nc") {
    terra::writeCDF(x = rast,
                    varname = product,
                    longname = longname,
                    unit = unit,
                    filename = f.name,
                    overwrite = TRUE)
  }
}
