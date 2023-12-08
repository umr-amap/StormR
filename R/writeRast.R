




#' Check inputs for writeRast function
#'
#' @noRd
#' @param rast SpatRaster
#' @param filename character
#' @param path character
#'
#' @return NULL
checkInputsWriteRast <- function(rast, filename, path) {
  # Check rast input
  stopifnot("no data to write" = !missing(rast))

  # Check format input
  format <- strsplit(filename, split = ".", fixed = TRUE)[[1]][2]
  stopifnot("Invalid format" = format %in% c("tiff", "nc"))

  # Check filenames
  if (!is.null(filename)) {
    stopifnot("filename must be characters" = identical(class(filename), "character"))
  }

  # Check path
  stopifnot("path must be characters" = identical(class(path), "character"))
}

#' Write raster to netcdf file
#'
#' @noRd
#' @param rastds SpatRasterDataSet defined from `writeRast`
#' @param filename character. Output file name.
#'
#' @return NULL
writeNC <- function(rastds, filename) {
  n <- length(rastds)
  a <- rastds[[1]]
  xname <- "longitude"
  yname <- "latitude"
  xunit <- "degrees_east"
  yunit <- "degrees_north"
  xdim <- ncdf4::ncdim_def(xname, xunit, terra::xFromCol(a, seq_len(ncol(a))))
  ydim <- ncdf4::ncdim_def(yname, yunit, terra::yFromRow(a, seq_len(nrow(a))))

  zdimt <- list()
  firstExposure <- TRUE
  j <- 1
  for (i in 1:n) {
    lyr <- terra::varnames(rastds)[i]
    product <- strsplit(lyr, split = "_")[[1]][2]
    storm <- strsplit(lyr, split = "_")[[1]][1]
    if (product == "Speed") {
      zdimt[[j]] <- ncdf4::ncdim_def(paste0("time_", storm), "seconds since 1970-1-1 00:00:00",
        as.numeric(terra::time(rastds[lyr])),
        unlim = TRUE
      )
      j <- j + 1
    } else if (product == "Exposure" && firstExposure) {
      zdimExposure <- ncdf4::ncdim_def("Wind_thresholds", "m.s-1", sshs[1:6], unlim = FALSE)
      firstExposure <- FALSE
    }
  }

  missval <- -1.175494e38

  nc <- ncol(a)

  j <- 1
  ncvars <- list()
  for (i in 1:n) {
    lyr <- terra::varnames(rastds)[i]
    product <- strsplit(lyr, split = "_")[[1]][2]
    storm <- strsplit(lyr, split = "_")[[1]][1]
    if (product %in% c("MSW", "PDI")) {
      ncvars[[i]] <- ncdf4::ncvar_def(lyr, terra::units(rastds)[i],
        list(xdim, ydim), missval, terra::longnames(rastds)[i],
        prec = "float", compression = NA
      )
    } else if (product %in% c("Speed", "Direction")) {
      ncvars[[i]] <- ncdf4::ncvar_def(lyr, terra::units(rastds)[i],
        list(xdim, ydim, zdimt[[j]]), missval, terra::longnames(rastds)[i],
        prec = "float", compression = NA
      )
      if (product == "Direction") j <- j + 1
    } else if (product == "Exposure") {
      ncvars[[i]] <- ncdf4::ncvar_def(lyr, terra::units(rastds)[i],
        list(xdim, ydim, zdimExposure), missval, terra::longnames(rastds)[i],
        prec = "float", compression = NA
      )
    }
  }

  ncvars[[n + 1]] <- ncdf4::ncvar_def("crs", "", list(), NULL, prec = "integer")

  ncobj <- ncdf4::nc_create(filename, ncvars, force_v4 = TRUE, verbose = FALSE)
  on.exit(ncdf4::nc_close(ncobj))

  haveprj <- FALSE
  prj <- terra::crs(rastds[1])
  prj <- gsub("\n", "", prj)
  if (prj != "") {
    haveprj <- TRUE
    ncdf4::ncatt_put(ncobj, ncvars[[n + 1]], "crs_wkt", prj, prec = "text")
    # need for older gdal?
    ncdf4::ncatt_put(ncobj, ncvars[[n + 1]], "spatial_ref", prj, prec = "text")
    prj <- terra::crs(rastds[1], proj = TRUE)
    if (prj != "") {
      ncdf4::ncatt_put(ncobj, ncvars[[n + 1]], "proj4", prj, prec = "text")
    }
    prj <- terra::crs(rastds[1], describe = TRUE)[1, 3]
    if (!is.na(prj)) {
      ncdf4::ncatt_put(ncobj, ncvars[[n + 1]], "epsg_code", prj, prec = "text")
    }
  }

  e <- terra::ext(rastds)
  rs <- terra::res(rastds)
  gt <- paste(trimws(formatC(as.vector(c(e$xmin, rs[1], 0, e$ymax, 0, -1 * rs[2])), 22)), collapse = " ")
  ncdf4::ncatt_put(ncobj, ncvars[[n + 1]], "geotransform", gt, prec = "text")

  for (i in 1:n) {
    a <- rastds[i]
    terra::readStart(a)
    b <- terra::blocks(a, 4)
    if (length(ncvars[[i]]$dim) == 3) {
      for (j in 1:b$n) {
        d <- terra::readValues(a, b$row[j], b$nrows[j], 1, nc, FALSE, FALSE)
        d[is.nan(d)] <- NA
        d <- array(d, c(nc, b$nrows[j], terra::nlyr(rastds[i])))
        ncdf4::ncvar_put(ncobj, ncvars[[i]], d,
          start = c(1, b$row[j], 1),
          count = c(nc, b$nrows[j], terra::nlyr(rastds[i]))
        )
      }
    } else {
      for (j in 1:b$n) {
        d <- terra::readValues(a, b$row[j], b$nrows[j], 1, nc, FALSE, FALSE)
        d[is.nan(d)] <- NA
        d <- matrix(d, ncol = b$nrows[j])
        ncdf4::ncvar_put(ncobj, ncvars[[i]], d, start = c(1, b$row[j]), count = c(nc, b$nrows[j]))
      }
    }
    terra::readStop(a)
    if (haveprj) {
      ncdf4::ncatt_put(ncobj, ncvars[[i]], "grid_mapping", "crs", prec = "text")
    }
  }



  ncdf4::ncatt_put(ncobj, 0, "Conventions", "CF-1.4", prec = "text")
  pkgversion <- drop(read.dcf(file = system.file("DESCRIPTION", package = "terra"), fields = c("Version")))
  ncdf4::ncatt_put(ncobj, 0, "created_by",
    paste("R packages ncdf4 and terra (version ", pkgversion, ")", sep = ""),
    prec = "text"
  )
  ncdf4::ncatt_put(ncobj, 0, "date", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), prec = "text")
}


#' Exporting rasters to GeoTIFF or NetCDF files
#'
#' The `writeRast()` function exports rasters stored in SpatRaster objects to
#' GeoTIFF or NetCDF files.
#'
#' @param rast `SpatRaster` object.
#' @param filename character. Output file name. Can be either a `".tiff"` file (GeoTIFF) or `".nc"` (NetCDF).
#'  By default `filename=NULL` and the name of the file is generated based on
#'  raster information (storms and products) with the '.tiff' extension.
#' @param path character. Path to the directory where the file is exported to. By default `path=./'`.
#' @param ... Additional arguments to be passed to
#'  [`terra::writeRaster`](https://rdrr.io/cran/terra/man/writeRaster.html)
#'  function when saving to `".tiff"` format.
#' @returns `NULL`
#' @examples
#' # Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
#'
#' # Getting storm track data for tropical cyclone Pam (2015) near Vanuatu
#' pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#'
#' # Computing maximum sustained wind speed
#' pam.msw <- spatialBehaviour(pam)
#'
#' # Exporting maximum sustained wind speed raster layer to a GeoTIFF file
#' writeRast(pam.msw, path = paste0(tempdir(), "/"))
#'
#' # Computing power dissipation index for several storms near New Caledonia
#' sts.nc <- defStormsList(sds = sds, loi = "New Caledonia")
#' pdi.nc <- spatialBehaviour(sts.nc, product = "PDI")
#'
#' # Exporting the power dissipation index raster layers to a NetCDF file
#' writeRast(pdi.nc, path = paste0(tempdir(), "/"))
#' }
#' @export
writeRast <- function(rast, filename = NULL, path = "./", ...) {
  productsInfo <- strsplit(names(rast), split = "_", fixed = TRUE)
  varNames <- unique(rapply(productsInfo, function(x) paste0(x[1:2], collapse = "_")))
  # Gather all storms inside raster
  stormNames <- unique(lapply(varNames, function(x) strsplit(x, split = "_", fixed = TRUE)[[1]][1]))
  # Gather all products type computed inside raster
  products <- lapply(varNames, function(x) strsplit(x, split = "_", fixed = TRUE)[[1]][2])

  # Define output filename
  if (!is.null(filename)) {
    fName <- filename
  } else {
    fName <- paste0(paste(stormNames, collapse = "_"), "_", paste(unique(products), collapse = "_"), ".tiff")
  }

  checkInputsWriteRast(rast, fName, path)

  # Check output format
  format <- strsplit(fName, split = ".", fixed = TRUE)[[1]][2]

  if (format == "tiff") {
    # Write tiff file. Easy !
    terra::writeRaster(
      x = rast,
      filename = paste0(path, fName),
      ...
    )
  } else if (format == "nc") {
    # Write netcdf file. More tricky !
    varUnits <- c()
    varLongnames <- c()
    # We separate all variables by Storm and product
    for (p in products) {
      if (p == "MSW") {
        varUnits <- c(varUnits, "m.s-1")
        varLongnames <- c(varLongnames, "maximum sustained wind")
      } else if (p == "PDI") {
        varUnits <- c(varUnits, "none")
        varLongnames <- c(varLongnames, "power dissipation index")
      } else if (p == "Exposure") {
        varUnits <- c(varUnits, "none")
        varLongnames <- c(varLongnames, "Wind threshold exposure")
      } else if (p == "Speed") {
        varUnits <- c(varUnits, "m.s-1")
        varLongnames <- c(varLongnames, "radial wind speed")
      } else if (p == "Direction") {
        varUnits <- c(varUnits, "degree")
        varLongnames <- c(varLongnames, "wind direction")
      }
    }

    # We recreate a spatRasterDataset, one subDataset is one product for one storm
    splitVector <- c()
    for (i in 1:terra::nlyr(rast)) {
      ind <- which(paste(c(strsplit(names(rast[[i]]), split = "_", fixed = TRUE)[[1]][1:2]),
        collapse = "_"
      ) == varNames)
      splitVector <- c(splitVector, ind)
    }
    rastds <- terra::sds(terra::split(x = rast, f = splitVector))

    terra::varnames(rastds) <- varNames
    terra::units(rastds) <- varUnits
    terra::longnames(rastds) <- varLongnames

    # We write the netcdf file
    writeNC(rastds, filename = paste0(path, fName))
  }
}
