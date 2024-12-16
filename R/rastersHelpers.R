####################################
# Helpers for rasters computation  #
####################################


#' Compute MSW raster
#'
#' @noRd
#' @param speed SpatRaster stack. Wind speed raster for all time steps
#' @param nbg numeric. Number of cells to consider for the focal function
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#'
#' @return MSW SpatRaster
computeMSWRaster <- function(speed, nbg, name) {
  msw <- max(speed, na.rm = TRUE)
  # Applying focal function to smooth results
  msw <- terra::focal(msw, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = TRUE)
  names(msw) <- paste0(name, "_MSW")
  terra::time(msw) <- as.POSIXct(terra::time(speed)[[1]])

  return(msw)
}


#' Compute PDI raster
#'
#' @noRd
#' @param speed SpatRaster stack. Wind speed raster for all time steps
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param nbg numeric. Number of cells to consider for the focal function
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#'
#' @return PDI SpatRaster
computePDIRaster <- function(speed, tempRes, nbg, name) {
  pdi <- computePDI(speed, tempRes)
  # Applying focal function to smooth results
  pdi <- terra::focal(pdi, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = TRUE)
  names(pdi) <- paste0(name, "_PDI")
  terra::time(pdi) <- terra::time(speed[[1]])

  return(round(pdi, 3))
}


#' Compute Exposure raster
#'
#' @noRd
#' @param speed SpatRaster stack. Wind speed raster for all time steps
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param nbg numeric. Number of cells to consider for the focal function
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#' @param threshold numeric vector. Wind threshold
#'
#' @return Exposure SpatRaster
computeExposureRaster <- function(speed, tempRes, nbg, name, threshold) {
  exposure <- terra::rast(
    terra::ext(speed),
    res = terra::res(speed),
    vals = NA,
    nlyr = length(threshold)
  )
  for (l in seq_along(threshold)) {
    isGreaterSpeed <- terra::ifel(speed >= threshold[l], 1, NA)
    prod <- sum(isGreaterSpeed, na.rm = TRUE) * tempRes
    # Applying focal function to smooth results
    prod <- terra::focal(prod, w = matrix(1, nbg, nbg), mean, na.rm = TRUE)
    names(prod) <- paste0(name, "_Exposure_", threshold[l])
    terra::time(prod) <- terra::time(speed[[1]])
    exposure[[l]] <- prod
  }

  return(round(exposure, 3))
}


####################################
# Helpers for raster manipulation  #
####################################


#' Move the raster to the raster of the loi
#'
#' @noRd
#' @param raster SpatRaster. Raster to move
#' @param template SpatRaster. Raster of reference
#' @param extent SpatialExtent. Extent of the loi from the raster of reference
#'
#' @return SpatRaster. Raster moved to the template
moveOnLoi <- function(raster, template, extent) {
  # Move the raster to the raster of the loi
  return(terra::crop(terra::merge(raster, template), extent))
}


#' Whether or not to mask final computed products
#'
#' @noRd
#' @param finalStack SpatRaster stack. Where all final computed products are
#'   gathered
#' @param loi sf object. loi used as template to rasterize the mask
#' @param template SpatRaster. rasterTemplate generated with makeTemplateRaster
#'   function, used as a template to rasterize the mask
#'
#' @return finalStack masked or not
maskProduct <- function(finalStack, loi, template) {
  # Masking the stack to fit loi
  v <- terra::vect(loi)
  m <- terra::rasterize(v, template)
  return(terra::mask(finalStack, m))
}



####################################
# Helpers to make template rasters #
####################################


#' Initialize rasters for the computations
#'
#' Note: Special case for "Profiles" product, where no raster is initialized as
#' we don't know the number of observations yet.
#'
#' @noRd
#' @param template SpatRaster. Raster template generated with makeTemplateRaster
#' @param nbStorms numeric. Number of storms
#' @param product character or character vector. Products to compute from spatialBehaviour
#' @param thresholds numeric vector. Wind thresholds to compute the duration of exposure
#'
#' @return a list of rasters

initRasters <- function(template, nbStorms, product, thresholds) {
  mswRaster <- NULL
  pdiRaster <- NULL
  exposureRaster <- NULL
  profilesRaster <- NULL
  if ("MSW" %in% product) {
    mswRaster <- rep(template, nbStorms)
  }
  if ("PDI" %in% product) {
    pdiRaster <- rep(template, nbStorms)
  }
  if ("Exposure" %in% product) {
    exposureRaster <- rep(template, nbStorms * length(thresholds))
  }

  return(list(mswRaster = mswRaster,
              pdiRaster = pdiRaster,
              exposureRaster = exposureRaster,
              profilesRaster = profilesRaster))
}


#' Generate raster template
#'
#' @noRd
#' @param region "sf" object or data.frame with "xmin", "xmax", "ymin", "ymax".
#'  Defines the LOI + buffer extention
#' @param resolution numeric. Space resolution for the template
#' @param time string. Time of the template
#'
#' @return a SpatRaster
makeTemplateRaster <- function(region, res, time = NULL) {
  # Deriving the raster template
  if (is(region, "sf")) {
    ext <- sf::st_bbox(region)
  } else {
    ext <- region
  }

  template <- terra::rast(
    xmin = ext$xmin,
    xmax = ext$xmax,
    ymin = ext$ymin,
    ymax = ext$ymax,
    resolution = res,
    vals = NA,
  )
  if (!is.null(time)) {
    terra::time(template) <- as.POSIXct(time)
  }
  terra::origin(template) <- c(0, 0)

  return(template)
}


#' Generate a raster from a template raster with the coordinates of each cell.
#' The raster will have two layers: "lon" and "lat".
#'
#' @noRd
#' @param raster SpatRaster. Raster template
#'
#' @return a SpatRaster. Raster with two layers: "lon" and "lat"
makeCoordinatesRaster <- function(raster) {
  ncol <- terra::ncol(raster)
  nrow <- terra::nrow(raster)
  coords <- terra::crds(raster, na.rm = FALSE)
  coordsX <- matrix(coords[, 1], nrow = nrow, ncol = ncol, byrow = FALSE)
  coordsY <- matrix(coords[, 2], nrow = nrow, ncol = ncol, byrow = FALSE)
  rasterCoords <- rep(raster, 2)
  terra::values(rasterCoords) <- c(coordsX, coordsY)
  names(rasterCoords) <- c("lon", "lat")
  return(rasterCoords)
}

#' Generate a raster of the wind field (wind & direction)
#' from a template raster with the coordinates of each cell.
#'
#' @noRd
#' @param wind matrix. List describing the wind field with two fields: "speed" and "direction"
#' @param template SpatRaster. Raster template
#' @param name character. Name of the storm
#' @param index numeric. Index of the storm
#'
#' @return a SpatRaster. Raster with two layers: "speed" and "direction"
rasterizeWind <- function(wind, template, name, index) {
  # Rasterize the wind
  windRaster <- rep(terra::rast(template), 2)
  terra::values(windRaster) <- c(wind$speed, wind$direction)
  names(windRaster) <- c(paste0(name, "_Speed_", index), paste0(name, "_Direction_", index))
  return(windRaster)
}
