########################
# Helper to check inputs#
########################


#' Check inputs for computeTEW.SpatRaster function
#'
#' @noRd
#' @param profiles spatRaster
#' @param sts StormsList object
#' @param dtm object
#' @param angle numeric
#' @param product character
#' @param threshold numeric
#' @param verbose numeric
#' @return NULL


checkInputscomputeTEW.SpatRaster <- function(profiles, sts, dtm, angle, threshold, product, verbose) {
  # Checking sts input
  stopifnot("no data found" = !missing(sts))

  # Checking dtm input
  stopifnot("no dtm data found" = !missing(dtm))

  # Checking profiles input
  stopifnot("no profiles found" = !missing(profiles))
  stopifnot("'profiles' must be a SpatRaster object." = inherits(profiles, "SpatRaster"))
  stopifnot(
    "'profiles' must contain Profiles layers with '_Speed_' and '_Direction_' in their names." =
      length(grep("_Speed_", names(profiles))) > 0 &&
        length(grep("_Direction_", names(profiles))) > 0
  )


  # Checking product input
  stopifnot("Invalid product name" = product %in% c("TEW1wdMax", "TEW1wdMean", "TEW1wd", "TEW", "TEWIntegrated"))

  # Checking threshold input
  stopifnot("Threshold must be numeric" = identical(class(threshold), "numeric"))
  stopifnot("invalid value(s) for threshold input (must be >= 0)" = threshold >= 0)

  # Checking angle input
  stopifnot("Angle must be numeric" = identical(class(angle), "numeric"))
  stopifnot("invalid value(s) for angle input (must be >= 0)" = angle >= 0)

  # Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1 or 2" = verbose %in% c(0, 1, 2))
}

#' Check inputs for computeTEW.list function
#'
#' @noRd
#' @param profiles spatRaster
#' @param points data.frame
#' @param dtm object
#' @param angle numeric
#' @param product character
#' @param threshold numeric
#' @param verbose numeric
#' @return NULL


checkInputscomputeTEW.list <- function(profiles, points, dtm, angle, threshold, verbose) {
  # Checking sts input
  stopifnot("no data found" = !missing(points))

  # Checking dtm input
  stopifnot("no dtm data found" = !missing(dtm))

  # Checking profiles input
  stopifnot("no profiles found" = !missing(profiles))
  stopifnot("'profiles' must be a list of data.frames" = inherits(profiles, "list"))

  stopifnot(
    "'profiles' must contain 'direction' column for each data.frame of the list" =
      all(
        sapply(profiles, function(storm) {
          all(
            sapply(storm, function(df) {
              is.data.frame(df) &&
                "direction" %in% names(df)
            })
          )
        })
      )
  )

  # Checking angle input
  stopifnot("Angle must be numeric" = identical(class(angle), "numeric"))
  stopifnot("invalid value(s) for angle input (must be >= 0)" = angle >= 0)

  # Checking threshold input
  stopifnot("Threshold must be numeric" = identical(class(threshold), "numeric"))
  stopifnot("invalid value(s) for threshold input (must be >= 0)" = threshold >= 0)

  # Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1 or 2" = verbose %in% c(0, 1, 2))
}



#' Compute the topographic exposure to wind. 
#' In comparison to `terra::shade()`, this function
#' can work with a raster with multiple wind directions (one by point of the raster). It can
#' also work with numeric vectors or values.
#'
#' @noRd
#' @param slope SpatRaster / numeric with slope values (in radians)
#' @param aspect SpatRaster / numeric with aspect values (in radians)
#' @param angle inflection angle of the wind (in degrees, by default = 6°)
#' @param degreeDir SpatRaster / numeric with wind direction (in degrees)
#'
#' @return SpatRaster of the topographic exposure to wind

computeShade <- function(slope, aspect, angle = 6, degreeDir) {
  # resample so that loi match mnt
  # Seems not necessary
  # if (!terra::compareGeom(slope, rasterDir, stopOnError = FALSE)) {
  #  rasterDir <- terra::resample(rasterDir, slope, method = "near")
  # }

  # zenith angle in radians
  zDeg <- 90 - angle
  z <- zDeg * pi / 180
  radianDir <- degreeDir * pi / 180
  # lambert formula (shade equivalent)
  shade <- (cos(slope) * cos(z)) +
    (sin(slope) * sin(z) * cos(radianDir - aspect))

  return(shade)
}


#' Compute the topographic exposure with shade pixel by pixel
#' with maximum speed value
#'
#' @noRd
#'
#' @param profiles SpatRaster with wind profiles from spatialBehaviour
#' @param layersMSW SpatRaster with speed layers
#' @param layersDir SpatRaster with direction layers
#' @param topo SpatRaster with topographic characteristics (slope and aspect)
#' @param angle inflection angle of the wind (in degrees)
#' @param threshold numeric. wind threshold (in m/s)
#'
#'
#' @return SpatRaster of the topographic exposure integrated over the storm track
computeTEWIntegrated <- function(profiles, layersMSW, layersDir, topo, angle, threshold) {
  speedStack <- profiles[[layersMSW]]
  dirStack <- profiles[[layersDir]]

  # resample to mnt
  if (!terra::compareGeom(topo$slope, speedStack, stopOnError = FALSE)) {
    speedStack <- terra::resample(speedStack, topo$slope, method = "bilinear")
    dirStack <- terra::resample(dirStack, topo$slope, method = "near")
  }

  # compute layer with maximum speed for each pixel
  maxSpeedLayer <- terra::app(speedStack, fun = "max", na.rm = TRUE)

  # get index of layer where the speed is maximal
  idx <- terra::which.max(speedStack)
  # get assign direction
  dirAtMax <- terra::selectRange(dirStack, idx)

  # compute topographic exposition
  expRast <- computeShade(topo$slope, topo$aspect, angle, dirAtMax)
  expRast <- terra::ifel(maxSpeedLayer < threshold, NA, expRast)

  # expRast <- terra::mask(expRast, topo$slope)  # Seems not necessary

  return(expRast)
}



#' Get the direction value from maximum speed
#'
#' @noRd
#' @param refLayer speed layer from profile (type. NAME_Speed_n)
#' @param targetLayer direction layer (type. NAME_Direction_n)
#'
#' @return value of the direction in degrees

getValueMaxSpeed <- function(targetLayer, refLayer) {
  idCell <- terra::where.max(refLayer)[1, "cell"]
  val <- as.numeric(targetLayer[idCell])

  return(val)
}


#' Get final direction
#'
#' @noRd
#' @param speed_layer speed layer from profile (type. NAME_Speed_n)
#' @param dir_layer direction layer from profile (type. NAME_Direction_n)
#'
#' @return direction in azimuth

getWindDirection <- function(speed_layer, dir_layer) {
  dir <- getValueMaxSpeed(dir_layer, speed_layer)

  return(dir)
}

#' Get terrain data
#'
#' @param elevation SpatRaster with elevation data
#'
#' @return list of SpatRaster with slope and aspect, in radians

getTerrain <- function(elevation) {
  list(
    slope = terra::terrain(elevation, v = "slope", unit = "radians"),
    aspect = terra::terrain(elevation, v = "aspect", unit = "radians")
  )
}



#' Compute the topographic exposure with shade
#'
#' @noRd
#' @param slope SpatRaster with slope values (in radians)
#' @param aspect SpatRaster with aspect values (in radians)
#' @param angle numeric. inflection angle of the wind (in degrees)
#' @param direction numeric. wind direction (in degrees)
#'
#' @return SpatRaster of the topographic exposure

# computeShade <- function(slope, aspect, angle = 6, direction) {
#  if (is.na(direction)) {
#    return(NULL)
#  }
#
#  expRast <- terra::shade(slope, aspect, angle, direction)
#
#  return(expRast)
# }

#' Compute TEW Profiles for all observations
#'
#' @noRd
#' @param profiles SpatRaster. profiles from spatialBehaviour
#' @param layersMSW layers of speed from profiles
#' @param layersDir layers of direction from profiles
#' @param topo topography of the loi (slope and aspect)
#' @param angle numeric. angle of inflection (in degrees)
#' @param threshold numeric. speed threshold.
#' @param usePixel logical. Whether to use pixel-based TEW or not for wind directions.
#'  Default value is set to FALSE.
#'
#' @return Profiles of Exposure, one layer per observation


computeTEWProfiles <- function(profiles, layersMSW, layersDir, topo, angle, threshold, usePixel = FALSE) {
  topoList <- list()

  for (i in seq_along(layersMSW)) {
    maxSpeed <- terra::global(profiles[[layersMSW[i]]], "max", na.rm = TRUE)[1, 1]

    if (!is.na(maxSpeed) && maxSpeed >= threshold) {
      if (usePixel) {
        dirRaster <- profiles[[layersDir[i]]]
        if (!terra::compareGeom(topo$slope, dirRaster, stopOnError = FALSE)) {
          dirRaster <- terra::resample(dirRaster, topo$slope, method = "near")
        }
        expRast <- computeShade(topo$slope, topo$aspect, angle, dirRaster)
      } else {
        dir <- getWindDirection(profiles[[layersMSW[i]]], profiles[[layersDir[i]]])
        expRast <- terra::shade(topo$slope, topo$aspect, angle, direction = dir)
      }

      if (!is.null(expRast)) {
        varName <- if (usePixel) gsub("Speed_", "TEW_", layersMSW[i]) else gsub("Speed_", "TEW1wd_", layersMSW[i])

        names(expRast) <- varName
        topoList[[varName]] <- expRast
      }
    }
  }

  if (length(topoList) == 0) {
    return(NULL)
  }
  return(terra::rast(topoList))
}


############################
# function computeTEW #
############################

#' Compute the topographic exposure to wind
#'
#' The `computeTEW()` function allows computing topographic exposure to wind (TEW)
#' for either:
#' 1) each cell of a regular grid (i.e., a raster)
#' or
#' 2) a data.frame with the coordinates of the points of interest,
#' for a given tropical cyclone or a set of tropical cyclones.
#' @export
computeTEW <- function(profiles, ...) {
  UseMethod("computeTEW")
}


#' Compute the topographic exposure to wind
#'
#' The `computeTEW()` function allows computing topographic exposure to wind (TEW)
#' for each cell of a raster grid for a tropical cyclone or set of tropical cyclones.
#'
#' @param profiles SpatRaster. Wind speed and direction profiles from `spatialBehaviour()` function which must respect
#'  the following terminology : 'STORM_Speed_N', 'STORM_Direction_N'
#' @param sts StormsList object
#' @param dtm SpatRaster of the elevation data (Digital Terrain Model) for a given location
#' @param angle numeric. Inflection angle of the wind (in degrees). default is 6°.
#' @param threshold numeric. Minimum wind speed threshold (in m/s) requirred to compute exposure. default is 0
#' @param product character vector. Desired output statistics:
#'   \itemize{
#'     \item `"TEW1wd"`, TEW computation at each observation, one wind direction taken where wind speed is maximal.
#'      Good if your loi is small as wind is considered spatially homogeneous
#'     \item `"TEW1wdMax"`, computes `"TEW1wd"`, then takes the maximum of the layers
#'     \item `"TEW1wdMean"`, computes `"TEW1wd"`, then takes the mean of the layers
#'     \item `"TEW"`, TEW computation at each observation, wind direction varies spatially
#'     \item `"TEWIntegrated"`, TEW computation for maximum wind speed over the whole storm track for each spatial
#'     location of the raster. Use wind direction associated with the maximal wind speed (default)
#'   }
#' @param verbose numeric. Whether or not the function should display
#'        information about the process and/or outputs. Can be:
#' \itemize{
#'    \item `2`, information about the processes and outputs are displayed (default setting),
#'    \item `1`, information about the processes are displayed,
#'    \item `0`, no information displayed.
#' }
#' @return the function returns one layer for topographic exposure to wind (TEW)
#' for each observation or interpolated observation and each `Storm`.
#' The names of the layer follow the following terminology, the name of the storm in capital letters,
#' "TEW", and the indices of the observation separated by underscores
#' (e.g., "PAM_TEW_41", ...)
#' @export

computeTEW.SpatRaster <- function(profiles,
                                  sts,
                                  dtm,
                                  angle = 6,
                                  threshold = 0,
                                  product = "TEWIntegrated",
                                  verbose = 2) {
  startTime <- Sys.time()

  checkInputscomputeTEW.SpatRaster(
    profiles, sts, dtm, angle, threshold, product, verbose
  )

  if (verbose > 0) {
    cat("=== computeTEW processing ... ===\n")
    cat("Initializing data ...")
  }

  topo <- getTerrain(dtm)
  nbStorms <- getNbStorms(sts)

  # stack who will contains every storm
  finalStack <- c()

  if (verbose > 0) {
    cat(" Done\n\nComputation settings:\n")
    cat("  (*) Product(s) to compute:", paste(product, collapse = ", "), "\n")
    cat("\nStorm(s):\n")
    cat("  (", nbStorms, ") ", paste(getNames(sts), collapse = ", "), "\n\n")
  }

  for (i in 1:nbStorms) {
    stormName <- getNames(sts)[i]

    if (verbose > 0) cat("\n --> Computing for :", stormName, "...\n\n")

    # get speed and direction layers
    layersMSW <- names(profiles)[grep("_Speed_", names(profiles))]
    layersDir <- names(profiles)[grep("_Direction_", names(profiles))]

    # stack for one storm
    currentStormStack <- NULL

    if (any(c("TEW1wd", "TEW1wdMax", "TEW1wdMean") %in% product)) {
      exposureStack <- computeTEWProfiles(profiles, layersMSW, layersDir, topo, angle, threshold, usePixel = FALSE)

      if (is.null(exposureStack)) {
        warning("No layers met the wind speed threshold for : ", stormName)
        next
      }

      if (!is.null(exposureStack)) {
        if ("TEW1wd" %in% product) currentStormStack <- c(currentStormStack, exposureStack)

        if ("TEW1wdMax" %in% product) {
          finalStackMax <- terra::app(exposureStack, fun = "max", na.rm = TRUE)
          names(finalStackMax) <- paste0(stormName, "_TEW1wd_Max")
          currentStormStack <- c(currentStormStack, finalStackMax)
        }

        if ("TEW1wdMean" %in% product) {
          finalStackMean <- terra::app(exposureStack, fun = "mean", na.rm = TRUE)
          names(finalStackMean) <- paste0(stormName, "_TEW1wd_Mean")
          currentStormStack <- c(currentStormStack, finalStackMean)
        }
      }
    }


    if ("TEW" %in% product) {
      pixExposureStack <- computeTEWProfiles(profiles, layersMSW, layersDir, topo, angle, threshold, usePixel = TRUE)
      if (!is.null(pixExposureStack)) {
        currentStormStack <- c(currentStormStack, pixExposureStack)
      }
    }

    if ("TEWIntegrated" %in% product) {
      finalSummary <- computeTEWIntegrated(profiles, layersMSW, layersDir, topo, angle, threshold)
      if (!is.null(finalSummary)) {
        names(finalSummary) <- paste0(stormName, "_TEW_Integrated")
        currentStormStack <- c(currentStormStack, finalSummary)
      }
    }

    # stock the stack in global
    if (is.null(finalStack)) {
      finalStack <- currentStormStack
    } else {
      finalStack <- c(finalStack, currentStormStack)
    }

    if (verbose > 0) cat(" Done.")
  }

  if (is.null(finalStack)) {
    return(NULL)
  }
  finalStack <- terra::rast(finalStack)

  endTime <- Sys.time()

  if (verbose > 0) {
    cat("\n\n=== DONE with run time", round(as.numeric(endTime - startTime), 3), "sec ===\n")
  }

  if (verbose > 1) {
    cat("Output:\n")
    cat("SpatRaster stack with", terra::nlyr(finalStack), "layers:\n")
    cat("index - name of layers\n")
    n <- names(finalStack)
    names(n) <- seq(1, terra::nlyr(finalStack))
    for (i in seq_along(n)) {
      cat(" ", names(n[i]), "   ", n[i], "\n")
    }
    cat("\n")
  }

  return(finalStack)
}

#' Compute the topographic exposure to wind
#'
#' The `computeTEW()` function allows computing topographic exposure to wind (TEW)
#' for each points registred in a data.frame for a tropical cyclone or set of tropical cyclones.
#'
#' @param profiles list Wind speed and direction profiles from `temporalBehaviour()` function which must respect
#'  the following terminology : 'speed', 'direction' columns in a data.frame
#' @param points data.frame. Consisting of two columns names as "x" (for the longitude) and
#'  "y" (for the latitude), providing the coordinates in decimal degrees of the point locations.
#'  Same as the "points" argument of the `temporalBehaviour()` function
#' @param dtm SpatRaster of the elevation data (Digital Terrain Model) for a given location
#' @param angle numeric. Inflection angle of the wind (in degrees). default is 6°.
#' @param threshold numeric. Minimum wind speed threshold (in m/s) requirred to compute exposure. default is 0
#' @param verbose numeric. Whether or not the function should display
#'        information about the process and/or outputs. Can be:
#' \itemize{
#'    \item `2`, information about the processes and outputs are displayed (default setting),
#'    \item `1`, information about the processes are displayed,
#'    \item `0`, no information displayed.
#' }
#' @return the function returns a list of data.frame with one additional column for topographic exposure to wind (TEW)
#' for each observation or interpolated observation and each `Storm`.
#' @export

computeTEW.list <- function(profiles,
                            points,
                            dtm,
                            angle = 6,
                            threshold = 0,
                            verbose = 2) {
  startTime <- Sys.time()

  checkInputscomputeTEW.list(
    profiles, points, dtm, angle, threshold, verbose
  )

  if (verbose > 0) {
    cat("=== computeTEW processing ... ===\n")
    cat("Initializing data ...")
  }

  topo <- getTerrain(dtm)
  slope <- terra::extract(topo$slope, points)$slope
  aspect <- terra::extract(topo$aspect, points)$aspect
  nbStorms <- length(profiles)
  nbPoints <- nrow(points)
  pointsNames <- rownames(points)

  stopifnot(length(profiles[[1]]) == nbPoints)
  stopifnot(names(profiles[[1]]) == pointsNames)

  for (i in 1:nbStorms) {
    stormName <- names(profiles)[i]

    if (verbose > 0) cat("\n --> Computing for :", stormName, "...\n\n")

    for (j in 1:nbPoints) {
      pointName <- pointsNames[j]
      if (verbose > 0) cat("\n --> Point :", pointName, "...\n\n")

      winDir <- profiles[[i]][[j]]$direction

      tew <- computeShade(slope[j], aspect[j], angle = 6, winDir)

      profiles[[i]][[j]]$tew <- tew
    }

    if (verbose > 0) cat(" Done.")
  }

  endTime <- Sys.time()

  if (verbose > 0) {
    cat("\n\n=== DONE with run time", round(as.numeric(endTime - startTime), 3), "sec ===\n")
  }

  return(profiles)
}
