########################
# Helper to check inputs#
########################


#' Check inputs for computeTEW function
#'
#' @noRd
#' @param sts StormsList object
#' @param dtm object
#' @param spProfiles spatRaster 
#' @param angle numeric
#' @param product character
#' @param threshold numeric
#' @param spaceRes character
#' @param tempRes numeric
#' @param verbose numeric
#' @return NULL


checkInputscomputeTEW <- function(sts, dtm, spProfiles, angle, threshold, product, spaceRes, tempRes, verbose) {
  # Checking sts input
  stopifnot("no data found" = !missing(sts))
  
  # Checking dtm input
  stopifnot("no dtm data found" = !missing(dtm))
  
  # Checking spProfiles input
  if (!is.null(spProfiles)) {
    stopifnot("'spProfiles' must be a SpatRaster object." = inherits(spProfiles, "SpatRaster"))
    stopifnot("'spProfiles' must contain Profiles layers with '_Speed_' and '_Direction_' in their names." = 
                length(grep("_Speed_", names(spProfiles))) > 0 && 
                length(grep("_Direction_", names(spProfiles))) > 0)
  }
  
  # Checking product input
  stopifnot("Invalid product" = product %in% c("Max", "Mean", "Profiles","PixProfiles","Summary"))
  
  # Checking threshold input
  stopifnot("Threshold must be numeric" = identical(class(threshold), "numeric"))
  stopifnot("invalid value(s) for threshold input (must be >= 0)" = threshold >= 0)
  
  # Checking angle input 
  stopifnot("Angle must be numeric" = identical(class(angle), "numeric"))
  stopifnot("invalid value(s) for angle input (must be >= 0)" = angle >= 0)
  
  # Checking spaceRes input
  stopifnot("spaceRes must be character" = identical(class(spaceRes), "character"))
  stopifnot("spaceRes must be length 1" = length(spaceRes) == 1)
  stopifnot(
    "invalid spaceRes: must be either 30s, 2.5min, 5min or 10min" =
      spaceRes %in% c("30sec", "2.5min", "5min", "10min")
  )
  
  # Checking tempRes input
  stopifnot("tempRes must be numeric" = identical(class(tempRes), "numeric"))
  stopifnot("tempRes must be length 1" = length(tempRes) == 1)
  stopifnot("invalid tempRes: must be either 60, 30 or 15" = tempRes %in% c(60, 30, 15))
  
  # Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1 or 2" = verbose %in% c(0, 1, 2))
}



#' Compute the topographic exposure profiles with shade pixel by pixel
#' 
#' @noRd
#' @param slope SpatRaster with slope values (in radians)
#' @param aspect SpatRaster with aspect values (in radians)
#' @param angle inflection angle of the wind (in degrees, by default = 6°)
#' @param direction SpatRaster with wind direction (in degrees)
#'
#'@return SpatRaster of the topographic exposure

computePixelShade <- function(slope, aspect, angle = 6, rasterDir) {
  
  #resample so that loi match mnt
  if (!terra::compareGeom(slope, rasterDir, stopOnError = FALSE)) {
    rasterDir <- terra::resample(rasterDir, slope, method = "near")
  }
  #zenith angle in radians
  ZDeg <- 90 - angle
  Z <- ZDeg * pi / 180
  radianDir <- rasterDir * pi / 180
  #lambert formula (shade equivalent)
  exposureRaster <- (cos(slope) * cos(Z)) + 
    (sin(slope) * sin(Z) * cos(radianDir - aspect))
  
  return(exposureRaster)
}


#' Compute the topographic exposure with shade pixel by pixel
#' with maximum speed value
#' 
#' @noRd
#' 
#' @param pf SpatRaster with profiles from spatialBehaviour
#' @param layersMSW SpatRaster with speed layers
#' @param layersDir SpatRaster with direction layers
#' @param topo SpatRaster with topographic characteristics (slope and aspect)
#' @param angle inflection angle of the wind (in degrees, by default = 6°)
#' @param threshold numeric. wind threshold (in m/s) 
#'
#'
#'@return SpatRaster of the topographic exposure summary
computeSummary <- function(pf, layersMSW, layersDir, topo, angle, threshold) {
  
  speedStack <- pf[[layersMSW]]
  dirStack <- pf[[layersDir]]
  
  # resample to mnt
  if (!terra::compareGeom(topo$slope, speedStack, stopOnError = FALSE)) {
    speedStack <- terra::resample(speedStack, topo$slope, method = "bilinear")
    dirStack <- terra::resample(dirStack, topo$slope, method = "near")
  }
  
  #compute layer with maximum speed for each pixel
  maxSpeedLayer <- terra::app(speedStack, fun = "max", na.rm = TRUE)
  
  #get index of layer where the speed is maximal
  idx <- terra::which.max(speedStack)
  #get assign direction
  dirAtMax <- terra::selectRange(dirStack, idx)
  
  #compute topographic exposition
  expRast <- computePixelShade(topo$slope, topo$aspect, angle, dirAtMax)
  expRast <- terra::ifel(maxSpeedLayer < threshold, NA, expRast)
  
  expRast <- terra::mask(expRast, topo$slope)
  
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
  idCell <- terra::where.max(refLayer)[1,"cell"]
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
#' @noRd
#' @param dtm File of elevation data .tif 
#' 
#' @return compute slope and aspect

getTerrain <- function(dtm) {
  list(
    slope = terra::terrain(dtm, v = "slope", unit = "radians"),
    aspect = terra::terrain(dtm, v = "aspect", unit = "radians")
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
#'@return SpatRaster of the topographic exposure

computeShade <- function(slope, aspect, angle = 6, direction) {
  if (is.na(direction)){
    return(NULL)
  }
  
  expRast <- terra::shade(slope, aspect, angle, direction)
  
  return(expRast)
}

#' Compute Profiles Exposure
#' 
#' @noRd
#' @param pf SpatRaster. profiles from spatialBehaviour
#' @param layersMSW layers of speed from pf
#' @param layersDir layers of direction from pf
#' @param topo topography of the loi (slope and aspect)
#' @param angle numeric. angle of inflection (6°)
#' @param threshold numeric. speed threshold. default is 0
#' 
#' @return Profiles of Exposure, one layer per observation


computeExpProfiles <- function(pf, layersMSW, layersDir, topo, angle, threshold, usePixel = FALSE) {
  topoList <- list()
  
  for (i in seq_along(layersMSW)) {
    maxSpeed <- terra::global(pf[[layersMSW[i]]], "max", na.rm = TRUE)[1, 1]
    
    if (!is.na(maxSpeed) && maxSpeed >= threshold) {
      
      if (usePixel) {
        dirRaster <- pf[[layersDir[i]]]
        expRast <- computePixelShade(topo$slope, topo$aspect, angle, dirRaster)
      } else {
        dir <- getWindDirection(pf[[layersMSW[i]]], pf[[layersDir[i]]])
        expRast <- computeShade(topo$slope, topo$aspect, angle, direction = dir)
      }
      
      if (!is.null(expRast)){
        varName <- gsub("Speed_", "TEW_", layersMSW[i])
        if (usePixel) varName <- paste0(varName, "_Pix")
        
        names(expRast) <- varName
        topoList[[varName]] <- expRast
      }
    }
  }
  
  if (length(topoList) == 0) return(NULL)
  return(terra::rast(topoList))
}


############################
# function computeTEW #
############################





#' Compute the topographic exposure to wind 
#' 
#' The `computeTEW()` function allows computing topographic exposure to wind (TEW)
#' for each cell of a regular grid (i.e., a raster) for a given tropical cyclone 
#' or set of tropical cyclones.
#' 
#' @param sts StormsList object
#' @param dtm character. Name of the .tiff file which contains elevation data (Digital Terrain Model) for a given location. 
#' @param spProfiles SpatRaster. Profiles of `spatialBehaviour()` function. Default is NULL. 
#' @param angle numeric. Inflection angle of the wind (in degrees). default is 6°. 
#' @param threshold numeric. Minimum wind speed threshold (in m/s) requirred to compute exposure. default is 0
#' @param product character vector. Desired output statistics:
#'   \itemize{
#'     \item `"Profiles"`, for exposure at each observation,
#'     \item `"Max"`, for maximum exposure, or
#'     \item `"Mean"`, for mean exposure (default)
#'     \item `"PixProfiles"`, for exposure at each observation with one direction by pixel
#'     \item `"Summary"`, for the summary of exposure when speed was maximal in each pixel
#'   }
#' @param spaceRes character. Spatial resolution. Can be `"30 sec"` (~1 km at the equator),
#' `"2.5 min"` (~4.5 km at the equator), `"5 min"` (~9 km at the equator) or `"10 min"` (~18.6 km at the equator).
#'  Default setting is `"2.5 min"`.
#' @param tempRes numeric. Temporal resolution (min). Can be `60` ( default setting),
#'   `30` or `15`.
#' @param verbose numeric. Whether or not the function should display 
#'        information about the process and/or outputs. Can be:
#' \itemize{
#'    \item `2`, information about the processes and outputs are displayed (default setting),
#'    \item `1`, information about the processes are displayed, pr
#'    \item `0`, no information displayed.
#' 
#' @return the function returns one layer for topographic exposure to wind (TEW)
#' for each observation or interpolated observation and each `Storm`.
#' The names of the layer follow the following terminology, the name of the storm in capital letters,
#' "Exposure", and the indices of the observation separated by underscores
#' (e.g., "PAM_Exposure_41", ...)
#' @export

computeTEW <- function(sts, dtm, 
                            spProfiles = NULL,
                            angle = 6, 
                            threshold = 0, 
                            product = "Summary", 
                            spaceRes = "2.5min",
                            tempRes = 60,
                            verbose = 2) {
  startTime <- Sys.time()
  
  checkInputscomputeTEW(
    sts, dtm, spProfiles, angle, threshold, product, spaceRes, tempRes, verbose
  )
  
  if (verbose > 0) {
    cat("=== computeTEW processing ... ===\n")
    cat("Initializing data ...")
  }
  
  topo <- getTerrain(dtm)
  nbStorms <- getNbStorms(sts)
  
  # stack who will contains every storm
  finalStack <- c() 
  
  for (i in 1:nbStorms) {
    
    stormName <- getNames(sts)[i]
    
    if (verbose > 0) cat("\n --> Computing for :", stormName, "...\n\n")
    
    # extracting information for one storm 
    st <- sts
    st@data <- list(sts@data[[i]])
    
    # compute or reuse spatial profiles
    if (!is.null(spProfiles)) {
      pf <- spProfiles
    } else {
      pf <- spatialBehaviour(st, product = "Profiles",
                             tempRes = tempRes, 
                             spaceRes = spaceRes, 
                             verbose = 1)
    }

    
    layersMSW <- names(pf)[grep("_Speed_", names(pf))]
    layersDir <- names(pf)[grep("_Direction_", names(pf))]
    
      if (verbose > 0) {
    cat(" Done\n\nComputation settings:\n")
    cat("  (*) Temporal resolution: Every", tempRes, " minutes\n")
    cat("  (*) Space resolution:", names(resolutions[spaceRes]), "\n")
    cat("  (*) Product(s) to compute:", paste(product, collapse = ", "), "\n")
    cat("\nStorm(s):\n")
    cat("  (", nbStorms, ") ", paste(getNames(sts), collapse = ", "), "\n\n")
  }
    
    
    # stack for one storm
    currentStormStack <- NULL
    
    
    if (any(c("Profiles", "Max", "Mean") %in% product)) {
      exposureStack <- computeExpProfiles(pf, layersMSW, layersDir, topo, angle, threshold, usePixel = FALSE)
      
      if (is.null(exposureStack)) {
        warning("No layers met the wind speed threshold for : ", stormName)
        next
      }
      
      if (!is.null(exposureStack)) {
        if ("Profiles" %in% product) currentStormStack <- c(currentStormStack, exposureStack)
        
        if ("Max" %in% product) {
          finalStackMax <- terra::app(exposureStack, fun = "max", na.rm = TRUE)
          names(finalStackMax) <- paste0(stormName, "_TEW_Max")
          currentStormStack <- c(currentStormStack, finalStackMax)
        }
        
        if ("Mean" %in% product) {
          finalStackMean <- terra::app(exposureStack, fun = "mean", na.rm = TRUE)
          names(finalStackMean) <- paste0(stormName, "_TEW_Mean")
          currentStormStack <- c(currentStormStack, finalStackMean)
        }
      }
    }
    
    
    if ("PixProfiles" %in% product) {
      pixExposureStack <- computeExpProfiles(pf, layersMSW, layersDir, topo, angle, threshold, usePixel = TRUE)
      if (!is.null(pixExposureStack)) {
        currentStormStack <- c(currentStormStack, pixExposureStack)
      }
    }    
    
    if ("Summary" %in% product) {
      finalSummary <- computeSummary(pf, layersMSW, layersDir, topo, angle,threshold)
      if (!is.null(finalSummary)) {
        names(finalSummary) <- paste0(stormName, "_TEW_Summary")
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
  
  if (is.null(finalStack)) return(NULL)
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



