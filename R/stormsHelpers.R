##########################################
# Functions to help with the storms data #
##########################################


#' Get indices of storm records for computations
#'
#' Whether to get only observations inside LOI + buffer extention (+ offset) or
#' getting all the observations
#'
#' @noRd
#' @param st Storm Object
#' @param offset numeric. Offset to apply at the begining and at the end
#' @param product character. product input from spatialBehaviour
#'
#' @return numeric vector gathering the indices of observation to use to perform
#'   the further computations
getIndices <- function(st, offset, product) {
  # Use observations within the loi for the computations
  ind <- seq(st@obs[1], st@obs[length(st@obs)], 1)

  if ("MSW" %in% product || "PDI" %in% product || "Exposure" %in% product) {
    # Handling indices and offset (outside of loi at entry and exit)
    for (o in 1:offset) {
      ind <- c(st@obs[1] - o, ind)
      ind <- c(ind, st@obs[length(st@obs)] + o)
    }

    # Remove negative values and values beyond the number of observations
    ind <- ind[ind > 0 & ind <= getNbObs(st)]
  }

  return(ind)
}




#' Get data associated with one storm to perform further computations
#'
#' @noRd
#' @param st Storm object
#' @param indices numeric vector extracted from getIndices
#' @param tempRes numeric. time step for interpolated data, in minutes
#' @param empiricalRMW logical. Whether to use rmw from the data or to compute
#'   them according to getRmw function
#' @param method character. method input from spatialBehaviour
#'
#' @return a data.frame of dimension length(indices) : 9. Columns are
#'  \itemize{
#'    \item lon: numeric. Longitude coordinates (degree)
#'    \item lat: numeric. Latitude coordinates (degree)
#'    \item msw: numeric. Maximum Sustained Wind (m/s)
#'    \item rmw: numeric. Radius of Maximum Wind (km)
#'    \item pc: numeric. Pressure at the center of the storm (mb)
#'    \item poci: numeric. Pressure of Outermost Closed Isobar (mb)
#'    \item stormSpeed: numeric. Velocity of the storm (m/s)
#'    \item vxDeg: numeric. Velocity of the speed in the x direction (deg/h)
#'    \item vyDeg: numeric Velocity of the speed in the y direction (deg/h)
#'  }
getDataInterpolate <- function(st, indices, tempRes, empiricalRMW, method) {
  # Nb of observations of storm and time associated
  lenIndices <- length(indices)
  timeObs <- st@obs.all$iso.time[indices]
  # If data has irregular temporal resolution, we have to find the gcd of the time series
  timeStepData <- as.numeric(difftime(timeObs[2:lenIndices],
                                      timeObs[1:lenIndices - 1],
                                      units = "mins"))
  gcd2 <- function(a, b) {
    if (b == 0) a else Recall(b, a %% b)
  }
  gcd <- function(...) Reduce(gcd2, c(...))
  timeStepDataGCD <- gcd(timeStepData)
  # Determine temporal interpolation time step
  interpolatedRes <- min(timeStepDataGCD, tempRes)

  # Get the total time of the storm (in mins)
  timeInit <- timeObs[1]
  timeEnd <- timeObs[lenIndices]
  timeDiffObs <- as.numeric(difftime(timeEnd,
                                     timeInit,
                                     units = "mins"))

  # Deal with length and time of interpolated data
  lenInterpolated <- as.integer(timeDiffObs / interpolatedRes) + 1
  timeInterpolated <- format(seq.POSIXt(as.POSIXct(timeInit),
                                        as.POSIXct(timeEnd),
                                        by = paste0(interpolatedRes, " min")),
                             "%Y-%m-%d %H:%M:%S")
  indicesObsInterpolated <- match(timeObs, timeInterpolated)
  if (interpolatedRes == tempRes) {
    # Case where interpolation is done at the frequency requested by the user
    timeData <- timeInterpolated
    indicesFinal <- seq(1, lenInterpolated)
  } else {
    # Case where interpolation time is smaller than requested by user
    # (if the dataset has really short observation intervals,
    # usualy when irregular observations).
    timeData <- format(seq.POSIXt(as.POSIXct(timeInit),
                                  as.POSIXct(timeEnd),
                                  by = paste0(tempRes, " min")),
                       "%Y-%m-%d %H:%M:%S")
    indicesFinal <- match(timeData, timeInterpolated)
  }

  # Initiate the final data.frame
  data <- data.frame(
    lon = rep(NA, lenInterpolated),
    lat = rep(NA, lenInterpolated),
    stormSpeed = rep(NA, lenInterpolated),
    vxDeg = rep(NA, lenInterpolated),
    vyDeg = rep(NA, lenInterpolated),
    msw = rep(NA, lenInterpolated),
    rmw = rep(NA, lenInterpolated),
    indices = rep(NA, lenInterpolated),
    isoTimes = rep(NA, lenInterpolated)
  )

  # Filling indices and isoTimes
  ind <- c()
  for (i in seq(1, lenInterpolated)) {
    timeIntervals <- as.numeric(difftime(timeObs,
                                         timeInterpolated[i],
                                         units = "mins"))
    # Case of interpolation time equal to observation time
    indObs <- which(timeIntervals > 0)[1]
    ind <- c(ind,
      formatC(indices[[1]] - 1 + indObs - timeIntervals[indObs] / (timeIntervals[indObs] - timeIntervals[indObs - 1]),
              digits = 2,
              format = "f")
    )
  }
  # When interpolation time matches observation time, we keep "integer" indices
  ind <- gsub(".00", "", ind)

  data$indices <- ind
  data$isoTimes <- timeInterpolated

  # Get lon & lat
  lon <- st@obs.all$lon[indices]
  lat <- st@obs.all$lat[indices]

  stormSpeed <- rep(NA, lenIndices)
  vxDeg <- rep(NA, lenIndices)
  vyDeg <- rep(NA, lenIndices)

  # Computing storm velocity (m/s)
  for (i in 1:(lenIndices - 1)) {
    stormSpeed[i] <- terra::distance(
      x = cbind(lon[i], lat[i]),
      y = cbind(lon[i + 1], lat[i + 1]),
      lonlat = TRUE
    ) * (0.001 / 3) / 3.6

    # component wise velocity in both x and y direction (degree/h)
    vxDeg[i] <- (lon[i + 1] - lon[i]) / 3
    vyDeg[i] <- (lat[i + 1] - lat[i]) / 3
  }

  # Prepare all fields
  data$msw[indicesObsInterpolated] <- st@obs.all$msw[indices]
  data$lon[indicesObsInterpolated] <- lon
  data$lat[indicesObsInterpolated] <- lat
  data$stormSpeed[indicesObsInterpolated] <- stormSpeed
  data$vxDeg[indicesObsInterpolated] <- vxDeg
  data$vyDeg[indicesObsInterpolated] <- vyDeg

  if (empiricalRMW) {
    data$rmw[indicesObsInterpolated] <- getRmw(data$msw[indicesObsInterpolated], lat)
  } else {
    if (!("rmw" %in% colnames(st@obs.all)) || (all(is.na(st@obs.all$rmw[indices])))) {
      warning("Missing rmw data to perform model. empiricalRMW set to TRUE")
      data$rmw[indicesObsInterpolated] <- getRmw(data$msw[indicesObsInterpolated], lat)
    } else {
      ##interpolatedRMW[indicesObsInterpolated] <- st@obs.all$rmw[indices]
      data$rmw[indicesObsInterpolated] <- st@obs.all$rmw[indices]
    }
  }

  # Interpolate data
  data$lon <- zoo::na.approx(data$lon)
  data$lat <- zoo::na.approx(data$lat)
  data$msw <- zoo::na.approx(data$msw, rule = 2)
  data$rmw <- zoo::na.approx(data$rmw, rule = 2)
  # For velocities, we use na.locf instead of linear interpolation
  data$stormSpeed <- zoo::na.locf(data$stormSpeed)
  data$vxDeg <- zoo::na.locf(data$vxDeg)
  data$vyDeg <- zoo::na.locf(data$vyDeg)

  if (method == "Holland" || method == "Boose") {
    if (all(is.na(st@obs.all$poci[indices])) || all(is.na(st@obs.all$pres[indices]))) {
      stop("Missing pressure data to perform Holland model")
    }

    data$poci <- rep(NA, lenInterpolated)
    data$pc <- rep(NA, lenInterpolated)
    data$poci[indicesObsInterpolated] <- st@obs.all$poci[indices]
    data$pc[indicesObsInterpolated] <- st@obs.all$pres[indices]

    # Interpolate data
    data$poci <- zoo::na.approx(data$poci)
    data$pc <- zoo::na.approx(data$pc)
  }

  return(data[indicesFinal, ])
}



#' Compute the countries intersecting with the LOI
#'
#' @noRd
#' @param loi sf. Location of Interest
#'
#' @return sf. Countries intersecting with the LOI
getCountriesInLoi <- function(loi) {
  # Map for intersection
  world <- rworldmap::getMap(resolution = "high")
  world <- sf::st_as_sf(world, crs = wgs84)
  indCountriesInLoi <- which(sf::st_intersects(loi, world$geometry, sparse = FALSE) == TRUE)
  return(world$geometry[indCountriesInLoi])
}


getLandIntersect <- function(points, countries) {
  if (class(points) == "SpatRaster") {
    pts <- sf::st_as_sf(as.data.frame(terra::crds(points, na.rm = FALSE)), coords = c("x", "y"), crs = "wgs84")
  } else {
    pts <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = "wgs84")
  }
  landIntersect <- rep(0, nrow(pts))
  for (country in countries) {
    ind <- which(sf::st_intersects(pts, country, sparse = FALSE) == TRUE)
    landIntersect[ind] <- 1
  }

  return(landIntersect)
}

##########################################
# Helpers with the spatial computations  #
##########################################


#' Compute distance to the eye of the storm in km
#'
#' @noRd
#' @param points vector or SpatRaster. Coordinates of the points. 1st column is lon, 2nd column is lat
#' @param eye vector. Coordinates of the eye of the storm
#'
#' @return SpatRaster if isRaster, else matrix. Distance to the eye of the storm in km
computeDistanceEyeKm <- function(points, eye) {
  # Computing distances to the eye of the storm in km
  # Case for spatialBehaviour
  if (is(points, "SpatRaster")) {
    #distEyeKm <- terra::distance(
    #  x = points,
    #  y = terra::vect(rbind(eye), crs = "+proj=longlat +datum=WGS84"),
    #  unit = "km"
    #)
    distEyeKm <- terra::distance(
      x = points,
      y = eye,
      lonlat = TRUE,
      unit = "km"
    )
  } else {
    # Case for temporalBehaviour
    distEyeKm <- terra::distance(
      x = terra::vect(points, geom = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84"),
      y = terra::vect(rbind(eye), crs = "+proj=longlat +datum=WGS84"),
      unit = "km"
    )
    distEyeKm <- terra::vect(eye, atts = t(distEyeKm))
    names(distEyeKm) <- rownames(points)
  }

  return(distEyeKm)
}

#' Compute distance to the eye of the storm in degrees
#'
#' @noRd
#' @param points vector. Coordinates of the points. 1st column is lon, 2nd column is lat
#' @param eye vector. Coordinates or vector of coordinates, of the eye of the storm as "x" and "y"
#'
#' @return list. xDistEyeDeg and yDistEyeDeg
computeDistanceEyeDeg <- function(points, eye) {
  # Computing distances to the eye of the storm for x and y axes in degrees
  if (dim(eye)[1] == 1) {
    #rasterCoords <- makeCoordinatesRaster(points)
    #distEyeDeg <- rasterCoords - eye
    #xDistEyeDeg <- points[, 1] - eye[1]
    #yDistEyeDeg <- points[, 2] - eye[2]
    distEyeDeg <- rbind(
      lon = points[, 1] - eye[1],
      lat = points[, 2] - eye[2]
    )
  } else {
    xDistEyeDeg <- apply(points, c(1, 2), function(p) p - eye[, 1])[, , 1]
    yDistEyeDeg <- apply(points, c(1, 2), function(p) p - eye[, 2])[, , 2]
    distEyeDeg <- terra::vect(eye, atts = cbind(xDistEyeDeg, yDistEyeDeg))
    names <- c()
    for (rn in rownames(points)) for (c in c("lon", "lat")) names <- append(names, paste0(rn, "_", c))
    names(distEyeDeg) <- names
  }

  return(distEyeDeg)
}
