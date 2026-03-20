############
# PRODUCTS #
############

#' Compute the products for a given storm at a given time step
#'
#' @noRd
#' @param speed. Wind speeds.
#' @param direction. Raster with one layer containing wind directions.
#' @param product. Product(s) to compute.
#' @param tempRes numeric. Time resolution, used for the numerical integration
#' @param windThreshold list numeric. Wind threshold used fior "Exposure" product computation.
#'
#' @return Product computed for the current time step.
computeProduct <- function(speed, direction, product, tempRes, windThreshold) {
  UseMethod("computeProduct")
}

#' Compute the products for a given storm at a given time step
#'
#' @noRd
#' @param speed SpatRaster. Raster with one layer containing wind speeds.
#' @param direction SpatRaster. Raster with one layer containing wind directions.
#' @param product list of character. Product(s) to compute.
#' @param tempRes numeric. Time resolution, used for the numerical integration
#' @param windThreshold numeric. Wind threshold used fior "Exposure" product computation.
#'
#' @return list of SpatRaster. Layers computed for the current time step.

computeProduct.SpatRaster <- function(speed, direction, product, tempRes, windThreshold) {
  stormRastersTS <- list(MSW = c(), PDI = c(), EXP = c(), Speed = c(), Direction = c())
  if ("MSW" %in% product) {
    stormRastersTS$MSW <- speed
  }
  if ("PDI" %in% product) {
    stormRastersTS$PDI <- computePDI(speed, tempRes)
  }
  if ("Exposure" %in% product) {
    stormRastersTS$EXP <- computeExposure(speed, tempRes, windThreshold)
  }
  if ("Profiles" %in% product) {
    stormRastersTS$Speed <- speed
    stormRastersTS$Direction <- direction
  }
  return(stormRastersTS)
}



#' Compute the products for a given storm for non raster data
#'
#' @noRd
#'
#' @param speed numeric vector. Wind speed values
#' @param direction numeric vector. Wind direction
#' @param product character. Product input from temporalBehaviour
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param windThreshold numeric vector. Wind threshold
#'
#' @return numeric array of dimension:
#' \itemize{
#'   \item number of observations: number of points. If product == "MSW"
#'   \item 1 : number of points. If product == "PDI"
#'   \item number of thresholds : number of points. If product == "Exposure"
#' }
computeProduct.numeric <- function(speed, direction, product, tempRes, windThreshold) {
  if (product == "TS") {
    prod <- cbind(speed, direction)
  } else if (product == "PDI") {
    prod <- computePDI(speed, tempRes)
  } else if (product == "Exposure") {
    prod <- computeExposure(speed, tempRes, windThreshold)
  }

  return(prod)
}


#' Compute the PDI for a storm
#'
#' The `computePDI()` function allows computing the power dissipation index (PDI)
#' as defined by K.A. Emanuel (1999)
#'
#' @param wind numeric vector or SpatRaster. Wind speed values
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#'
#' @return numeric vector or SpatRaster. PDI computed using the wind speed values in wind
#'
#' @details The method is used for both the `spatialBehaviour` and `temporalBehaviour` computation
#' @noRd
computePDI <- function(wind, tempRes) {
  # Surface sea-level air density in kg.m-3
  rho <- 1
  # Surface drag coefficient
  cd <- 0.002
  # Raising to power 3
  pdi <- wind**3
  # Applying both rho and surface drag coefficient
  pdi <- pdi * rho * cd
  # Integrating over the whole track and converting minutes to seconds
  pdi <- sum(pdi, na.rm = TRUE) * tempRes * 60

  return(round(pdi, 3))
}


#' Compute the time exposure for a storm
#'
#'
#' @param x SpatRaster or numeric vector of wind field
#' @param tempRes numeric. Temporal resolution, used for the temporal integration
#' @param threshold numeric vector. Wind thresholds used for exposure calculations
#' @param ... additional arguments
#'
#' @return raster or numeric vector. Exposure is computed using the wind speed values
#' @noRd
computeExposure <- function(x, tempRes, threshold, ...) {
  UseMethod("computeExposure")
}

#' @noRd
#'
#' @method computeExposure numeric
#'
#' @param x A numeric vector of wind speeds.
#' @param tempRes numeric. Temporal resolution, used for the temporal integration
#'
#' @details The method is used for the `temporalBehaviour` computation
computeExposure.numeric <- function(x, tempRes, threshold, ...) {
  exposure <- c()
  for (t in threshold) {
    exposureT <- rep(0, length(x))
    ind <- which(x >= t)
    # Converting to hours
    exposureT[ind] <- tempRes / 60.
    exposure <- c(exposure, sum(exposureT, na.rm = TRUE))
  }

  return(exposure)
}

#' @noRd
#' @method computeExposure SpatRaster
#'
#' @param x A \code{terra::SpatRaster} containing wind speed values.
#' @param tempRes numeric. Temporal resolution, used for the temporal integration
#' @param threshold character.
#'
#' @return list of SpatRaster
computeExposure.SpatRaster <- function(x, tempRes, threshold, ...) {
  exposure <- c()
  for (t in threshold) {
    exposureT <- terra::deepcopy(x)
    terra::values(exposureT) <- NA
    ind <- which(terra::values(x) >= t)
    # Converting to hours
    exposureT[ind] <- tempRes / 60.
    exposure <- c(exposure, exposureT)
  }

  return(exposure)
}



##########
# MODELS #
##########


#' Compute the Radius of Maximum Wind
#'
#' It is an empirical formula extracted from Willoughby et al. 2006 model
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#'
#' @returns Radius of Maximum Wind (km)
getRmw <- function(msw, lat) {
  return(round(46.4 * exp(-0.0155 * msw + 0.0169 * abs(lat))))
}


#' Willoughby et al. (2006) model
#'
#' Compute tangential wind speed according to Willoughby et al. (2006) model
#'
#' @noRd
#' @param r numeric. Distance to the eye of the storm (km) where the value must
#'   be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -60 and 60. Latitude of the eye of the
#'   storm
#'
#' @returns tangential wind speed value (m/s) according to Willoughby model at
#'   distance `r` to the eye of the storm located in latitude `lat`
willoughby <- function(r, rmw, msw, lat) {
  x1 <- 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat)
  x2 <- 25
  a <- 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat)
  n <- 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat)

  vr <- r
  vr[r >= rmw] <- msw * ((1 - a) * exp(-abs((r[r >= rmw] - rmw) / x1)) + a * exp(-abs(r[r >= rmw] - rmw) / x2))
  vr[r < rmw] <- msw * abs((r[r < rmw] / rmw)^n)


  return(round(vr, 3))
}


#' Holland (1980) model
#'
#' Compute tangential wind speed according to Holland (1980) model
#'
#' @noRd
#' @param r numeric. Distance to the eye of the storm (km) where the value must
#'   be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param pc numeric. Pressure at the center of the storm (hPa)
#' @param poci Pressure at the Outermost Closed Isobar (hPa)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#' @returns tangential wind speed value (m/s) according to Holland 80 model at
#'   distance `r` to the eye of the storm located in latitude `lat`
holland <- function(r, rmw, msw, pc, poci, lat) {
  rho <- 1.15 # air densiy
  f <- 2 * 7.29 * 10**(-5) * sin(lat) # Coriolis parameter
  b <- rho * exp(1) * msw**2 / (poci - pc)

  vr <- r
  vr <- sqrt(b / rho * (rmw / r)**b * (poci - pc) * exp(-(rmw / r)**b) + (r * f / 2)**2) - r * f / 2

  return(round(vr, 3))
}



#' Boose et al. (2004) model
#'
#' Compute tangential wind speed according to Boose et al. (2004) model
#'
#' @noRd
#' @param r numeric. Distance to the eye of the storm (km) where the value must
#'   be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param pc numeric. Pressure at the center of the storm (hPa)
#' @param poci Pressure at the Outermost Closed Isobar (hPa)
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'   direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#'   direction (deg)
#' @param vx numeric. Velocity of the storm in the x direction (deg/h)
#' @param vy numeric. Velociy of the storm in the y direction (deg/h)
#' @param vh numeric. Velociy of the storm (m/s)
#' @param landIntersect numeric array. 1 if coordinates intersect with land, 0 otherwise
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#'
#' @returns tangential wind speed value (m/s) according to Boose04 model at distance
#'   `r` to the eye of the storm located in latitude
boose <- function(r, rmw, msw, pc, poci, x, y, vx, vy, vh, landIntersect, lat) {
  rho <- 1 # air densiy
  b <- rho * exp(1) * msw**2 / (poci - pc)

  vr <- r
  vr <- sqrt((rmw / r)**b * exp(1 - (rmw / r)**b))


  if (lat >= 0) {
    # Northern Hemisphere, t is clockwise
    angle <- atan2(vy, vx) - atan2(y, x)
  } else {
    # Southern Hemisphere, t is counterclockwise
    angle <- atan2(y, x) - atan2(vy, vx)
  }


  vr[landIntersect == 1] <- 0.8 * (msw - (1 - sin(angle[landIntersect == 1])) * vh / 2) * vr[landIntersect == 1]
  vr[landIntersect == 0] <- (msw - (1 - sin(angle[landIntersect == 0])) * vh / 2) * vr[landIntersect == 0]


  return(round(vr, 3))
}


#' Compute wind profile according to the selected method and asymmetry
#'
#' @noRd
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use for
#'   the computations
#' @param method character. method input form stormBehaviour_sp
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'   direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#'   direction (deg)
#' @param crds numeric array (1 column lon, 1 column lat). coordinates of raster
#' @param distEye numeric array. Distance in meter from the eye of the storm for
#'   each coordinate of the rasterTemplate_model
#' @param buffer numeric. Buffer size (in degree) for the storm
#' @param loi sf. loi to intersect for Boose model
#' @param world sf. world for Boose model
#' @param indCountries numeric vector. Indices of countries to intersect for
#'   Boose model
#'
#' @return  numeric vector. Wind speed values (m/s)
computeWindProfile <- function(data, index, method, asymmetry, x, y, crds, distEye, buffer, loi, world, indCountries) {
  # Computing wind speed according to the input model
  if (method == "Willoughby") {
    speed <- willoughby(
      msw = data$msw[index],
      lat = data$lat[index],
      r = distEye * 0.001,
      rmw = data$rmw[index]
    )
  } else if (method == "Holland") {
    data$poci[data$pc == data$poci] <- data$poci[data$pc == data$poci] + 1 # avoid case pc = poci
    speed <- holland(
      r = distEye * 0.001,
      rmw = data$rmw[index],
      msw = data$msw[index],
      pc = data$pc[index],
      poci = data$poci[index],
      lat = data$lat[index]
    )
  } else if (method == "Boose") {
    # Intersect points coordinates with land
    pts <- sf::st_as_sf(as.data.frame(crds), coords = c("x", "y"))
    sf::st_crs(pts) <- wgs84

    landIntersect <- rep(0, length(x))

    for (i in indCountries) {
      ind <- which(sf::st_intersects(pts, world$geometry[i], sparse = FALSE) == TRUE)
      landIntersect[ind] <- 1
    }

    data$poci[data$pc == data$poci] <- data$poci[data$pc == data$poci] + 1 # avoid case pc = poci
    speed <- boose(
      r = distEye * 0.001,
      rmw = data$rmw[index],
      msw = data$msw[index],
      pc = data$pc[index],
      poci = data$poci[index],
      x = x,
      y = y,
      vx = data$vxDeg[index],
      vy = data$vyDeg[index],
      vh = data$stormSpeed[index],
      landIntersect = landIntersect,
      lat = data$lat[index]
    )

    direction <- computeDirection(x, y, data$lat[index], landIntersect)
  }


  # Compute wind direction
  if (method != "Boose") {
    direction <- computeDirection(x, y, data$lat[index])
  }


  # Adding asymmetry
  if (asymmetry != "None") {
    output <- computeAsymmetry(
      asymmetry, speed, x, y,
      data$vxDeg[index], data$vyDeg[index],
      data$stormSpeed[index],
      distEye * 0.001, data$rmw[index], data$lat[index]
    )
  } else {
    output <- list(speed = round(speed, 3), direction = round(direction, 3))
  }

  # Remove cells outside of buffer
  dist <- sqrt(x * x + y * y)
  output$speed[dist > buffer] <- NA
  output$direction[dist > buffer] <- NA

  return(output)
}





#' Compute asymmetry
#'
#' @noRd
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param speed numeric vector. Wind values
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'   direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#'   direction (deg)
#' @param vx numeric. Velocity of the storm in the x direction (deg/h)
#' @param vy numeric. Velocity of the storm in the y direction (deg/h)
#' @param vh numeric. Velocity of the storm (m/s)
#' @param r numeric. Distance to the eye of the storm (km) where the value must
#'   be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#'
#' @return numeric vectors. Wind speed values (m/s) and wind direction (rad) at
#'   each (x,y) position
computeAsymmetry <- function(asymmetry, speed, x, y, vx, vy, vh, r, rmw, lat) {
  # Circular symmetrical wind
  dir <- -(atan2(y, x) - pi / 2)

  if (lat >= 0) {
    dir <- dir - pi / 2
  } else {
    dir <- dir + pi / 2
  }

  dir[dir < 0] <- dir[dir < 0] + 2 * pi
  dir[dir > 2 * pi] <- dir[dir > 360] - 2 * pi

  windX <- speed * cos(dir)
  windY <- speed * sin(dir)

  # Moving wind
  stormDir <- -(atan2(vy, vx) - pi / 2)
  if (stormDir < 0) {
    stormDir <- stormDir + 2 * pi
  }

  mWindX <- vh * cos(stormDir)
  mWindY <- vh * sin(stormDir)

  # Formula for asymmetry
  if (asymmetry == "Chen") {
    formula <- 3 * rmw**(3 / 2) * r**(3 / 2) / (rmw**3 + r**3 + rmw**(3 / 2) * r**(3 / 2))
  } else if (asymmetry == "Miyazaki") {
    formula <- exp(-r / 500 * pi)
  }

  # New total wind speed
  tWindX <- windX + formula * mWindX
  tWindY <- windY + formula * mWindY


  speed <- sqrt(tWindX**2 + tWindY**2)

  # New wind direction
  direction <- atan2(tWindY, tWindX) * 180 / pi
  direction <- (direction + 180) %% 360

  return(list(speed = round(speed, 3), direction = round(direction, 3)))
}





#' Compute wind direction
#' @noRd
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'   direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#'   direction (deg)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#' @param landIntersect numeric array. 1 if coordinates intersect with land, 0 otherwise
#'
#' @return wind directions (rad) at each (x,y) position
computeDirection <- function(x, y, lat, landIntersect = NULL) {
  if (length(lat) != 1 || is.na(lat)) {
    stop("lat must be a single non-NA value")
  }

  if (length(x) != length(y)) {
    stop("x, y, and landIntersect must have the same length")
  }

  if (!is.null(landIntersect) && length(x) != length(landIntersect)) {
    stop("x, y, and landIntersect must have the same length")
  }

  # Hemisphere sign
  hemi <- if (lat >= 0) -1 else 1

  # Base azimuth
  azimuth <- -(atan2(y, x) - pi / 2)
  azimuth <- azimuth %% (2 * pi)

  # Convert to degrees
  direction <- azimuth * 180 / pi

  # Hemisphere adjustment
  direction <- direction + hemi * 90

  # Land interaction adjustment
  if (!is.null(landIntersect)) {
    land_shift <- 20 + 20 * (landIntersect == 1)
    direction <- direction + hemi * land_shift
  }

  # Normalize and convert to "coming-from" wind direction
  direction <- (direction + 180) %% 360

  return(direction)
}

#' Get Storm displacement speed
#'
#' Given vectors of longitude and latitude of the center of the storm, computes
#' the displacement velocity of the storm
#'
#' @noRd
#' @param longitude numeric vector. Vector of longitudes of the center of the storm.
#' @param latitutde numeric vector. Vector of latitudes of the center of the storm.
#' @param tempRes numeric. Temporal resolution of the data, in min.
#'
#' @return numeric vectors for storm displacement velocity (m.s-1), longitude and latitude
#' components (degree.h-1)
stormDisplacement <- function(longitude, latitude, tempRes) {
  lenData <- length(longitude)
  stormSpeed <- rep(NA, lenData)
  vxDeg <- rep(NA, lenData)
  vyDeg <- rep(NA, lenData)
  tempResH <- tempRes / 60.

  # Computing storm velocity (m/s)
  for (i in 1:(lenData - 1)) {
    stormSpeed[i] <- terra::distance(
      x = cbind(longitude[i], latitude[i]),
      y = cbind(longitude[i + 1], latitude[i + 1]),
      lonlat = TRUE
    ) * (0.001 / tempResH) / 3.6

    # component wise velocity in both x and y direction (degree/h)
    vxDeg[i] <- (longitude[i + 1] - longitude[i]) / tempResH
    vyDeg[i] <- (latitude[i + 1] - latitude[i]) / tempResH
  }
  stormSpeed[lenData] <- stormSpeed[lenData - 1]
  vxDeg[lenData] <- vxDeg[lenData - 1]
  vyDeg[lenData] <- vyDeg[lenData - 1]

  return(list(stormSpeed = stormSpeed, vxDeg = vxDeg, vyDeg = vyDeg))
}
