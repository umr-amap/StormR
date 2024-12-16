
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
#' @param landIntersect numeric vector. 1 if coordinates intersect with land, 0 otherwise
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

  fr <- function(r) sqrt((rmw / r)**b * exp(1 - (rmw / r)**b))
  vr <- lapply(r, fr)
  # Northern Hemisphere, t is clockwise
  # Southern Hemisphere, t is counterclockwise
  #if (lat >= 0) {
  #  angle <- atan2(vy, vx) - terra::atan2(y, x)
  #} else {
  #  angle <- terra::atan2(y, x) - atan2(vy, vx)
  #}
  angle <- ifelse(lat >= 0, atan2(vy, vx) - terra::atan2(y, x), terra::atan2(y, x) - atan2(vy, vx))

  # Land interaction
  vr[landIntersect == 1] <- 0.8 * (msw - (1 - sin(angle[landIntersect == 1])) * vh / 2) * vr[landIntersect == 1]
  vr[landIntersect == 0] <- (msw - (1 - sin(angle[landIntersect == 0])) * vh / 2) * vr[landIntersect == 0]


  return(round(vr, 3))
}



################################################
# Helpers to handle Models/Asymmetry/Direction #
################################################


#' Compute wind profile according to the selected method and asymmetry
#'
#' @noRd
#' @param name character. Name of the storm.
#' @param data data.frame. Data generated with getInterpolatedData function at a given time step
#'   containing all information for calculations.
#' @param method character. method input form stormBehaviour_sp.
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param distEyeKm SpatRaster or matrix. Distance to the eye of the storm in km
#' @param distEyeDeg SpatRaster with two layers named 'lon' and 'lat'.
#'   Distance to the eye of the storm in degrees for lon and lat dimensions.
#' @param rasterTemplate SpatRaster. Template raster containing coordinates of the points to calculate the wind profile
#' @param loi sf. loi to intersect for Boose model.
#' @param countries sf Geometry. countries to intersect for Boose model.
#' @param points sf Geometry or data.frame. Points for land / ocean intersection for Boose model.
#'
#' @return  numeric vector. Wind speed values (m/s)
computeWindProfile <- function(name, data, method, asymmetry, distEyeKm, distEyeDeg,
                               loi, countries, points) {

  landIntersect <- NULL

  # Computing wind speed according to the input model
  if (method == "Willoughby") {
    speed <- willoughby(
      r = distEyeKm, rmw = data$rmw, msw = data$msw, lat = data$lat
    )
  } else if (method == "Holland") {
    speed <- holland(
      r = distEyeKm, rmw = data$rmw, msw = data$msw, pc = data$pc, poci = data$poci, lat = data$lat
    )
  } else if (method == "Boose") {
    # Intersect points coordinates with land
    landIntersect <- getLandIntersect(points, countries)
    # No asymetry when using Boose model
    asymmetry <- "None"
    speed <- boose(
      r = distEyeKm, rmw = data$rmw, msw = data$msw, pc = data$pc, poci = data$poci,
      x = distEyeDeg$lon, y = distEyeDeg$lat, vx = data$vxDeg,vy = data$vyDeg,
      vh = data$stormSpeed, landIntersect = landIntersect, lat = data$lat
    )
  }

  # Computing wind direction
  if (is(speed, "data.frame")) {
    # temporalBehaviour case
    direction <- speed
    for (i in seq_along(rownames(df))) {
      direction[, i] <- computeDirection(method,
        distEyeDeg[, c(i * 2 - 1, i * 2)][[1]],
        distEyeDeg[, c(i * 2 - 1, i * 2)][[2]],
        data$lat,
        landIntersect
      )
    }
  } else {
    # spatialBehaviour case
    names(speed) <- paste0(name, "_Speed_", data["indices"])
    direction <- terra::rast(
      x = points,
      vals = terra::values(
        computeDirection(method,
                         distEyeDeg$lon,
                         distEyeDeg$lat,
                         data$lat,
                         landIntersect),
      ),
      names = paste0(name, "_Direction_", data["indices"])
    )
  }

  # Adding asymmetry
  if (asymmetry != "None") {
    output <- computeAsymmetry(
      asymmetry, speed, direction,
      data$vxDeg, data$vyDeg, data$stormSpeed, distEyeKm,
      data$rmw, data$lat
    )
  } else {
    output <- list(speed = speed, direction = direction)
  }

  return(output)
}


#' Compute azimuthal direction
#'
#' @noRd
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'  direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#' direction (deg)
#'
#' @return numeric vector. Azimuthal direction at each (x,y) position
azimuthalDirection <- function(x, y) {
  #azimuth <- -(terra::atan2(y, x) - pi / 2)
  #azimuth[azimuth < 0] <- azimuth[azimuth < 0] + 2 * pi
  #return(180 / pi * azimuth)
  return(180 / pi * ((-(terra::atan2(y, x) - pi / 2) + 2 * pi) %% (2 * pi)))
}

#' Rotate wind direction from -180/180 to 0/360
#'
#' @noRd
#' @param direction numeric vector. Wind direction values
#' between -180 and 180
#'
#' @return numeric vector. Wind direction values between 0 and 360
rotate0360 <- function(direction) {
  #direction[direction < 0] <- direction[direction < 0] + 360
  #direction[direction > 360] <- direction[direction > 360] - 360
  #return(direction)
  return((terra::rotate(direction, left = FALSE)))
}


#' Compute asymmetry
#'
#' @noRd
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param speed numeric vector. Wind speed values
#' @param dir numeric vector. Wind direction values
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
#' @return numeric vectors. Wind speed values (m/s) and wind direction (deg) at
#'   each (x,y) position
computeAsymmetry <- function(asymmetry, speed, dir, vx, vy, vh, r, rmw, lat) {
  # Circular symmetrical wind components
  windX <- speed * cos(dir)
  windY <- speed * sin(dir)

  # Moving wind
  stormDir <- -(atan2(vy, vx) - pi / 2)
  stormDir[stormDir < 0] <- stormDir[stormDir < 0] + 2 * pi

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

  if (is(tWindX, "data.frame")) {
    # temporalBehaviour case
    at2 <- mapply(atan2, tWindY, tWindX)
  } else {
    # spatialBehaviour case
    at2 <- terra::atan2(tWindY, tWindX)
    names(at2) <- names(dir)
  }
  # New wind direction
  direction <- (180 + at2 * 180 / pi) %% 360

  return(list(speed = round(speed, 3), direction = round(direction, 3)))
}


#' Compute wind direction. "Symetrical" in case of Willoughby or Holland model, following
#' Boose et al. (2004) model otherwise
#'
#' @noRd
#' @param method character. Method to compute the wind direction. Can be either
#'  "Willoughby" or "Holland" or "Boose"
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'   direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#'   direction (deg)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#' @param landIntersect numeric array. 1 if coordinates intersect with land, 0 otherwise
#'
#' @return wind directions (rad) at each (x,y) position
computeDirection <- function(method, x, y, lat, landIntersect) {
  az <- azimuthalDirection(x, y)

  if (length(lat) == 1) {
    # spatialBehaviour case
    direction <- if (lat >= 0) {
      az - 90
    } else {
      az + 90
    }
  } else {
    # temporalBehaviour case
    direction <- ifelse(lat >= 0, az - 90, az + 90)
  }

  if (method == "Boose") {
    direction <- mapply(landInteractionBoose, lat, direction, landIntersect)
  }

  return(round(rotate0360(direction), 3))
}

landInteractionBoose <- function(lat, direction, landIntersect) {
  if (lat >= 0) {
    direction[landIntersect == 1] <- direction[landIntersect == 1] - 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] - 20
  } else {
    direction[landIntersect == 1] <- direction[landIntersect == 1] + 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] + 20
  }
  return(direction)
}

#' rasterizePDI counterpart function for non raster data
#'
#' @noRd
#' @param speed numeric vector. Wind speed values
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#'
#' @return numeric. PDI computed using the wind speed values in wind
computePDI <- function(speed, tempRes) {
  # Computing surface drag coefficient
  rho <- 1
  cd <- 0.002
  # Raising to power 3
  pdi <- speed**3
  # Applying both rho and surface drag coefficient
  pdi <- pdi * rho * cd
  # Integrating over the whole track
  if (is(pdi, data.frame)) {
    # temporalBehaviour case
    pdi <- colSums(pdi, na.rm = TRUE) * tempRes
  } else {
    # spatialBehaviour case
    pdi <- sum(pdi, na.rm = TRUE) * tempRes
  }

  return(round(pdi, 3))
}

#' rasterizeExposure counterpart function for non raster data
#'
#' @noRd
#' @param speed numeric vector. Wind speed values
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param windThreshold numeric vector. Wind threshold
#'
#' @return numeric vector of length 5 (for each category). Exposure computed
#'   using the wind speed values in wind
computeExposure <- function(speed, tempRes, windThreshold) {
  exposure <- speed[FALSE, ]
  for (l in seq_along(windThreshold)) {
    isGreaterSpeed <- ifelse(speed >= windThreshold[l], 1, NA)
    exposure[l, ] <- colSums(isGreaterSpeed, na.rm = TRUE) * tempRes
  }

  return(exposure)
}
