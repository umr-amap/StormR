
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



##############################################
# Helpers to handle Models/Asymmetry/Direction#
##############################################

#' Compute distance to the eye of the storm in km
#'
#' @noRd
#' @param points vector or SpatRaster. Coordinates of the points. 1st column is lon, 2nd column is lat
#' @param eye vector. Coordinates of the eye of the storm
#' @param isRaster logical. If TRUE, points is a SpatRaster
#'
#' @return SpatRaster if isRaster, else matrix. Distance to the eye of the storm in km
computeDistanceEyeKm <- function(points, eye, isRaster = FALSE) {
  # Computing distances to the eye of the storm in km
  if (isRaster) {
    distEyeKm <- terra::distance(
      x = points,
      y = terra::vect(rbind(eye), crs = "+proj=longlat +datum=WGS84"),
      unit = "km"
    )
  } else {
    distEyeKm <- terra::distance(
      x = points,
      y = eye,
      unit = "km",
      lonlat = TRUE
    )
  }

  return(distEyeKm)
}

# ' Compute distance to the eye of the storm in degrees
# '
# ' @noRd
# ' @param points vector. Coordinates of the points. 1st column is lon, 2nd column is lat
# ' @param eye vector. Coordinates of the eye of the storm
# '
# ' @return list. xDistEyeDeg and yDistEyeDeg
computeDistanceEyeDeg <- function(points, eye) {
  # Computing distances to the eye of the storm for x and y axes in degrees
  xDistEyeDeg <- points[, 1] - eye[1]
  yDistEyeDeg <- points[, 2] - eye[2]

  return(list(xDist = xDistEyeDeg, yDist = yDistEyeDeg))
}


#' Compute wind profile according to the selected method and asymmetry
#'
#' @noRd
#' @param data data.frame. Data generated with getInterpolatedData function at a given time step
#'   containing all information for calculations
#' @param method character. method input form stormBehaviour_sp
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param distEyeKm SpatRaster or matrix. Distance to the eye of the storm in km
#' @param distEyeDeg list. xDist and yDist. Distance to the eye of the storm in degrees
#' @param points SpatRaster or matrix. Coordinates of the points to calculate the wind profile
#' @param resolution numeric. Resolution of the raster
#' @param loi sf. loi to intersect for Boose model
#' @param countries sf Geometry. countries to intersect for Boose model
#'
#' @return  numeric vector. Wind speed values (m/s)
computeWindProfile <- function(data, method, asymmetry, distEyeKm, distEyeDeg,
                               points, resolution, loi, countries) {
  # Computing wind speed according to the input model
  if (method == "Willoughby") {
    speed <- willoughby(
      r = distEyeKm, rmw = data$rmw, msw = data$msw, lat = data$lat
    )
    direction <- terra::rast(
      x = points,
      vals = computeDirection(distEyeDeg$xDist, distEyeDeg$yDist, data$lat)
    )
  } else if (method == "Holland") {
    speed <- holland(
      r = distEyeKm, rmw = data$rmw, msw = data$msw,
      pc = data$pc, poci = data$poci, lat = data$lat
    )
    direction <- terra::rast(
      x = points,
      vals = computeDirection(distEyeDeg$xDist, distEyeDeg$yDist, data$lat)
    )
  } else if (method == "Boose") {
    # Intersect points coordinates with land
    pts <- sf::st_as_sf(as.data.frame(terra::crds(points, na.rm = FALSE)), coords = c("x", "y"), crs = "wgs84")
    landIntersect <- rep(0, nrow(pts))

    for (country in countries) {
      ind <- which(sf::st_intersects(pts, country, sparse = FALSE) == TRUE)
      landIntersect[ind] <- 1
    }
    asymmetry <- "None"
    speed <- boose(
      r = distEyeKm, rmw = data$rmw, msw = data$msw,
      pc = data$pc, poci = data$poci, x = distEyeDeg$xDist, y = distEyeDeg$yDist,
      vx = data$vxDeg, vy = data$vyDeg, vh = data$stormSpeed,
      landIntersect = landIntersect, lat = data$lat
    )
    direction <- terra::rast(
      x = points,
      vals = computeDirectionBoose(distEyeDeg$xDist, distEyeDeg$yDist, data$lat, landIntersect)
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

  ## Remove cells outside of buffer
  #dist <- sqrt(xDistEyeDeg * xDistEyeDeg + yDistEyeDeg * yDistEyeDeg)
  #output$speed[dist > buffer] <- NA
  #output$direction[dist > buffer] <- NA

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
  azimuth <- -(atan2(y, x) - pi / 2)
  azimuth[azimuth < 0] <- azimuth[azimuth < 0] + 2 * pi
  return(180 / pi * azimuth)
}


#' Rotate wind direction from -180/180 to 0/360
#'
#' @noRd
#' @param direction numeric vector. Wind direction values
#' between -180 and 180
#'
#' @return numeric vector. Wind direction values between 0 and 360
rotate0360 <- function(direction) {
  direction[direction < 0] <- direction[direction < 0] + 360
  direction[direction > 360] <- direction[direction > 360] - 360
  return(direction)
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
  direction <- (180 + terra::atan2(tWindY, tWindX) * 180 / pi) %% 360

  return(list(speed = round(speed, 3), direction = round(direction, 3)))
}


#' Compute wind direction according to Boose et al. (2004) model
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
computeDirectionBoose <- function(x, y, lat, landIntersect) {
  az <- azimuthalDirection(x, y)

  if (lat >= 0) {
    direction <- az - 90
    direction[landIntersect == 1] <- direction[landIntersect == 1] - 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] - 20
  } else {
    direction <- az + 90
    direction[landIntersect == 1] <- direction[landIntersect == 1] + 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] + 20
  }

  return(round(rotate0360(direction), 3))
}


#' Compute symetrical wind direction
#' @noRd
#' @param x numeric vector. Distance(s) to the eye of the storm in the x
#'   direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y
#'   direction (deg)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the
#'   storm
#'
#' @return wind directions (rad) at each (x,y) position
computeDirection <- function(x, y, lat) {
  az <- azimuthalDirection(x, y)

  if (lat >= 0) {
    direction <- az - 90
  } else {
    direction <- az + 90
  }

  return(round(rotate0360(direction), 3))
}



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
  pdi <- sum(pdi, na.rm = TRUE) * tempRes

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


#' rasterizeExposure counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param threshold numeric vector. Wind threshold
#'
#' @return numeric vector of length 5 (for each category). Exposure computed
#'   using the wind speed values in wind
computeExposure <- function(wind, tempRes, threshold) {
  exposure <- c()
  for (t in threshold) {
    ind <- which(wind >= t)
    expo <- rep(0, length(wind))
    expo[ind] <- 1
    exposure <- c(exposure, sum(expo, na.rm = TRUE) * tempRes)
  }

  return(exposure)
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