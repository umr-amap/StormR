




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




########################
# Helper to check inputs#
########################





#' Check inputs for spatialBehaviour function
#'
#' @noRd
#' @param sts StormsList object
#' @param product character
#' @param windThreshold numeric
#' @param method character
#' @param asymmetry character
#' @param empiricalRMW logical
#' @param spaceRes character
#' @param tempRes numeric
#' @param verbose numeric
#' @return NULL
checkInputsSpatialBehaviour <- function(sts, product, windThreshold, method, asymmetry,
                                        empiricalRMW, spaceRes, tempRes, verbose) {
  # Checking sts input
  stopifnot("no data found" = !missing(sts))

  # Checking product input
  stopifnot("Invalid product" = product %in% c("MSW", "PDI", "Exposure", "Profiles"))

  # Checking windThreshold input
  if ("Exposure" %in% product) {
    stopifnot("windThreshold must be numeric" = identical(class(windThreshold), "numeric"))
    stopifnot("invalid value(s) for windThreshold input (must be > 0)" = windThreshold > 0)
  }

  # Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland", "Boose"))
  stopifnot("Only one method must be chosen" = length(method) == 1)
  if (method == "Holland") {
    stopifnot("Cannot perform Holland method (missing pressure input)" = "pres" %in% colnames(sts@data[[1]]@obs.all))
    stopifnot("Cannot perform Holland method (missing poci input)" = "poci" %in% colnames(sts@data[[1]]@obs.all))
  }

  # Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "Chen", "Miyazaki"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  # Checking empiricalRMW input
  stopifnot("empiricalRMW must be logical" = identical(class(empiricalRMW), "logical"))

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





#################################
# Helpers to make template rasters#
#################################





#' Generate raster template for the computations
#'
#' @noRd
#' @param buffer sf object. LOI + buffer extention
#' @param res numeric. Space resolution min for the template
#'
#' @return a SpatRaster
makeTemplateRaster <- function(buffer, res) {
  # Deriving the raster template
  ext <- terra::ext(
    sf::st_bbox(buffer)$xmin,
    sf::st_bbox(buffer)$xmax,
    sf::st_bbox(buffer)$ymin,
    sf::st_bbox(buffer)$ymax
  )



  template <- terra::rast(
    xmin = ext$xmin,
    xmax = ext$xmax,
    ymin = ext$ymin,
    ymax = ext$ymax,
    resolution = res,
    vals = NA,
  )
  terra::origin(template) <- c(0, 0)

  return(template)
}





#' Generate raster template to compute wind speed according to the different
#' models
#'
#' @noRd
#' @param rasterTemplate SpatRaster. Raster generated with makeTemplateRaster
#'   function
#' @param buffer numeric. Buffer size in degree
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use to
#'   generate raster
#'
#' @return SpatRaster
makeTemplateModel <- function(rasterTemplate, buffer, data, index) {
  template <- terra::rast(
    xmin = data$lon[index] - buffer,
    xmax = data$lon[index] + buffer,
    ymin = data$lat[index] - buffer,
    ymax = data$lat[index] + buffer,
    resolution = terra::res(rasterTemplate),
    vals = NA,
    time = as.POSIXct(data$isoTimes[index])
  )
  terra::origin(template) <- c(0, 0)

  return(template)
}





###############################
# Helpers to get the right data#
###############################





#' Get indices for computations
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





##############################################
# Helpers to handle Models/Asymmetry/Direction#
##############################################


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
    wind <- willoughby(
      msw = data$msw[index],
      lat = data$lat[index],
      r = distEye * 0.001,
      rmw = data$rmw[index]
    )
  } else if (method == "Holland") {
    wind <- holland(
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

    wind <- boose(
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

    direction <- computeDirectionBoose(x, y, data$lat[index], landIntersect)
  }



  # Compute wind direction
  if (method != "Boose") {
    direction <- computeDirection(x, y, data$lat[index])
  }



  # Adding asymmetry
  if (asymmetry != "None") {
    output <- computeAsymmetry(
      asymmetry, wind, x, y,
      data$vxDeg[index], data$vyDeg[index],
      data$stormSpeed[index],
      distEye * 0.001, data$rmw[index], data$lat[index]
    )
  } else {
    output <- list(wind = round(wind, 3), direction = round(direction, 3))
  }

  # Remove cells outside of buffer
  dist <- sqrt(x * x + y * y)
  output$wind[dist > buffer] <- NA
  output$direction[dist > buffer] <- NA

  return(output)
}





#' Compute asymmetry
#'
#' @noRd
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param wind numeric vector. Wind values
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
computeAsymmetry <- function(asymmetry, wind, x, y, vx, vy, vh, r, rmw, lat) {
  # Circular symmetrical wind
  dir <- -(atan2(y, x) - pi / 2)

  if (lat >= 0) {
    dir <- dir - pi / 2
  } else {
    dir <- dir + pi / 2
  }

  dir[dir < 0] <- dir[dir < 0] + 2 * pi
  dir[dir > 2 * pi] <- dir[dir > 360] - 2 * pi

  windX <- wind * cos(dir)
  windY <- wind * sin(dir)

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


  wind <- sqrt(tWindX**2 + tWindY**2)

  # New wind direction
  direction <- atan2(tWindY, tWindX) * 180 / pi
  direction[direction < 0] <- direction[direction < 0] + 360

  return(list(wind = round(wind, 3), direction = round(direction, 3)))
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
  azimuth <- -(atan2(y, x) - pi / 2)

  azimuth[azimuth < 0] <- azimuth[azimuth < 0] + 2 * pi

  if (lat >= 0) {
    direction <- azimuth * 180 / pi - 90
    direction[landIntersect == 1] <- direction[landIntersect == 1] - 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] - 20
  } else {
    direction <- azimuth * 180 / pi + 90
    direction[landIntersect == 1] <- direction[landIntersect == 1] + 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] + 20
  }

  direction[direction < 0] <- direction[direction < 0] + 360
  direction[direction > 360] <- direction[direction > 360] - 360

  return(direction)
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
  azimuth <- -(atan2(y, x) - pi / 2)

  azimuth[azimuth < 0] <- azimuth[azimuth < 0] + 2 * pi

  if (lat >= 0) {
    direction <- azimuth * 180 / pi - 90
  } else {
    direction <- azimuth * 180 / pi + 90
  }

  direction[direction < 0] <- direction[direction < 0] + 360
  direction[direction > 360] <- direction[direction > 360] - 360

  return(direction)
}





###########################
# Helpers to stack products#
###########################





#' Stack a computed layer in a raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param rasterTemplate SpatRaster. Raster template generated with
#'   makeTemplateRaster function
#' @param rasterWind SpatRaster. Layer to add to the stack
#'
#' @return list of SpatRaster
stackRaster <- function(stack, rasterTemplate, rasterWind) {
  ras <- rasterTemplate
  extent <- terra::ext(ras)
  ras <- terra::merge(rasterWind, ras)
  ras <- terra::crop(ras, extent)

  return(c(stack, ras))
}





#' Stack a computed PDI layer in a PDI raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param rasterTemplate SpatRaster. Raster template generated with
#'   makeTemplateRaster function
#' @param rasterWind SpatRaster. Layer to add to the stack
#'
#' @return list of SpatRaster
stackRasterPDI <- function(stack, rasterTemplate, rasterWind) {
  rho <- 1
  cd <- 0.002
  # Raising to power 3
  rasterWind <- rasterWind**3
  # Applying both rho and surface drag coefficient
  rasterWind <- rasterWind * rho * cd

  return(stackRaster(stack, rasterTemplate, rasterWind))
}





#' Stack a computed Exposure layer in a Exposure raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param rasterTemplate SpatRaster. Raster template generated with
#'   makeTemplateRaster function
#' @param rasterWind SpatRaster. Layer to add to the stack
#' @param threshold numeric. Wind threshold
#'
#' @return list of SpatRaster
stackRasterExposure <- function(stack, rasterTemplate, rasterWind, threshold) {
  for (t in threshold) {
    rasterCModel <- rasterWind
    terra::values(rasterCModel) <- NA
    ind <- which(terra::values(rasterWind) >= t)
    rasterCModel[ind] <- 1
    stack <- stackRaster(stack, rasterTemplate, rasterCModel)
  }

  return(stack)
}





#' Select the stack function to use depending on the product
#'
#' @noRd
#' @param product character. Product input from spatialBehaviour
#' @param stack list of SpatRaster. where to stack the layer
#' @param rasterTemplate SpatRaster. Raster template generated with
#'   makeTemplateRaster function
#' @param rasterWind SpatRaster. Layer to add to the stack
#' @param threshold numeric vector. Wind threshold
#'
#' @return list of SpatRaster
stackProduct <- function(product, stack, rasterTemplate, rasterWind, threshold) {
  if (product == "MSW") {
    stack <- stackRaster(stack, rasterTemplate, rasterWind)
  } else if (product == "PDI") {
    stack <- stackRasterPDI(stack, rasterTemplate, rasterWind)
  } else if (product == "Exposure") {
    stack <- stackRasterExposure(stack, rasterTemplate, rasterWind, threshold)
  }

  return(stack)
}





#' Compute MSW raster
#'
#' @noRd
#' @param finalStack list of SpatRaster. Where to add the computed MSW raster
#' @param stack SpatRaster stack. All the wind speed rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#' @param spaceRes character. spaceRes input from spatialBehaviour
#'
#' @return list of SpatRaster
rasterizeMSW <- function(finalStack, stack, spaceRes, name) {
  nbg <- switch(spaceRes,
    "30sec" = 59,
    "2.5min" = 11,
    "5min" = 5,
    "10min" = 3
  )

  msw <- max(stack, na.rm = TRUE)
  # Applying focal function to smooth results
  msw <- terra::focal(msw, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = TRUE)
  names(msw) <- paste0(name, "_MSW")

  return(c(finalStack, msw))
}





#' Compute PDI raster
#'
#' @noRd
#' @param finalStack list of SpatRaster. Where to add the computed MSW raster
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param stack SpatRaster stack. All the PDI rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#' @param spaceRes character. spaceRes input from spatialBehaviour
#' @param threshold numeric vector. Wind threshold
#'
#'
#' @return list of SpatRaster
rasterizePDI <- function(finalStack, stack, tempRes, spaceRes, name, threshold) {
  nbg <- switch(spaceRes,
    "30sec" = 59,
    "2.5min" = 11,
    "5min" = 5,
    "10min" = 3
  )

  # Integrating over the whole track
  prod <- sum(stack, na.rm = TRUE) * tempRes
  # Applying focal function to smooth results
  prod <- terra::focal(prod, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = TRUE)
  names(prod) <- paste0(name, "_PDI")


  return(c(finalStack, prod))
}





#' Compute PDI raster
#'
#' @noRd
#' @param finalStack list of SpatRaster. Where to add the computed MSW raster
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param stack SpatRaster stack. All the PDI rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#' @param spaceRes character. spaceRes input from spatialBehaviour
#' @param threshold numeric vector. Wind threshold
#'
#'
#' @return list of SpatRaster
rasterizeExp <- function(finalStack, stack, tempRes, spaceRes, name, threshold) {
  nbg <- switch(spaceRes,
    "30sec" = 59,
    "2.5min" = 11,
    "5min" = 5,
    "10min" = 3
  )

  for (l in seq_along(threshold)) {
    ind <- seq(l, terra::nlyr(stack), length(threshold))
    # Integrating over the whole track
    prod <- sum(terra::subset(stack, ind), na.rm = TRUE) * tempRes
    # Applying focal function to smooth results
    prod <- terra::focal(prod, w = matrix(1, nbg, nbg), mean, na.rm = TRUE)
    names(prod) <- paste0(name, "_Exposure_", threshold[l])
    finalStack <- c(finalStack, prod)
  }


  return(finalStack)
}





#' Select the rasterizeProduct function to use depending on the product
#'
#' @noRd
#' @param product character. Product input from spatialBehaviour
#' @param finalStack list of SpatRaster. Where to add the computed MSW raster
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param stack SpatRaster stack. All the Exposure rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#'   in finalStack
#' @param spaceRes character. spaceRes input from spatialBehaviour
#' @param threshold numeric vector. Wind threshold
#'
#' @return list of SpatRaster
rasterizeProduct <- function(product, finalStack, stack, tempRes, spaceRes, name, threshold) {
  if (product == "MSW") {
    # Computing MSW analytic raster
    finalStack <- rasterizeMSW(finalStack, stack, spaceRes, name)
  } else if (product == "PDI") {
    # Computing PDI analytic raster
    finalStack <- rasterizePDI(finalStack, stack, tempRes, spaceRes, name, NULL)
  } else if (product == "Exposure") {
    # Computing Exposure analytic raster
    finalStack <- rasterizeExp(finalStack, stack, tempRes, spaceRes, name, threshold)
  }

  return(finalStack)
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




############################
# spatialBehaviour function#
############################





#' Computing wind behaviour and summary statistics over given areas
#'
#' The spatialBehaviour() function allows computing wind speed and
#'  direction for each cell of a regular grid (i.e., a raster)
#' for a given tropical cyclone or set of tropical cyclones.
#' It also allows to compute three associated summary statistics.
#'
#' @param sts `StormsList` object
#' @param product character vector. Desired output statistics:
#'   \itemize{
#'     \item `"Profiles"`, for 2D wind speed and direction fields,
#'     \item `"MSW"`, for maximum sustained wind speed (default setting),
#'     \item `"PDI"`, for power dissipation index, or
#'     \item `"Exposure"`, for duration of exposure.
#'   }
#' @param windThreshold numeric vector. Minimal wind threshold(s) (in \eqn{m.s^{-1}}) used to
#'   compute the duration of exposure when `product="Exposure"`. Default value is to set NULL, in this
#'   case, the windthresholds are the one used in the scale defined in the stromsList.
#' @param method character. Model used to compute wind speed and direction.
#' Three different models are implemented:
#'   \itemize{
#'   \item `"Willoughby"`, for the symmetrical model developed by Willoughby et al. (2006) (default setting),
#'   \item `"Holland"`, for the symmetrical model developed by Holland (1980), or
#'   \item `"Boose"`, for the asymmetrical model developed by  Boose et al. (2004).
#'   }
#' @param asymmetry character. If `method="Holland"` or `method="Willoughby"`,
#' this argument specifies the method used to add asymmetry. Can be:
#'   \itemize{
#'      \item `"Chen"`, for the model developed by Chen (1994) (default setting),
#'      \item `"Miyazaki"`, for the model developed by Miyazaki et al. (1962), or
#'      \item `"None"`, for no asymmetry.
#'   }
#' @param empiricalRMW logical. Whether (TRUE) or not (FALSE) to compute
#' the radius of maximum wind (`rmw`) empirically using the model developed by
#' Willoughby et al. (2006). If `empiricalRMW==FALSE` (default setting) then the
#' `rmw` provided in the `StormsList` is used.
#' @param spaceRes character. Spatial resolution. Can be `"30 sec"` (~1 km at the equator),
#' `"2.5 min"` (~4.5 km at the equator), `"5 min"` (~9 km at the equator) or `"10 min"` (~18.6 km at the equator).
#'  Default setting is `"2.5 min"`.
#' @param tempRes numeric. Temporal resolution (min). Can be `60` ( default setting),
#'   `30` or `15`.
#' @param verbose numeric. Whether or not the function should display
#'   informations about the process and/or outputs. Can be:
#' \itemize{
#'    \item `2`, information about the processes and outputs are displayed (default setting),
#'    \item `1`, information about the processes are displayed, pr
#'    \item `0`, no information displayed.
#' }
#' @returns The spatialBehaviour() function returns SpatRaster objects (in WGS84).
#' The number of layers in the output depends on the number of storms in the inputs,
#' on the desired `product`, as well as the `tempRes` argument:
#' \itemize{
#'    \item if `product = "MSW"`, the function returns one layer for each `Storm`.
#'    The names of the layer follow the following terminology, the name of the storm
#'    in capital letters and “MSW” separated by underscores (e.g., "PAM_MSW"),
#'    \item if `product = "PDI"`, the function returns one layer for each `Storm`.
#'    The names of the layer follow the following terminology, the name of the storm
#'    in capital letters and “PDI” separated by underscores (e.g., "PAM_PDI"),
#'    \item if `product ="Exposure"`, the function returns one layer for each wind speed values
#'    in the `windThreshold` argument and for each `Storm`. The names of the layer follow the
#'    following terminology, the name of the storm in capital letters, "Exposure", and the threshold
#'     value separated by underscores (e.g., "PAM_Exposure_18", "PAM_Exposure_33", ...),
#'    \item if `product = "Profiles"` the function returns one layer for wind speed and
#'    one layer for wind direction for each observation or interpolated observation and each `Storm`.
#'    The names of the layer follow the following terminology, the name of the storm in capital letters,
#'    "Speed" or "Direction", and the indices of the observation separated by underscores
#'    (e.g., "PAM_Speed_41", "PAM_Direction_41",...).
#' }
#' @details Storm track data sets, such as those extracted from IBRTrACKS (Knapp et
#'   al., 2010), usually provide observation at a 3- or 6-hours temporal
#'   resolution. In the spatialBehaviour() function, linear interpolations are
#'   used to reach the temporal resolution specified in the `tempRes` argument
#'   (default value = 60 min). When `product = "MSW"`, `product = "PDI"`,
#'   or `product = "Exposure"` the `focal()` function from the `terra` R package
#'   is used to smooth the results using moving windows.
#'
#'   The Holland (1980) model, widely used in the literature, is based on the
#'   'gradient wind balance in mature tropical cyclones. The wind speed distribution
#'   is computed from the circular air pressure field, which can be derived from
#'   the central and environmental pressure and the radius of maximum winds.
#'
#'   \eqn{v_r = \sqrt{\frac{b}{\rho} \times \left(\frac{R_m}{r}\right)^b \times (p_{oci} - p_c)
#'   \times e^{-\left(\frac{R_m}{r}\right)^b} + \left(\frac{r \times f}{2}\right)^2} -
#'   \left(\frac{r \times f}{2}\right)}
#'
#'   with,
#'
#'   \eqn{b = \frac{\rho \times e \times v_m^2}{p_{oci} - p_c}}
#'
#'   \eqn{f = 2 \times 7.29 \times 10^{-5} \sin(\phi)}
#'
#'   where, \eqn{v_r} is the tangential wind speed (in \eqn{m.s^{-1}}),
#'   \eqn{b} is the shape parameter,
#'   \eqn{\rho} is the air density set to \eqn{1.15 kg.m^{-3}},
#'   \eqn{e} being the base of natural logarithms (~2.718282),
#'   \eqn{v_m} the maximum sustained wind speed (in \eqn{m.s^{-1}}),
#'   \eqn{p_{oci}} is the pressure at outermost closed isobar of the storm (in \eqn{Pa}),
#'   \eqn{p_c} is the pressure at the centre of the storm (in \eqn{Pa}),
#'   \eqn{r} is the distance to the eye of the storm (in \eqn{km}),
#'   \eqn{R_m} is the radius of maximum sustained wind speed (in \eqn{km}),
#'   \eqn{f} is the Coriolis force (in \eqn{N.kg^{-1}}, and
#'   \eqn{\phi} being the latitude).
#'
#'   The Willoughby et al. (2006) model is an empirical model fitted to aircraft
#'   observations. The model considers two regions: inside the eye and at external
#'   radii, for which the wind formulations use different exponents to better match
#'   observations. In this model, the wind speed increases as a power function of the
#'   radius inside the eye and decays exponentially outside the eye after a smooth
#'   polynomial transition across the eyewall.
#'
#'   \eqn{\left\{\begin{aligned}
#'    v_r &= v_m \times \left(\frac{r}{R_m}\right)^{n} \quad if \quad r < R_m \\
#'    v_r &= v_m \times \left((1-A) \times e^{-\frac{|r-R_m|}{X1}} +
#'    A \times e^{-\frac{|r-R_m|}{X2}}\right) \quad if \quad r \geq R_m \\
#'    \end{aligned}
#'    \right.
#'    }
#'
#'    with,
#'
#'    \eqn{n = 2.1340 + 0.0077 \times v_m - 0.4522 \times \ln(R_m) - 0.0038 \times |\phi|}
#'
#'    \eqn{X1 = 287.6 - 1.942 \times v_m + 7.799 \times \ln(R_m) + 1.819 \times |\phi|}
#'
#'    \eqn{A = 0.5913 + 0.0029 \times v_m - 0.1361 \times \ln(R_m) - 0.0042 \times |\phi|} and \eqn{A\ge0}
#'
#'   where, \eqn{v_r} is the tangential wind speed (in \eqn{m.s^{-1}}),
#'   \eqn{v_m} is the maximum sustained wind speed (in \eqn{m.s^{-1}}),
#'   \eqn{r} is the distance to the eye of the storm (in \eqn{km}),
#'   \eqn{R_m} is the radius of maximum sustained wind speed (in \eqn{km}),
#'   \eqn{\phi} is the latitude of the centre of the storm,
#'   \eqn{X2 = 25}.
#'
#'   Asymmetry can be added to Holland (1980) and Willoughby et al. (2006) wind fields as follows,
#'
#'   \eqn{\vec{V} = \vec{V_c} + C \times \vec{V_t}}
#'
#'   where, \eqn{\vec{V}} is the combined asymmetric wind field,
#'   \eqn{\vec{V_c}} is symmetric wind field,
#'   \eqn{\vec{V_t}} is the translation speed of the storm, and
#'   \eqn{C} is function of \eqn{r}, the distance to the eye of the storm (in \eqn{km}).
#'
#'   Two formulations of C proposed by Miyazaki et al. (1962) and Chen (1994) are implemented.
#'
#'   Miyazaki et al. (1962)
#'   \eqn{C = e^{(-\frac{r}{500} \times \pi)}}
#'
#'   Chen (1994)
#'   \eqn{C = \frac{3 \times R_m^{\frac{3}{2}} \times r^{\frac{3}{2}}}{R_m^3 +
#'   r^3 +R_m^{\frac{3}{2}} \times r^{\frac{3}{2}}}}
#'
#'   where, \eqn{R_m} is the radius of maximum sustained wind speed (in \eqn{km})
#'
#'   The Boose et al. (2004) model, or “HURRECON” model, is a modification of the
#'   Holland (1980) model. In addition to adding
#'   asymmetry, this model treats of water and land differently, using different
#'   surface friction coefficient for each.
#'
#'   \eqn{v_r = F\left(v_m - S \times (1 - \sin(T)) \times \frac{v_h}{2} \right) \times
#'   \sqrt{\left(\frac{R_m}{r}\right)^b \times e^{1 - \left(\frac{R_m}{r}\right)^b}}}
#'
#'   with,
#'
#'   \eqn{b = \frac{\rho \times e \times v_m^2}{p_{oci} - p_c}}
#'
#'   where, \eqn{v_r} is the tangential wind speed (in \eqn{m.s^{-1}}),
#'   \eqn{F} is a scaling parameter for friction (\eqn{1.0} in water, \eqn{0.8} in land),
#'   \eqn{v_m} is the maximum sustained wind speed (in \eqn{m.s^{-1}}),
#'   \eqn{S} is a scaling parameter for asymmetry (usually set to \eqn{1}),
#'   \eqn{T} is the oriented angle (clockwise/counter clockwise in Northern/Southern Hemisphere) between
#'   the forward trajectory of the storm and a radial line from the eye of the storm to point $r$
#'   \eqn{v_h} is the storm velocity (in \eqn{m.s^{-1}}),
#'   \eqn{R_m} is the radius of maximum sustained wind speed (in \eqn{km}),
#'   \eqn{r} is the distance to the eye of the storm (in \eqn{km}),
#'   \eqn{b} is the shape parameter,
#'   \eqn{\rho = 1.15} is the air density (in \eqn{kg.m^{-3}}),
#'   \eqn{p_{oci}} is the pressure at outermost closed isobar of the storm (in \eqn{Pa}), and
#'   \eqn{p_c} is the pressure at the centre of the storm (\eqn{pressure} in \eqn{Pa}).
#'
#' @references
#' Boose, E. R., Serrano, M. I., & Foster, D. R. (2004). Landscape and regional impacts of hurricanes in Puerto Rico.
#' Ecological Monographs, 74(2), Article 2. https://doi.org/10.1890/02-4057
#'
#' Chen, K.-M. (1994). A computation method for typhoon wind field. Tropic Oceanology, 13(2), 41–48.
#'
#' Holland, G. J. (1980). An Analytic Model of the Wind and Pressure Profiles in Hurricanes.
#' Monthly Weather Review, 108(8), 1212–1218. https://doi.org/10.1175/1520-0493(1980)108<1212:AAMOTW>2.0.CO;2
#'
#' Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann, C. J. (2010).
#' The International Best Track Archive for Climate Stewardship (IBTrACS).
#' Bulletin of the American Meteorological Society, 91(3), Article 3. https://doi.org/10.1175/2009bams2755.1
#'
#' Miyazaki, M., Ueno, T., & Unoki, S. (1962). The theoretical investigations of
#' typhoon surges along the Japanese coast (II). Oceanographical Magazine, 13(2), 103–117.
#'
#' Willoughby, H. E., Darling, R. W. R., & Rahn, M. E. (2006). Parametric Representation of
#' the Primary Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles.
#' Monthly Weather Review, 134(4), 1102–1120. https://doi.org/10.1175/MWR3106.1
#'
#' @examples
#' \donttest{
#' # Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' # Geting storm track data for tropical cyclone Pam (2015) near Vanuatu
#' pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#'
#' # Computing maximum sustained wind speed generated by Pam (2015) near Vanuatu
#' # using default settings
#' msw.pam <- spatialBehaviour(pam)
#'
#' # Computing PDI generated by Pam (2015) near Vanuatu using the Holland model without asymmetry
#' pdi.pam <- spatialBehaviour(pam, method = "Holland", product = "PDI", asymmetry = "None")
#'
#' # Computing duration of exposure to Saffir-Simpson hurricane wind scale threshold values
#' # during Pam (2015) near Vanuatu using default settings
#' exp.pam <- spatialBehaviour(pam, product = "Exposure")
#'
#' # Computing wind speed and direction profiles  generated by Pam (2015) near Vanuatu
#' # using Boose model
#' prof.pam <- spatialBehaviour(pam, product = "Profiles", method = "Boose")
#' }
#'
#' @export
spatialBehaviour <- function(sts,
                             product = "MSW",
                             windThreshold = NULL,
                             method = "Willoughby",
                             asymmetry = "Chen",
                             empiricalRMW = FALSE,
                             spaceRes = "2.5min",
                             tempRes = 60,
                             verbose = 2) {
  startTime <- Sys.time()

  if (is.null(windThreshold)) {
    windThreshold = sts@scale
  }

  checkInputsSpatialBehaviour(
    sts, product, windThreshold, method, asymmetry,
    empiricalRMW, spaceRes, tempRes, verbose
  )

  if (verbose > 0) {
    cat("=== spatialBehaviour processing ... ===\n\n")
    cat("Initializing data ...")
  }

  # Make raster template
  rasterTemplate <- makeTemplateRaster(sts@spatialLoiBuffer, resolutions[spaceRes])
  # Buffer size in degree
  buffer <- 2.5
  # Initializing final raster stacks
  finalStackMSW <- c()
  finalStackPDI <- c()
  finalStackEXP <- c()
  finalStackWind <- c()

  if (method == "Boose") {
    # Map for intersection
    world <- rworldmap::getMap(resolution = "high")
    world <- sf::st_as_sf(world)
    world <- sf::st_transform(world, crs = wgs84)
    indCountries <- which(sf::st_intersects(sts@spatialLoiBuffer, world$geometry, sparse = FALSE) == TRUE)
    asymmetry <- "None"
  } else {
    indCountries <- NULL
  }

  if (verbose > 0) {
    s <- 1 # Initializing count of storms
    cat(" Done\n\n")
    cat("Computation settings:\n")
    cat("  (*) Temporal resolution: Every", tempRes, " minutes\n")
    cat("  (*) Space resolution:", names(resolutions[spaceRes]), "\n")
    cat("  (*) Method used:", method, "\n")
    cat("  (*) Product(s) to compute:", product, "\n")
    cat("  (*) Asymmetry used:", asymmetry, "\n")
    if (empiricalRMW) {
      cat("  (*) rmw computed according to the empirical formula (See Details section)")
    }

    cat("\nStorm(s):\n")
    cat("  (", getNbStorms(sts), ") ", getNames(sts), "\n\n")
  }

  for (st in sts@data) {
    # Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, product)

    # Getting data associated with storm st
    dataTC <- getDataInterpolate(st, ind, tempRes, empiricalRMW, method)

    nbStep <- dim(dataTC)[1] - 1

    if (verbose > 0) {
      step <- 1
      cat(st@name, " (", s, "/", getNbStorms(sts), ")\n")
      pb <- utils::txtProgressBar(min = step, max = nbStep, style = 3)
    }

    auxStackMSW <- c()
    auxStackPDI <- c()
    auxStackEXP <- c()
    auxStackSpeed <- c()
    auxStackDirection <- c()



    for (j in 1:nbStep) {
      # Making template to compute wind profiles
      rasterTemplateModel <- makeTemplateModel(rasterTemplate, buffer, dataTC, j)
      rasterWind <- rasterTemplateModel
      rasterDirection <- rasterTemplateModel

      # Computing coordinates of raster
      crds <- terra::crds(rasterWind, na.rm = FALSE)

      # Computing distances in degree to the eye of the storm for x and y axes
      x <- crds[, 1] - dataTC$lon[j]
      y <- crds[, 2] - dataTC$lat[j]

      # Computing distances to the eye of the storm in m
      distEye <- terra::distance(
        x = crds,
        y = cbind(dataTC$lon[j], dataTC$lat[j]),
        lonlat = TRUE
      )

      # Computing wind speed/direction
      output <- computeWindProfile(
        dataTC, j, method, asymmetry,
        x, y, crds, distEye, buffer,
        sts@spatialLoiBuffer,
        world, indCountries
      )

      terra::values(rasterWind) <- output$wind
      terra::values(rasterDirection) <- output$direction

      # Stacking products
      if ("MSW" %in% product) {
        auxStackMSW <- stackProduct("MSW", auxStackMSW, rasterTemplate, rasterWind, NULL)
      }
      if ("PDI" %in% product) {
        auxStackPDI <- stackProduct("PDI", auxStackPDI, rasterTemplate, rasterWind, NULL)
      }
      if ("Exposure" %in% product) {
        auxStackEXP <- stackProduct("Exposure", auxStackEXP, rasterTemplate, rasterWind, windThreshold)
      }
      if ("Profiles" %in% product) {
        names(rasterWind) <- paste0(st@name, "_", "Speed", "_", dataTC$indices[j])
        names(rasterDirection) <- paste0(st@name, "_", "Direction", "_", dataTC$indices[j])
        auxStackSpeed <- stackRaster(auxStackSpeed, rasterTemplate, rasterWind)
        auxStackDirection <- stackRaster(auxStackDirection, rasterTemplate, rasterDirection)
      }


      if (verbose > 0) {
        utils::setTxtProgressBar(pb, step)
        step <- step + 1
      }
    }


    if (verbose > 0) {
      close(pb)
    }


    # Rasterize final products
    if ("MSW" %in% product) {
      auxStackMSW <- terra::rast(auxStackMSW)
      finalStackMSW <- rasterizeProduct(
        "MSW", finalStackMSW, auxStackMSW,
        tempRes, spaceRes, st@name, NULL
      )
      terra::time(finalStackMSW[[length(finalStackMSW)]]) <- terra::time(auxStackMSW[[1]])
    }
    if ("PDI" %in% product) {
      auxStackPDI <- terra::rast(auxStackPDI)
      finalStackPDI <- rasterizeProduct(
        "PDI", finalStackPDI, auxStackPDI,
        tempRes, spaceRes, st@name, NULL
      )
      terra::time(finalStackPDI[[length(finalStackPDI)]]) <- terra::time(auxStackPDI[[1]])
    }
    if ("Exposure" %in% product) {
      auxStackEXP <- terra::rast(auxStackEXP)
      finalStackEXP <- rasterizeProduct(
        "Exposure", finalStackEXP, auxStackEXP,
        tempRes, spaceRes, st@name, windThreshold
      )
    }
    if ("Profiles" %in% product) {
      auxStackSpeed <- terra::rast(auxStackSpeed)
      auxStackDirection <- terra::rast(auxStackDirection)
      finalStackWind <- c(finalStackWind, auxStackSpeed, auxStackDirection)
    }

    if (verbose > 0) {
      s <- s + 1
    }
  }

  finalStack <- c()

  if ("MSW" %in% product) {
    finalStack <- c(finalStack, finalStackMSW)
  }
  if ("PDI" %in% product) {
    finalStack <- c(finalStack, finalStackPDI)
  }
  if ("Exposure" %in% product) {
    finalStack <- c(finalStack, finalStackEXP)
  }
  if ("Profiles" %in% product) {
    finalStack <- c(finalStack, finalStackWind)
  }

  finalStack <- terra::rast(finalStack)
  finalStack <- maskProduct(finalStack, sts@spatialLoiBuffer, rasterTemplate)

  endTime <- Sys.time()

  if (verbose > 0) {
    cat("\n=== DONE with run time", as.numeric(round(endTime - startTime, 3)), "sec ===\n\n")

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
  }

  return(finalStack)
}
