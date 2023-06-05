




########
# MODELS#
########





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
#' Compute radial wind speed according to Willoughby et al. (2006) model
#'
#' @noRd
#' @param r numeric array. Distance to the eye of the storm (km) where the value must
#'   be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -60 and 60. Latitude of the eye of the
#'   storm
#'
#' @returns radial wind speed value (m/s) according to Willoughby model at
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


#' Willoughby et al. (2006) model
#'
#' Compute radial wind speed according to Willoughby et al. (2006) model in cpp
#'
#' @noRd
#' @param r numeric. Distance to the eye of the storm (km) where the value must
#'   be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -60 and 60. Latitude of the eye of the
#'   storm
#'
#' @returns radial wind speed value (m/s) according to Willoughby model at
#'   distance `r` to the eye of the storm located in latitude `lat`
#' @importFrom Rcpp cppFunction
#'

Rcpp::cppFunction("
  NumericVector willoughby_cpp(NumericVector r, double rmw,
            double msw, double lat) {
  int s = r.size();
  NumericVector vr(s);
  double x2 = 25;
  double x1 = 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat);
  double a = 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat);
  double n = 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat);
  for(int i = 0; i < s; ++i) {
    if (r[i] >= rmw) {
      vr[i] = msw * ((1 - a) * exp(-abs((r[i] - rmw) / x1)) +
              a * exp(-abs(r[i] - rmw) / x2));
    } else {
      vr[i] = msw * abs(pow(r[i] / rmw,n));
    }
    vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
  }
  return vr;
}")


#' Holland (1980) model
#'
#' Compute radial wind speed according to Holland (1980) model
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
#' @returns radial wind speed value (m/s) according to Holland 80 model at
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
#' Compute radial wind speed according to Boose et al. (2004) model
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
#' @returns radial wind speed value (m/s) according to Boose04 model at distance
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
  stopifnot("invalid tempRes: must be either 1, 0.75, 0.5 or 0.25" = tempRes %in% c(1, 0.75, 0.5, 0.25))

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
#'
#' @return SpatRaster
makeTemplateModel <- function(rasterTemplate, buffer, data) {
  template <- terra::rast(
    xmin = min(data$lon) - buffer,
    xmax = max(data$lon) + buffer,
    ymin = min(data$lat) - buffer,
    ymax = max(data$lat) + buffer,
    nlyr = length(data$isoTimes),
    resolution = terra::res(rasterTemplate),
    vals = NA,
    time = as.POSIXct(data$isoTimes)
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
#' @param dt numeric. time step
#' @param timeDiff numeric. Time diff in database
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
getDataInterpolate <- function(st, indices, dt, timeDiff, empiricalRMW, method) {
  lenIndices <- length(indices)
  lenData <- dt * (lenIndices - 1) - (lenIndices - 2)
  indicesObs <- seq(1, lenData, dt - 1)

  data <- data.frame(
    lon = rep(NA, lenData),
    lat = rep(NA, lenData),
    stormSpeed = rep(NA, lenData),
    vxDeg = rep(NA, lenData),
    vyDeg = rep(NA, lenData),
    msw = rep(NA, lenData),
    rmw = rep(NA, lenData),
    indices = rep(NA, lenData),
    isoTimes = rep(NA, lenData)
  )

  # Filling indices and isoTimes
  ind <- c()
  isoT <- c()
  timeRes <- 1 / ((dt - 1) / timeDiff) * 60

  for (i in indices[1:lenIndices - 1]) {
    lab <- as.character(i)
    t <- st@obs.all$iso.time[i]
    for (j in 1:(dt - 2)) {
      lab <- c(lab, paste0(as.character(i), ".", as.character(j)))
      t <- c(t, as.character(as.POSIXct(st@obs.all$iso.time[i]) + j * 60 * timeRes))
    }
    ind <- c(ind, lab)
    isoT <- c(isoT, t)
  }
  ind <- c(ind, as.character(indices[lenIndices]))
  isoT <- c(isoT, st@obs.all$iso.time[indices[lenIndices]])

  data$indices <- ind
  data$isoTimes <- isoT


  lon <- st@obs.all$lon[indices]
  lat <- st@obs.all$lat[indices]

  stormSpeed <- rep(NA, lenIndices)
  vxDeg <- rep(NA, lenIndices)
  vyDeg <- rep(NA, lenIndices)

  # Computing storm velocity (m/s)
  stormSpeed[1:lenIndices - 1] <- terra::distance(
    x = cbind(lon[1:lenIndices - 1], lat[1:lenIndices - 1]),
    y = cbind(lon[2:lenIndices], lat[2:lenIndices]),
    lonlat = TRUE,
    pairwise = TRUE
  ) * (0.001 / 3) / 3.6

  # component wise velocity in both x and y direction (degree/h)
  vxDeg[1:lenIndices - 1] <- (lon[2:lenIndices] - lon[1:lenIndices - 1]) / 3
  vyDeg[1:lenIndices - 1] <- (lat[2:lenIndices] - lat[1:lenIndices - 1]) / 3

  data$msw[indicesObs] <- st@obs.all$msw[indices]

  if (empiricalRMW) {
    data$rmw[indicesObs] <- getRmw(data$msw[indicesObs], lat)
  } else {
    if (!("rmw" %in% colnames(st@obs.all))) {
      warning("Missing rmw data to perform model. empiricalRMW set to TRUE")
      data$rmw[indicesObs] <- getRmw(data$msw[indicesObs], lat)
    } else if (all(is.na(st@obs.all$rmw[indices]))) {
      warning("Missing rmw data to perform model. empiricalRMW set to TRUE")
      data$rmw[indicesObs] <- getRmw(data$msw[indicesObs], lat)
    } else {
      data$rmw[indicesObs] <- st@obs.all$rmw[indices]
    }
  }

  data$lon[indicesObs] <- lon
  data$lat[indicesObs] <- lat
  data$stormSpeed[indicesObs] <- stormSpeed
  data$vxDeg[indicesObs] <- vxDeg
  data$vyDeg[indicesObs] <- vyDeg

  # Interpolate data
  data$lon <- zoo::na.approx(data$lon)
  data$lat <- zoo::na.approx(data$lat)
  data$msw <- zoo::na.approx(data$msw, rule = 2)
  data$rmw <- zoo::na.approx(data$rmw, rule = 2)

  for (i in 1:(dt - 2)) {
    ind <- indicesObs + i
    ind <- ind[seq_along(ind) - 1]
    data$stormSpeed[ind] <- stormSpeed[seq_along(ind)]
    data$vxDeg[ind] <- vxDeg[seq_along(ind)]
    data$vyDeg[ind] <- vyDeg[seq_along(ind)]
  }


  if (method == "Holland" || method == "Boose") {
    if (all(is.na(st@obs.all$poci[indices])) || all(is.na(st@obs.all$pres[indices]))) {
      stop("Missing pressure data to perform Holland model")
    }

    data$poci <- rep(NA, lenData)
    data$pc <- rep(NA, lenData)
    data$poci[indicesObs] <- st@obs.all$poci[indices]
    data$pc[indicesObs] <- st@obs.all$pres[indices]

    # Interpolate data
    data$poci <- zoo::na.approx(data$poci)
    data$pc <- zoo::na.approx(data$pc)
  }


  return(data)
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
    wind <- willoughby_cpp(
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
      asymmetry, wind, direction, x, y,
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
computeAsymmetry <- function(asymmetry, wind, direction, x, y, vx, vy, vh, r, rmw, lat) {
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

code <- "
NumericVector computeAsymmetry_cpp(
      CharacterVector asymmetry,
      NumericVector wind,
      NumericVector x,
      NumericVector y,
      double vx,
      double vy,
      double vh,
      double r,
      double rmw,
      double lat) {
  double pi = 3.141592653589793238463 ;
  int s = wind.size();
  NumericVector dir = -(atan(y / x) - pi / 2);
  if (lat >= 0) {
    dir = dir - pi / 2;
  } else {
    dir = dir + pi / 2;
  }

  for(int i = 0; i < s; i++) {
    if (dir[i] < 0) {
      dir[i] += 2 * pi;
    } else if (dir[i] > 2 * pi) {
      dir[i] -= 2 * pi;
    }
  }

  NumericVector windX = wind * cos(dir);
  NumericVector windY = wind * sin(dir);

  double stormDir = -(atan(vy / vx) - pi / 2);
  if (stormDir < 0) {stormDir += 2 * pi;}
  double mWindX = vh * cos(stormDir);
  double mWindY = vh * sin(stormDir);

  if (asymmetry == CharacterVector::create(\"Chen\")) {
    double formula = 3 * pow(rmw, 3 / 2) * pow(r, 3 / 2) / (pow(rmw, 3) + pow(r, 3) + pow(rmw, 3 / 2) * pow(r, 3 / 2));
  } else if (asymmetry == CharacterVector::create(\"Miyazaki\")) {
    double formula = exp(-r / 500 * pi);
  }

  NumericVector tWindX = windX + formula * mWindX;
  NumericVector tWindY = windY + formula * mWindY;

  NumericVector twind = sqrt(pow(tWindX, 2) + pow(tWindY, 2));
  NumericVector direction = atan(tWindY / tWindX) * 180 / pi;

  for(int i = 0; i < s; i++) {
    if (direction[i] < 0) {
      direction[i] += 360;
    }
  }

  return List::create(std::round(twind * 1000.0) / 1000.0, std::round(direction * 1000.0) / 1000.0);
}"



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
#'   compute the duration of exposure when `product="Exposure"`. By default the thresholds
#'   used in the Saffir-Simpson hurricane wind scale are used (i.e., 18, 33, 42, 49, 58, 70 \eqn{m.s^{-1}}).
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
#' @param tempRes numeric. Temporal resolution. Can be `1` (for 60 min, default setting),
#'  `0.75` (for 45min), `0.5` (for 30 min), and `0.25` (15 for min).
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
#'   (default value = 1 hour). When `product = "MSW"`, `product = "PDI"`,
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
#'   where, \eqn{v_r} is the radial wind speed (in \eqn{m.s^{-1}}),
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
#'   where, \eqn{v_r} is the radial wind speed (in \eqn{m.s^{-1}}),
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
#'   where, \eqn{v_r} is the radial wind speed (in \eqn{m.s^{-1}}),
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
#' \dontrun{
#' # Creating a stormsDataset
#' sds <- defDatabase()
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
                             windThreshold = c(18, 33, 42, 49, 58, 70),
                             method = "Willoughby",
                             asymmetry = "Chen",
                             empiricalRMW = FALSE,
                             spaceRes = "2.5min",
                             tempRes = 1,
                             verbose = 2) {
  startTime <- Sys.time()

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
  # Initializing final raster
  nbStorms <- getNbStorms(sts)
  nbRasters <- 0
  # Don't deal with Profiles so far
  if ("MSW" %in% product) {
    nbRasters <- nbRasters + nbStorms
  }
  if ("PDI" %in% product) {
    nbRasters <- nbRasters + nbStorms
  }
  if ("Exposure" %in% product) {
    nbRasters <- nbRasters + length(windThreshold) * nbStorms
  }

  finalRaster <- rep(rasterTemplate, nbRasters)

  # For computing / smoothing
  nbg <- switch(spaceRes,
                "30sec" = 59,
                "2.5min" = 11,
                "5min" = 5,
                "10min" = 3
  )

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
    cat("  (*) Temporal resolution: Every", switch(as.numeric(tempRes),
      "1" = 60,
      "0.75" = 45,
      "0.5" = 30,
      "0.25" = 15
    ), "min\n")
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

  # Handle the indice of layer
  i <- 0
  # Loop over storms
  for (st in sts@data) {
    # Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, product)


    it1 <- st@obs.all$iso.time[1]
    it2 <- st@obs.all$iso.time[2]
    timeDiff <- as.numeric(as.POSIXct(it2) - as.POSIXct(it1))
    # Interpolated time step dt, default value dt <- 4 --> 1h
    dt <- 1 + (1 / tempRes * timeDiff) # + 1 for the limit values

    # Getting data associated with storm st
    dataTC <- getDataInterpolate(st, ind, dt, timeDiff, empiricalRMW, method)

    nbStep <- dim(dataTC)[1] - 1

    if (verbose > 0) {
      step <- 1
      cat(st@name, " (", s, "/", getNbStorms(sts), ")\n")
      pb <- utils::txtProgressBar(min = step, max = nbStep, style = 3)
    }


    # Making template to compute wind profiles
    rasterWind <- rep(rasterTemplate, nbStep)
    rasterDirection <- rep(rasterTemplate, nbStep)

    # Computing coordinates of raster
    crds <- terra::crds(rasterWind, na.rm = FALSE)

    for (j in 1:nbStep) {
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

      terra::values(rasterWind[[j]]) <- output$wind
      terra::values(rasterDirection[[j]]) <- output$direction
      names(rasterWind[[j]]) <- paste0(st@name, "_", "Speed", "_", dataTC$indices[j])
      names(rasterDirection[[j]]) <- paste0(st@name, "_", "Direction", "_", dataTC$indices[j])

      if (verbose > 0) {
        utils::setTxtProgressBar(pb, step)
        step <- step + 1
      }
    }

    # Compute products
    if ("MSW" %in% product) {
      tmp <- max(rasterWind, na.rm = TRUE)
      # Applying focal function to smooth results
      tmp <- terra::focal(tmp, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = TRUE)
      names(tmp) <- paste0(getNames(st), "_MSW")
      terra::time(tmp) <- terra::time(rasterWind[[1]])
      finalRaster[[i]] <- tmp
      i <- i + 1
    }
    if ("PDI" %in% product) {
      rho <- 1
      cd <- 0.002
      # Integrating over the whole track
      tmp <- sum(rasterWind**3 * rho * cd, na.rm = TRUE) * tempRes
      # Applying focal function to smooth results
      tmp <- terra::focal(tmp, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = TRUE)
      names(tmp) <- paste0(getNames(st), "_PDI")
      terra::time(tmp) <- terra::time(rasterWind[[1]])
      finalRaster[[i]] <- tmp
      i <- i + 1
      }
    if ("Exposure" %in% product) {
      rasterExp <- rasterWind
      terra::values(rasterExp) <- findInterval(terra::values(rasterWind), windThreshold)
      for (l in seq_along(windThreshold)) {
        # Integrating over the whole track
        prod <- sum(terra::subset(stack, ind), na.rm = TRUE) * tempRes
        # Applying focal function to smooth results
        prod <- terra::focal(prod, w = matrix(1, nbg, nbg), mean, na.rm = TRUE)
        names(prod) <- paste0(getNames(st), "_Exposure_", windThreshold[l])
        finalRaster[[i]] <- tmp
        i <- i + 1
      }
    }

    if (verbose > 0) {
      close(pb)
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





###############################
# Helpers for temporalBehaviour#
###############################





#' Check inputs for temporalBehaviour function
#'
#' @noRd
#' @param sts StormsList object
#' @param points data.frame
#' @param product character
#' @param windThreshold numeric
#' @param method character
#' @param asymmetry character
#' @param empiricalRMW logical
#' @param tempRes numeric
#' @param verbose logical
#' @return NULL
checkInputsTemporalBehaviour <- function(sts, points, product, windThreshold, method, asymmetry,
                                         empiricalRMW, tempRes, verbose) {
  # Checking sts input
  stopifnot("no data found" = !missing(sts))

  # Checking points input
  stopifnot("no data found" = !missing(points))
  stopifnot("points must be data.frame" = identical(class(points), "data.frame"))
  stopifnot(
    "colnames of points must be \"x\" (Eastern degree), \"y\" (Northern degree)" = colnames(points) == c("x", "y")
  )
  stopifnot("Invalid points coordinates" = points$x > -180 & points$x <= 360 &
    points$y >= -90 & points$y <= 90)

  # Checking product input
  stopifnot("Invalid product" = product %in% c("TS", "PDI", "Exposure"))
  stopifnot("Only one product must be chosen" = length(product) == 1)

  # Checking windThreshold input
  if ("Exposure" %in% product) {
    stopifnot("windThreshold must be numeric" = identical(class(windThreshold), "numeric"))
    stopifnot("invalid value(s) for windThreshold input (must be > 0)" = windThreshold > 0)
  }

  # Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland", "Boose"))
  stopifnot("Only one method must be chosen" = length(method) == 1)

  # Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "Chen", "Miyazaki"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  # Checking empiricalRMW input
  stopifnot("empiricalRMW must be logical" = identical(class(empiricalRMW), "logical"))

  # Checking tempRes input
  stopifnot("tempRes must be numeric" = identical(class(tempRes), "numeric"))
  stopifnot("tempRes must be length 1" = length(tempRes) == 1)
  stopifnot("invalid tempRes: must be either 1, 0.75, 0.5 or 0.25" = tempRes %in% c(1, 0.75, 0.5, 0.25))

  # Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1 or 2" = verbose %in% c(0, 1, 2))
}





#' rasterizePDI counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#'
#' @return numeric. PDI computed using the wind speed values in wind
computePDI <- function(wind, tempRes) {
  # Computing surface drag coefficient
  rho <- 1
  cd <- 0.002
  # Raising to power 3
  pdi <- wind**3
  # Applying both rho and surface drag coefficient
  pdi <- pdi * rho * cd
  # Integrating over the whole track
  pdi <- sum(pdi, na.rm = TRUE) * tempRes

  return(round(pdi, 3))
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





#' rasterizeProduct counterpart function for non raster data
#'
#' @noRd
#' @param product character. Product input from temporalBehaviour
#' @param wind numeric vector. Wind speed values
#' @param direction numeric vector. Wind direction
#' @param tempRes numeric. Time resolution, used for the numerical integration
#'   over the whole track
#' @param result numeric array. Similar as finalStack, i.e where to add the
#'   computed product
#' @param threshold numeric vector. Wind threshold
#'
#' @return numeric array of dimension:
#' \itemize{
#'   \item number of observations: number of points. If product == "MSW"
#'   \item 1 : number of points. If product == "PDI"
#'   \item 5 : number of points. If product == "Exposure"
#' }
computeProduct <- function(product, wind, direction, tempRes, result, threshold) {
  if (product == "TS") {
    prod <- cbind(wind, direction)
  } else if (product == "PDI") {
    prod <- computePDI(wind, tempRes)
  } else if (product == "Exposure") {
    prod <- computeExposure(wind, tempRes, threshold)
  }

  return(cbind(result, prod))
}





#' Arrange result before the end of temporalBehaviour
#'
#' @noRd
#' @param finalResult list of data.frame. Where to add the computed product
#' @param result result output from computeProduct function
#' @param product character. Product input from temporalBehaviour
#' @param points points input from temporalBehaviour
#' @param isoT numeric vector. Iso Times of observations
#' @param indices numeric vector. Indices of observations
#' @param st Storm object.
#' @param threshold numeric vector. Wind threshold
#'
#' @return finalResult
finalizeResult <- function(finalResult, result, product, points, isoT, indices, st, threshold) {
  if (product == "TS") {
    l <- list()
    for (i in 1:dim(points)[1]) {
      if (i > 1) {
        i <- i + 1
      }

      df <- data.frame(result[, i], result[, i:i + 1], indices = indices, isoTimes = isoT)
      colnames(df) <- c("speed", "direction", "indices", "isoTimes")

      l <- append(l, list(df))
    }
    names(l) <- rownames(points)
  } else if (product == "PDI") {
    l <- data.frame(result, row.names = "PDI")
    colnames(l) <- rownames(points)
  } else {
    l <- data.frame(result, row.names = paste("Min threshold:", threshold, "m/s"))
    colnames(l) <- rownames(points)
  }

  dfn <- list(l)
  names(dfn) <- st@name
  finalResult <- append(finalResult, dfn)

  return(finalResult)
}





############################
# temporalBehaviour function#
############################





#' Computing wind behaviour time series and summary statistics at given point locations
#'
#' The `temporalBehaviour()` function allows computing wind speed and direction
#' for a given location or set of locations along the lifespan of a tropical cyclone.
#' It also allows to compute three associated summary statistics.
#'
#' @param sts `StormsList` object.
#' @param points data.frame. Consisting of two columns names as "x" (for the longitude) and
#' "y" (for the latitude), providing the coordinates in decimal degrees of the point locations. Row names
#' can also be provided to named the locations.
#' @param product character. Desired output statistics:
#'   \itemize{
#'     \item `"TS"`, for time series of wind speeds and directions (default setting),
#'     \item `"PDI"`, for power dissipation index, or
#'     \item `"Exposure"`, for the duration of exposure to defined wind thresholds.
#'   }
#' @param windThreshold numeric vector. Minimal wind threshold(s) (in \eqn{m.s^{-1}}) used to
#'   compute the duration of exposure when `product="Exposure"`. By default the thresholds
#'   used in the Saffir-Simpson hurricane wind scale are used (i.e., 18, 33, 42, 49, 58, 70 \eqn{m.s^{-1}}).
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
#' @param tempRes numeric. Temporal resolution. Can be `1` (for 60 min, default setting),
#'  `0.75` (for 45min), `0.5` (for 30 min), and `0.25` (15 for min).
#' @param verbose numeric. Information displayed. Can be:
#' \itemize{
#'    \item `2`, information about the processes and outputs are displayed (default setting),
#'    \item `1`, information about the processes are displayed, or
#'    \item `0`, no information displayed.
#' }
#' @returns For each storm and each point location, the `temporalBehaviour()` function returns
#' a data.frame. The data frames are organised into named lists. Depending on the `product` argument
#'  different data.frame are returned:
#' \itemize{
#'    \item if `product == "TS"`, the function returns a data.frame with
#'    one row for each observation (or interpolated observation) and
#'    four columns for wind speed (in \eqn{m.s^{-1}}), wind direction (in degree),
#'    the observation number, and the ISO time of observations,
#'    \item if `product == "PDI"`, the function returns a data.frame with one row
#'    for each point location and one column for the PDI,
#'    \item if `product == "Exposure"`, the function returns a data.frame with one
#'    row for the duration of exposure to winds above each wind speed threshold defined
#'    by the `windThreshold` argument and one column for each point location.
#'    }
#'
#' @details  Storm track data sets, such as those extracted from IBRTrACKS (Knapp et
#'   al., 2010), usually provide observation at a 3- or 6-hours temporal
#'   resolution. In the temporalBehaviour() function, linear interpolations are
#'   used to reach the temporal resolution specified in the `tempRes` argument
#'   (default value = 1 hour).
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
#'   where, \eqn{v_r} is the radial wind speed (in \eqn{m.s^{-1}}),
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
#'   where, \eqn{v_r} is the radial wind speed (in \eqn{m.s^{-1}}),
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
#'   \eqn{C = \frac{3 \times R_m^{\frac{3}{2}} \times
#'   r^{\frac{3}{2}}}{R_m^3 + r^3 +R_m^{\frac{3}{2}} \times r^{\frac{3}{2}}}}
#'
#'   where, \eqn{R_m} is the radius of maximum sustained wind speed (in \eqn{km})
#'
#'   The Boose et al. (2004) model, or “HURRECON” model, is a modification of the
#'   Holland (1980) model. In addition to adding
#'   asymmetry, this model treats of water and land differently, using different
#'   surface friction coefficient for each.
#'
#'   \eqn{v_r = F\left(v_m - S \times (1 - \sin(T)) \times \frac{v_h}{2} \right)
#'   \times \sqrt{\left(\frac{R_m}{r}\right)^b \times e^{1 - \left(\frac{R_m}{r}\right)^b}}}
#'
#'   with,
#'
#'   \eqn{b = \frac{\rho \times e \times v_m^2}{p_{oci} - p_c}}
#'
#'   where, \eqn{v_r} is the radial wind speed (in \eqn{m.s^{-1}}),
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
#' Holland, G. J. (1980). An Analytic Model of the Wind and Pressure
#' Profiles in Hurricanes. Monthly Weather Review, 108(8), 1212–1218.
#' https://doi.org/10.1175/1520-0493(1980)108<1212:AAMOTW>2.0.CO;2
#'
#' Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann, C. J. (2010). The
#' International Best Track Archive for Climate Stewardship (IBTrACS).
#' Bulletin of the American Meteorological Society, 91(3), Article 3. https://doi.org/10.1175/2009bams2755.1
#'
#' Miyazaki, M., Ueno, T., & Unoki, S. (1962). The theoretical investigations of typhoon surges
#' along the Japanese coast (II).
#' Oceanographical Magazine, 13(2), 103–117.
#'
#' Willoughby, H. E., Darling, R. W. R., & Rahn, M. E. (2006). Parametric Representation of the
#' Primary Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles.
#' Monthly Weather Review, 134(4), 1102–1120. https://doi.org/10.1175/MWR3106.1
#'
#' @examples
#' \dontrun{
#' # Creating a stormsDataset
#' sds <- defDatabase()
#'
#' # Geting storm track data for tropical cyclone Pam (2015) near Vanuatu
#' pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#'
#' pts <- data.frame(x = c(168.5, 168), y = c(-17.9, -16.3))
#' row.names(pts) <- c("point_1", "point_2")
#'
#' # Computing time series of wind speed and direction for Pam
#' # over points 1 and 2 defined above
#' ts.pam <- temporalBehaviour(pam, points = pts)
#'
#' # Computing PDI for Pam over points 1 and 2 defined above
#' pdi.pam <- temporalBehaviour(pam, points = pts, product = "PDI")
#'
#' # Computing the duration of exposure to wind speeds above the thresholds
#' # used by the Saffir-Simpson hurricane wind scale for Pam
#' # over points 1 and 2 defined above
#' exp.pam <- temporalBehaviour(pam, points = pts, product = "Exposure")
#' }
#'
#' @export
temporalBehaviour <- function(sts,
                              points,
                              product = "TS",
                              windThreshold = c(18, 33, 42, 49, 58, 70),
                              method = "Willoughby",
                              asymmetry = "Chen",
                              empiricalRMW = FALSE,
                              tempRes = 1,
                              verbose = 1) {
  checkInputsTemporalBehaviour(sts, points, product, windThreshold, method, asymmetry, empiricalRMW, tempRes, verbose)


  if (verbose > 0) {
    cat("=== temporalBehaviour processing ... ===\n\n")
    cat("Initializing data ...")
  }


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

  points$x[points$x < 0] <- points$x[points$x < 0] + 360

  buffer <- 2.5
  # Initializing final result
  finalResult <- list()

  if (verbose > 0) {
    cat(" Done\n\n")
    cat("Computation settings:\n")
    cat("  (*) Temporal resolution: Every", switch(as.numeric(tempRes),
      "1" = 60,
      "0.75" = 45,
      "0.5" = 30,
      "0.25" = 15
    ), "min\n")
    cat("  (*) Method used:", method, "\n")
    cat("  (*) Product(s) to compute:", product, "\n")
    cat("  (*) Asymmetry used:", asymmetry, "\n")
    if (empiricalRMW) {
      cat("  (*) rmw computed according to the empirical formula (See Details section)")
    }
    cat("  (*) Points: lon-lat\n")
    for (i in 1:dim(points)[1]) {
      cat("      ", points$x[i], " ", points$y[i], "\n")
    }
    cat("\n")

    cat("\nStorm(s):\n")
    cat("  (", getNbStorms(sts), ") ", getNames(sts), "\n\n")
  }


  for (st in sts@data) {
    # Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, "none")

    it1 <- st@obs.all$iso.time[1]
    it2 <- st@obs.all$iso.time[2]
    timeDiff <- as.numeric(as.POSIXct(it2) - as.POSIXct(it1))
    # Interpolated time step dt, default value dt <- 4 --> 1h
    dt <- 1 + (1 / tempRes * timeDiff) # + 1 for the limit values

    # Getting data associated with storm st
    dataTC <- getDataInterpolate(st, ind, dt, timeDiff, empiricalRMW, method)


    # Computing distances from the eye of storm for every observations x, and
    # every points y
    distEye <- terra::distance(
      x = cbind(dataTC$lon, dataTC$lat),
      y = cbind(points$x, points$y),
      lonlat = TRUE
    )

    res <- c()
    # For each point
    for (i in 1:dim(points)[1]) {
      # Coordinates
      pt <- points[i, ]

      # Computing distance between eye of storm and point P
      x <- pt$x - dataTC$lon
      y <- pt$y - dataTC$lat

      # Computing wind speed/direction
      dist2p <- distEye[, i]
      output <- computeWindProfile(
        dataTC, i, method, asymmetry, x, y, pt, dist2p,
        buffer, sts@spatialLoiBuffer,
        world, indCountries
      )

      vr <- output$wind
      dir <- output$direction

      # Computing product
      res <- computeProduct(product, vr, dir, tempRes, res, windThreshold)
    }

    finalResult <- finalizeResult(
      finalResult, res, product, points,
      dataTC$isoTimes, dataTC$indices, st,
      windThreshold
    )
  }

  if (verbose > 0) {
    cat("\n=== DONE ===\n\n")

    if (verbose > 1) {
      cat("Output:\n")
      cat("DataFrame with", length(finalResult), "storm:\n")
      cat("index - name of the storm - Observation Point - Point indices\n")
      stormNames <- names(finalResult)
      for (i in seq_along(stormNames)) {
        stormName <- stormNames[i]
        pointNames <- names(finalResult[[stormName]])
        for (j in seq_along(pointNames)) {
          pointName <- pointNames[j]
          cat(
            " ", i, "   ",
            stormName, "                ",
            pointName, "            ",
            finalResult[[stormName]][[pointName]]$indices, "\n"
          )
        }
      }
      cat("\n")
    }
  }

  return(finalResult)
}
