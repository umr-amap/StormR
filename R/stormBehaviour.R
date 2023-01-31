



#' Compute the Radius of Maximum Wind
#'
#' It is an empirical formula extracted from Willoughby et al. 2006 model
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the storm
#'
#' @returns Radius of Maximum Wind (km)
getRmw <- function(msw, lat) {
  return (46.4 * exp(-0.0155 * msw + 0.0169 * abs(lat)))
}





#' Willoughby et al. 2006 model
#'
#' Compute radial wind speed according to Willoughby et al. 2006 model
#'
#' @noRd
#' @param r numeric. Distance to the eye of the storm (km) where the value must be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -60 and 60. Latitude of the eye of the storm
#'
#' @returns radial wind speed value (m/s) according to Willoughby model at distance `r` to the
#'  eye of the storm located in latitude `lat`
Willoughby_profile <- function(r, rmw, msw, lat){

  if (r >= rmw) {
    xx1 <- 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat)
    xx2 <- 25
    aa <- 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat)
    vr <- msw * ((1 - aa) * exp(-abs((r - rmw) / xx1)) + aa * exp(-abs(r -
                                                                          rmw) / xx2))
  } else{
    nn <- 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat)
    vr <- msw * abs((r / rmw) ^ nn)
  }

  return(vr)
}

#Vectorize version of the above model
Willoughby <- Vectorize(Willoughby_profile, vectorize.args = "r")





#' Holland et al. 1980 model
#'
#' Compute radial wind speed according to Holland et al. 1980 model
#'
#' @noRd
#' @param r numeric. Distance to the eye of the storm (km) where the value must be computed
#' @param rmw numeric. Radius of Maximum Wind (km)
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param pc numeric. Pressure at the center of the storm (hPa)
#' @param poci Pressure at the Outermost Closed Isobar (hPa)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the storm
#' @returns radial wind speed value (m/s) according to Holland 80 model at distance `r` to the
#'  eye of the storm located in latitude `lat`
Holland_profile <- function(r, rmw, msw, pc, poci, lat){

  rho <- 1.15  #air densiy
  f <- 2 * 7.29 *10**(-5) * sin(lat) #Coriolis parameter
  b <- rho * exp(1) * msw**2 / (poci - pc)

  vr <- sqrt(b/rho * (rmw/r)**b * (poci - pc)*exp(-(rmw/r)**b) + (r*f/2)**2) - r*f/2

  return(vr)

}

#Vectorize version of the above model
Holland <- Vectorize(Holland_profile, vectorize.args = "r")





#' Parametrization of surface drag coefficient according to Wang, G. et al. 2022
#'
#' @noRd
#' @param vr numeric. Radial wind speed (m/s)
#'
#' @return Associated surface drag coefficient
compute_Cd <- function(vr){

  if(is.na(vr)){
    return(NA)
  }else if(vr <= 18){
    return(0)
  }else if(vr > 18 & vr <= 31.5){
    return((0.8 + 0.06 * vr) * 0.001)

  }else if(vr > 31.5){
    return((0.55 + 2.97 * vr/31.5 - 1.49 * (vr/ 31.5) ** 2) * 0.001)
  }
}

#Vectorize version of the above function
rasterizeCd <- Vectorize(compute_Cd, vectorize.args = "vr")





#' Check inputs for stormBehaviour function
#'
#' @noRd
#' @param sts Storms object
#' @param product character
#' @param method character
#' @param asymmetry character
#' @param empirical_rmw logical
#' @param format character
#' @param space_res numeric
#' @param time_res numeric
#' @param verbose logical
#' @param focus_loi logical
#' @return NULL
checkInputsSb <- function(sts, product, method, asymmetry,
                         empirical_rmw, format, space_res,
                         time_res, verbose, focus_loi){

  #Checking sts input
  stopifnot("no data found" = !missing(sts))

  #Checking product input
  stopifnot("Invalid product" = product %in% c("MSW", "PDI", "Exposure"))
  stopifnot("Only one product must be chosen" = length(product) == 1)

  #Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland"))
  stopifnot("Only one method must be chosen" = length(method) == 1)

  #Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "V1", "V2"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  #Checking empirical_rmw input
  stopifnot("empirical_rmw must be logical" = identical(class(empirical_rmw), "logical"))

  #Checking format input
  stopifnot("Invalid format input" = format %in% c("profiles","analytic"))
  stopifnot("format should be length 1" = length(format) == 1)

  #Checking space_res input
  stopifnot("space_res must be numeric" = identical(class(space_res), "numeric"))
  stopifnot("space_res must be length 1" = length(space_res) == 1)
  stopifnot("space_res must be positif" = space_res > 0)

  #Checking time_res input
  stopifnot("time_res must be numeric" = identical(class(time_res), "numeric"))
  stopifnot("time_res must be length 1" = length(time_res) == 1)
  stopifnot("invalid time_res: must be either 1, 0.75, 0.5 or 0.25" = time_res %in% c(1, 0.75, 0.5, 0.25))

  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose), "logical"))

  #Checking focus_loi input
  stopifnot("focus_loi must be logical" = identical(class(focus_loi), "logical"))


}





#' Check inputs for Unknow function
#'
#' @noRd
#' @param sts Storms object
#' @param points data.frame
#' @param product character
#' @param method character
#' @param asymmetry character
#' @param empirical_rmw logical
#' @param time_res numeric
#' @param focus_loi logical
#' @return NULL
checkInputsUnknow <- function(sts, points, product, method, asymmetry,
                          empirical_rmw, time_res, focus_loi){

  #Checking sts input
  stopifnot("no data found" = !missing(sts))

  #Checking points input
  stopifnot("no data found" = !missing(points))
  stopifnot("points must be data.frame" = identical(class(points), "data.frame"))
  stopifnot("colnames of points must be lon, lat" = colnames(points) == c("lon","lat"))
  stopifnot("Invalid points coordinates" = points$lon >= 0 & points$lon <= 360 &
              points$lat >= -90 & points$lat <= 90)

  #Checking product input
  stopifnot("Invalid product" = product %in% c("TS", "PDI", "Exposure"))
  stopifnot("Only one product must be chosen" = length(product) == 1)

  #Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland"))
  stopifnot("Only one method must be chosen" = length(method) == 1)

  #Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "V1", "V2"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  #Checking empirical_rmw input
  stopifnot("empirical_rmw must be logical" = identical(class(empirical_rmw), "logical"))

  #Checking time_res input
  stopifnot("time_res must be numeric" = identical(class(time_res), "numeric"))
  stopifnot("time_res must be length 1" = length(time_res) == 1)
  stopifnot("invalid time_res: must be either 1, 0.75, 0.5 or 0.25" = time_res %in% c(1, 0.75, 0.5, 0.25))

  #Checking focus_loi input
  stopifnot("focus_loi must be logical" = identical(class(focus_loi), "logical"))


}





#' Generate raster template for the computations
#'
#' @noRd
#' @param buffer sf object. LOI + buffer extention
#' @param res numeric. Space resolution (km) for the template
#'
#' @return a SpatRaster
makeTemplateRaster <- function(buffer, res){

  #Derivating the raster template
  ext <- terra::ext(sf::st_bbox(buffer)$xmin,
                    sf::st_bbox(buffer)$xmax,
                    sf::st_bbox(buffer)$ymin,
                    sf::st_bbox(buffer)$ymax)

  ras <- terra::rast(
    xmin = ext$xmin,
    xmax = ext$xmax,
    ymin = ext$ymin,
    ymax = ext$ymax,
    vals = NA
  )
  #Projection in Mercator
  ras <- terra::project(ras, "EPSG:3857")
  #Resampling in new resolution in Mercator
  template <- ras
  terra::res(template) <- c(res * 1000, res * 1000)
  template <- terra::resample(ras, template)
  #Reprojection in lon/lat
  template <- terra::project(template, "EPSG:4326")

  #Handling time line crossing
  if(terra::ext(template)$xmin < 0)
    template <- terra::rast(resolution = terra::res(template), extent = ext)

  return(template)

}





#' Get indices for computations
#'
#' Whether to get only observations inside LOI + buffer extention (+ offset) or
#' getting all the observations
#'
#' @noRd
#' @param st Storm Object
#' @param offset numeric. Offset to apply at the begining and at the end
#' @param format character. format input from stormBehaviour
#' @param focus_loi logical. logical input from stormBehaviour
#'
#' @return numeric vector gathering the indices of observation to use to perform
#' the further computations
getIndices <- function(st, offset, format, focus_loi){

  #Handling indices inside loi.buffer or not
  if (focus_loi) {
    #Use observations within the loi for the computations
    ind <- seq(st@obs[1], st@obs[length(st@obs)], 1)

    if(format == "analytic"){
      #Handling indices and offset (outside of loi at entry and exit)
      for(o in 1:offset){
        ind <- c(st@obs[1] - o, ind)
        ind <- c(ind, st@obs[length(st@obs)] + o)
      }

      #Remove negative values and values beyond st@numobs.all
      ind <- ind[ind > 0 & ind <= st@numobs.all]

    }

  } else{
    #Use all observations available for the computations
    ind <- seq(1, st@numobs.all, 1)
  }

  return(ind)
}





#' Get data associated with one storm to perform further computations
#'
#' @noRd
#' @param st Storm object
#' @param indices numeric vector extracted from getIndices
#' @param asymmetry character. Which version of asymmetry. If "V2", it substract
#' velocity of storm to maximum sustained windspeed for the further computations
#' @param empirical_rmw logical. Whether to use rmw from the data or to compute them
#' according to getRmw function
#' @param method character. method input from stormBehaviour
#'
#' @return a data.frame of dimension length(indices) : 10. Columns are
#'  \itemize{
#'    \item lon: numeric. Longitude coordinates (degree)
#'    \item lat: numeric. Latitude coordinates (degree)
#'    \item msw: numeric. Maximum Sustained Wind (m/s)
#'    \item rmw: numeric. Radius of Maximum Wind (km)
#'    \item roci: numeric. Radius of Outermost Closed Isobar (km)
#'    \item pc: numeric. Pressure at the center of the storm (mb)
#'    \item poci: numeric. Pressure of Outermost Closed Isobar (mb)
#'    \item storm.speed: numeric. Velocity of the storm (m/s)
#'    \item vx.deg: numeric. Velocity of the speed in the x direction (deg/h)
#'    \item vy.deg: numeric Velocity of the speed in the y direction (deg/h)
#'  }
getDataInterpolate <- function(st, indices, dt, asymmetry, empirical_rmw, method){


  len.indices <- length(indices)
  len.data <- dt * (len.indices - 1) - (len.indices-2)
  indices.obs <- seq(1, len.data, dt-1)

  data <- data.frame(lon = rep(NA, len.data),
                    lat = rep(NA, len.data),
                    storm.speed = rep(NA,len.data),
                    vx.deg = rep(NA,len.data),
                    vy.deg = rep(NA,len.data),
                    msw = rep(NA,len.data),
                    rmw = rep(NA,len.data))

  lon <- st@obs.all$lon[indices]
  lat <- st@obs.all$lat[indices]

  storm.speed <- rep(NA, len.indices)
  vx.deg <- rep(NA, len.indices)
  vy.deg <- rep(NA, len.indices)

  #Computing storm velocity (m/s)
  for(i in 1:(len.indices-1)){
    storm.speed[i] <- terra::distance(x = cbind(lon[i],lat[i]),
                                     y = cbind(lon[i+1],lat[i+1]),
                                     lonlat = T) * (0.001 / 3) / 3.6

    #component wise velocity in both x and y direction (degree/h)
    vx.deg[i] <- (lon[i + 1] - lon[i]) / 3
    vy.deg[i] <- (lat[i + 1] - lat[i]) / 3
  }

  if(asymmetry == "V2"){
    data$msw[indices.obs] <- st@obs.all$msw[indices] - storm.speed
  }else{
    data$msw[indices.obs] <- st@obs.all$msw[indices]
  }

  if(empirical_rmw){
    data$rmw[indices.obs] <- getRmw(data$msw[indices.obs], lat)
  }else{
    if(all(is.na(st@obs.all$rmw[indices]))){
      warning("Missing rmw data to perform model. Consider setting empirical_rmw to TRUE")
      data$rmw[indices.obs] <- getRmw(data$msw[indices.obs], lat)
    }else{
      data$rmw[indices.obs] <- st@obs.all$rmw[indices]
    }
  }

  data$lon[indices.obs] <- lon
  data$lat[indices.obs] <- lat
  data$storm.speed[indices.obs] <- storm.speed
  data$vx.deg[indices.obs] <- vx.deg
  data$vy.deg[indices.obs] <- vy.deg

  #Interpolate data
  data$lon <- zoo::na.approx(data$lon)
  data$lat <- zoo::na.approx(data$lat)
  data$msw <- zoo::na.approx(data$msw, rule = 2)
  data$rmw <- zoo::na.approx(data$rmw, rule = 2)

  for(i in 1:(dt-2)){
    ind <- indices.obs + i
    ind <- ind[1:(length(ind) - 1)]
    data$storm.speed[ind] <- storm.speed[1:length(ind)]
    data$vx.deg[ind] <- vx.deg[1:length(ind)]
    data$vy.deg[ind] <- vy.deg[1:length(ind)]
  }


  if(method == "Holland"){
    if(all(is.na(st@obs.all$poci[indices])) || all(is.na(st@obs.all$pres[indices])))
      stop("Missing pressure data to perform Holland model")

    data$poci <- rep(NA, len.data)
    data$pc <- rep(NA, len.data)
    data$poci[indices.obs] <- st@obs.all$poci[indices]
    data$pc[indices.obs] <- st@obs.all$pres[indices]

    #Interpolate data
    data$poci <- zoo::na.approx(data$poci)
    data$pc <- zoo::na.approx(data$pc)
  }


  return(data)

}





#' Get data associated with one storm to perform further computations
#'
#' @noRd
#' @param st Storm object
#' @param indices numeric vector extracted from getIndices
#' @param asymmetry character. Which version of asymmetry. If "V2", it substract
#' velocity of storm to maximum sustained windspeed for the further computations
#' @param empirical_rmw logical. Whether to use rmw from the data or to compute them
#' according to getRmw function
#' @param method character. method input from stormBehaviour
#'
#' @return a data.frame of dimension length(indices) : 10. Columns are
#'  \itemize{
#'    \item lon: numeric. Longitude coordinates (degree)
#'    \item lat: numeric. Latitude coordinates (degree)
#'    \item msw: numeric. Maximum Sustained Wind (m/s)
#'    \item rmw: numeric. Radius of Maximum Wind (km)
#'    \item roci: numeric. Radius of Outermost Closed Isobar (km)
#'    \item pc: numeric. Pressure at the center of the storm (mb)
#'    \item poci: numeric. Pressure of Outermost Closed Isobar (mb)
#'    \item storm.speed: numeric. Velocity of the storm (m/s)
#'    \item vx.deg: numeric. Velocity of the speed in the x direction (deg/h)
#'    \item vy.deg: numeric Velocity of the speed in the y direction (deg/h)
#'  }
getData <- function(st, indices , asymmetry, empirical_rmw, method){

  data <- data.frame(lon = st@obs.all$lon[indices],
                     lat = st@obs.all$lat[indices])

  data$storm.speed <- NA
  data$vx.deg <- NA
  data$vy.deg <- NA

  #Computing storm velocity (m/s)
  for(i in 1:(dim(data)[1]-1)){
    data$storm.speed[i] <- terra::distance(x = cbind(data$lon[i],data$lat[i]),
                                         y = cbind(data$lon[i+1],data$lat[i+1]),
                                         lonlat = T) * (0.001 / 3) / 3.6

    #component wise velocity in both x and y direction (degree/h)
    data$vx.deg[i] <- (data$lon[i + 1] - data$lon[i]) / 3
    data$vy.deg[i] <- (data$lat[i + 1] - data$lat[i]) / 3
  }

  if(asymmetry == "V2"){
    data$msw <- st@obs.all$msw[indices] - data$storm.speed
  }else{
    data$msw <- st@obs.all$msw[indices]
  }

  if(empirical_rmw){
    data$rmw <- getRmw(data$msw, data$lat)
  }else{
    if(all(is.na(st@obs.all$rmw[indices])))
      stop("Missing rmw data to perform model. Consider setting empirical_rmw to TRUE")
    data$rmw <- st@obs.all$rmw[indices]
  }

  if(method == "Holland"){
    if(all(is.na(st@obs.all$poci[indices])) || all(is.na(st@obs.all$pres[indices])))
      stop("Missing pressure data to perform Holland model")

    data$poci <- st@obs.all$poci[indices]
    data$pc <- st@obs.all$pres[indices]
  }


  return(data)

}





#' Generate raster template to compute wind speed according to the different models
#'
#' @noRd
#' @param raster_template SpatRaster. Raster generated with makeTemplateRaster function
#' @param buffer numeric. Buffer size in degree
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use to generate raster
#'
#' @return SpatRaster
makeTemplateModel <- function(raster_template, buffer, data, index){

  template <- terra::rast(xmin = data$lon[index] - buffer,
                         xmax = data$lon[index] + buffer,
                         ymin = data$lat[index] - buffer,
                         ymax = data$lat[index] + buffer,
                         res = terra::res(raster_template),
                         vals = NA)
  terra::origin(template) <- terra::origin(raster_template)

  return(template)

}





#' Version 1 of asymmetry (Boose et al. 2001)
#'
#' @noRd
#' @param x numeric vector. Distance(s) to the eye of the storm in the x direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y direction (deg)
#' @param vx numeric. Velocity of the storm in the x direction (deg/h)
#' @param vy numeric. Velociy of the storm in the y direction (deg/h)
#' @param basin character. Basin
#'
#' @return numeric vector. Orientation of wind speed (rad) at each (x,y) position
asymmetryV1 <- function(x, y, vx, vy, basin){

  if(basin %in% c("SA", "SP", "SI")){
    #Southern Hemisphere, t is counterclockwise
    res <- atan2(vy,vx) - atan2(y,x) + pi

  }else{
    #Northern Hemisphere, t is clockwise
    res <- atan2(y,x) - atan2(vy,vx)  + pi
  }

  return(res)
}





#' Version 2 of asymmetry
#'
#' @noRd
#' @param x numeric vector. Distance(s) to the eye of the storm in the x direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y direction (deg)
#' @param vx numeric. Velocity of the storm in the x direction (deg/h)
#' @param vy numeric. Velociy of the storm in the y direction (deg/h)
#' @param basin character. Basin
#'
#' @return numeric vector. Orientation of wind speed (rad) at each (x,y) position
asymmetryV2 <- function(x, y, vx, vy, basin){

  if(basin %in% c("SA", "SP", "SI")){
    #Southern Hemisphere, t is clockwise
    res <- acos((y * vx - x * vy) / (sqrt(vx**2 + vy**2) * sqrt(x**2 + y**2)))

  }else{
    #Northern Hemisphere, t is counterclockwise
    res <- acos((- y * vx + x * vy) / (sqrt(vx**2 + vy**2) * sqrt(x**2 + y**2)))
  }

  return(res)
}





#' Compute wind profile according to the selected method and asymmetry
#'
#' @noRd
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use for
#' the computations
#' @param dist_m numeric array. Distance in meter from the eye of the storm for
#' each coordinate of the raster_template_model (or points if format is a data frame)
#' @param method character. method input form stormBehaviour
#' @param asymmetry character. asymmetry input from stormBehviour
#' @param basin numeric. basin name to choose wind orientation
#' @param x numeric array. Distance in degree from the eye of storm in the x direction
#' @param y numeric array. Distance in degree from the eye of storm in the y direction
#'
#' @return
#'   \itemize{
#'     \item If format is a data frame, the wind speed values at each observation
#'     \item Otherwise, SpatRaster that contains the wind speed profile at observations index of data
#'  }
computeWindProfile <- function(data, index, dist_m, method, asymmetry, basin, x, y){


  #Computing sustained wind according to the input model
  if (method == "Willoughby") {
    wind <- Willoughby(
      msw = data$msw[index],
      lat = data$lat[index],
      r = dist_m * 0.001,
      rmw = data$rmw[index]
    )

  }else if (method == "Holland") {
    wind <- Holland(
      r = dist_m * 0.001,
      rmw = data$rmw[index],
      msw = data$msw[index],
      pc = data$pc[index] * 100,
      poci = data$poci[index] * 100,
      lat = data$lat[index]
    )
  }

  #Adding asymmetry
  if (asymmetry == "V1") {
    #Boose version
    angle <- asymmetryV1(x, y, data$vx.deg[index], data$vy.deg[index], basin)
    wind <- wind - (1 - sin(angle))*(data$storm.speed[index]/3.6)/2

  } else if(asymmetry == "V2"){
    angle <- asymmetryV2(x, y, data$vx.deg[index], data$vy.deg[index], basin)
    wind <- wind + cos(angle)* data$storm.speed[index]
  }

  return(wind)

}





#' Stack a computed layer in a raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#' @param is_basin logical. Whether loi represents the whole basin or not
#' @param extent terra::extent. Extent to use, to crop the layer to the correct extent
#'
#' @return list of SpatRaster
stackRaster <- function(stack, raster_template, raster_wind, is_basin, extent){

  ras <- raster_template
  if(is_basin)
    ras <- terra::crop(ras, extent)

  ras <- terra::merge(raster_wind, ras)
  ras <- terra::crop(ras, extent)

  return(c(stack, ras))
}





#' Stack a computed PDI layer in a PDI raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#' @param is_basin logical. Whether loi represents the whole basin or not
#' @param extent terra::extent. Extent to use, to crop the layer to the correct extent
#'
#' @return list of SpatRaster
stackRasterPDI <- function(stack, raster_template, raster_wind, is_basin, extent){

  raster.cd <- raster_wind
  terra::values(raster.cd) <- rasterizeCd(terra::values(raster_wind))

  rho <- 0.001
  #Raising to power 3
  raster_wind <- raster_wind ** 3
  #Applying both rho and surface drag coefficient
  raster_wind <- raster_wind * rho * raster.cd

  return(stackRaster(stack, raster_template, raster_wind, is_basin, extent))

}





#' Stack a computed Exposure layer in a Exposure raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#' @param is_basin logical. Whether loi represents the whole basin or not
#' @param extent terra::extent. Extent to use, to crop the layer to the correct extent
#'
#' @return list of SpatRaster
stackRasterExposure <- function(stack, raster_template, raster_wind, is_basin, extent){

  for(c in 2:6){
    raster_c_model <- raster_wind
    terra::values(raster_c_model) <- NA
    ind <- which(terra::values(raster_wind) >= sshs[c] &
                  terra::values(raster_wind) < sshs[c+1])
    raster_c_model[ind] <- 1
    stack <- stackRaster(stack, raster_template, raster_c_model, is_basin, extent)
  }

  return(stack)
}





#' Select the stack function to use depending on the product
#'
#' @noRd
#' @param product character. Product input from stormBehaviour
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#' @param is_basin logical. Whether loi represents the whole basin or not
#' @param extent terra::extent. Extent to use, to crop the layer to the correct extent
#'
#' @return list of SpatRaster
stackProduct <- function(product, stack, raster_template, raster_wind, is_basin, extent){

  if (product == "MSW") {
    stack <- stackRaster(stack, raster_template, raster_wind, is_basin, extent)

  }else if (product == "PDI"){
    stack <- stackRasterPDI(stack, raster_template, raster_wind, is_basin, extent)

  }else if (product == "Exposure"){
    stack <- stackRasterExposure(stack, raster_template, raster_wind, is_basin, extent)
  }

  return(stack)
}





#' Compute MSW raster
#'
#' @noRd
#' @param final_stack list of SpatRaster. Where to add the computed MSW raster
#' @param stack SpatRaster stack. All the wind speed rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#' in final_stack
#'
#' @return list of SpatRaster
rasterizeMSW <- function(final_stack, stack, name){

  msw <- max(stack, na.rm = T)
  #Applying focal function twice to smooth results
  msw <- terra::focal(msw, w = matrix(1, 3, 3), max, na.rm = T, pad = T)
  msw <- terra::focal(msw, w = matrix(1, 3, 3), mean, na.rm = T, pad = T)
  names(msw) <- paste0(name, "_MSW")

  return(c(final_stack, msw))
}





#' Compute PDI raster
#'
#' @noRd
#' @param final_stack list of SpatRaster. Where to add the computed MSW raster
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param stack SpatRaster stack. All the PDI rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#' in final_stack
#'
#' @return list of SpatRaster
rasterizePDI <- function(final_stack, stack, time_res, name){

  #Integrating over the whole track
  pdi <- sum(stack, na.rm = T) * time_res
  #Applying focal function to smooth results
  pdi <- terra::focal(pdi, w = matrix(1, 3, 3), sum, na.rm = T, pad = T)
  names(pdi) <- paste0(name, "_PDI")

  return(c(final_stack, pdi))
}





#' Compute Exposure raster
#'
#' @noRd
#' @param final_stack list of SpatRaster. Where to add the computed MSW raster
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param stack SpatRaster stack. All the Exposure rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#' in final_stack
#'
#' @return list of SpatRaster
rasterizeExposure <- function(final_stack, stack, time_res, name){

  #For each category in SSHS
  all.categories <- c()
  for (i in c(1, 2, 3, 4, 0)) {
    ind <- which(seq(1, terra::nlyr(stack)) %% 5 == i)
    #Integrating over the whole track
    exposure <- sum(terra::subset(stack, ind), na.rm = T) * time_res
    #Applying focal function to smooth results
    exposure <- terra::focal(exposure, w = matrix(1, 3, 3), sum, na.rm = T, pad = T)
    if (i == 0)
      i <- 5

    names(exposure) <- paste0(name, "_Exposure", i)
    final_stack <- c(final_stack, exposure)
    all.categories <- c(all.categories, exposure)
  }

  #Adding all categories
  all.categories <- terra::rast(all.categories)
  all.categories <- sum(all.categories, na.rm = T)
  names(all.categories) <- paste0(name, "_ExposureAll")

  return(c(final_stack, all.categories))
}





#' Select the rasterizeProduct function to use depending on the product
#'
#' @noRd
#' @param product character. Product input from stormBehaviour
#' @param format format input from stormBehaviour
#' @param final_stack list of SpatRaster. Where to add the computed MSW raster
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param stack SpatRaster stack. All the Exposure rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#' in final_stack
#' @param indices numeric vector. Indices of observations. Only used to give proper
#' layer names if format == "profiles"
#'
#' @return list of SpatRaster
rasterizeProduct <- function(product, format, final_stack, stack, time_res, name, indices){

  if (product == "MSW") {
    if(format == "profiles"){
      names(stack) <- paste0(name, "_profile", indices[1:length(indices)-1])
      final_stack <- c(final_stack, stack)

    }else{
      #Computing MSW analytic raster
      final_stack <- rasterizeMSW(final_stack, stack, name)
    }

  } else if (product == "PDI") {
    #Computing PDI analytic raster
    final_stack <- rasterizePDI(final_stack, stack, time_res, name)

  } else if (product == "Exposure") {
    #Computing Exposure analytic raster
    final_stack <- rasterizeExposure(final_stack, stack, time_res, name)

  }

  return(final_stack)

}





#' rasterizePDI counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#'
#' @return numeric. PDI computed using the wind speed values in wind
computePDI <- function(wind, time_res){
  #Computing surface drag coefficient
  cd <- rasterizeCd(wind)

  rho <- 0.001
  #Raising to power 3
  pdi <- wind ** 3
  #Applying both rho and surface drag coefficient
  pdi <- wind * rho * cd
  #Integrating over the whole track
  pdi <- sum(pdi, na.rm <- T) * time_res

  return(pdi)
}





#' rasterizeExposure counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#'
#' @return numeric vector of length 5 (for each category).
#'  Exposure computed using the wind speed values in wind
computeExposure <- function(wind, time_res){

  exposure <- c()

  for(c in 2:6){
    ind <- which(wind >= sshs[c] & wind < sshs[c+1])
    expo <- rep(0,length(wind))
    expo[ind] <- 1
    expo <- sum(expo, na.rm = T) * time_res
    exposure  <- c(exposure, expo)
  }

  return(exposure)
}





#' rasterizeProduct counterpart function for non raster data
#'
#' @noRd
#' @param product character. Product input from Unknow
#' @param wind numeric vector. Wind speed values
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param result numeric array. Similar as final_stack, i.e where to add the
#' computed product
#'
#' @return numeric array of dimension:
#' \itemize{
#'   \item number of observations: number of points. If product == "MSW"
#'   \item 1 : number of points. If product == "PDI"
#'   \item 5 : number of points. If product == "Exposure"
#' }
computeProduct <- function(product, wind, time_res, result){

  if(product == "TS"){
    prod <- wind
  }else if (product == "PDI") {
    prod <- computePDI(wind, time_res)

  }else if (product == "Exposure") {
    prod <- computeExposure(wind, time_res)

  }

  return(cbind(result, prod))
}





#' Whether or not to mask final computed products
#'
#' @noRd
#' @param final_stack SpatRaster stack. Where all final computed products
#' are gathered
#' @param focus_loi logical. focus_loi input from stormBehaviour
#' @param loi sf object. loi used as template to rasterize the mask
#' @param template SpatRaster. raster.template generated with makeTemplateRaster
#' function, used as a template to rasterize the mask
#'
#' @return final_stack masked or not
maskProduct <- function(final_stack, focus_loi, loi, template){

  if (focus_loi) {
    #Masking the stack to fit loi
    v <- terra::vect(loi)
    m <- terra::rasterize(v, template)
    return(terra::mask(final_stack, m))
  }else{
    return(final_stack)
  }
}





#' Arrange result before the end of Unknow
#'
#' @noRd
#' @param final_result list of data.frame. Where to add the computed product
#' @param result result output from computeProduct function
#' @param product character. Product input from Unknow
#' @param points points input from Unknow
#' @param indices numeric vector. Indices of observations
#' @param name character. Name of the storm
#'
#' @return final_result
finalizeResult <- function(final_result, result, product, points, indices, name){

  if(product == "TS"){
    df <- data.frame(result)
  }else if(product == "PDI"){
    df <- data.frame(result, row.names = "PDI")
  }else{
    df <- data.frame(result ,row.names = c("Cat.1", "Cat.2", "Cat.3", "Cat.4", "Cat.5"))
  }

  colnames(df) <- paste0("(",points$lon,",",points$lat,")")


  dfn <- list(df)
  names(dfn) <- name
  final_result <- append(final_result, dfn)

  return(final_result)
}



onestep <- function(index, raster_template, buffer, data, method, asymmetry, format, product, basin, is_basin, extent){

    #Making template to compute wind profiles
    raster.template.model <- makeTemplateModel(raster_template, buffer, data, index)
    raster.wind <- raster.template.model

    #Computing coordinates to the eye of the storm for x and y axes
    x <- (terra::crds(raster.wind, na.rm <- FALSE)[, 1] - data$lon[index])
    y <- (terra::crds(raster.wind, na.rm <- FALSE)[, 2] - data$lat[index])

    #Computing distances to the eye of the storm in m
    dist.m <- terra::distance(x = terra::crds(raster.wind, na.rm = FALSE)[, ],
                             y = cbind(data$lon[index], data$lat[index]),
                             lonlat = T)

    #Computing wind profile
    terra::values(raster.wind) <- computeWindProfile(data, index, dist.m, method, asymmetry, basin, x, y)

    if(product == "MSW"){
      ras <- raster_template
      if(is_basin)
        ras <- terra::crop(ras, extent)

      ras <- terra::merge(raster.wind, ras)
      ras <- terra::crop(ras, extent)

      return(ras)

    }else if(product == "PDI"){
      raster.cd <- raster.wind
      terra::values(raster.cd) <- rasterizeCd(terra::values(raster.wind))

      rho <- 0.001
      #Raising to power 3
      raster.wind <- raster.wind ** 3
      #Applying both rho and surface drag coefficient
      raster.wind <- raster.wind * rho * raster.cd

      ras <- raster_template
      if(is_basin)
        ras <- terra::crop(ras, extent)

      ras <- terra::merge(raster.wind, ras)
      ras <- terra::crop(ras, extent)

      return(ras)

    }else if (product == "Exposure"){
      stack <- c()
      for(c in 2:6){
        raster.c.model <- raster.wind
        terra::values(raster.c.model) <- NA
        ind <- which(terra::values(raster.wind) >= sshs[c] &
                      terra::values(raster.wind) < sshs[c+1])
        raster.c.model[ind] <- 1

        ras <- raster_template
        if(is_basin)
          ras <- terra::crop(ras, extent)

        ras <- terra::merge(raster.wind, ras)
        ras <- terra::crop(ras, extent)
        stack <- c(stack, ras)
      }

      return(stack)
    }

}



#' Compute regimes of wind speed and other products for given storms
#'
#' This function computes/rasterizes analytic products for each storm of a `Storms` object
#' among Maximum Sustained Wind, Power Dissipation Index and Category exposure.
#' It can also rasterize the 2D wind speed structures or produce time series of wind speed
#' for every observations
#'
#' @param sts Storms object
#' @param product character. Product to compute that is either
#'   \itemize{
#'     \item "MSW", Maximum Sustained Wind
#'     \item "PDI", Power Dissipation Index
#'     \item "Exposure", hours spent for each and all categories together
#'   }
#'   Default value is set to "MSW"
#' @param method character. Cyclonic model used to compute product, that is either
#'   \itemize{
#'   \item "Willoughby"
#'   \item "Holland"
#'   }
#'  Default value is set to "Willoughby"
#' @param asymmetry character. Indicates which version of asymmetry to use in
#'   the computations, that is either
#'   \itemize{
#'   \item "None", no asymmetry
#'   \item "V1", first version
#'   \item "V2, second version
#'   }
#'  Default value is set to "None".
#' @param empirical_rmw logical. Whether to compute the Radius of Maximum Wind
#'   empirically, according to getRmw function or using the Radius of Maximum Wind
#'    from the observations. Default value is set to FALSE
#' @param format either a character among "analytic" and "profiles":
#'   \itemize{
#'     \item  If "analytic", analytic rasters (integration in space and
#'      time over the track) are returned.
#'     \item  If "profiles", product input is
#'   ignored and set to "MSW" and 2D wind speed structures for each observation
#'   are returned.
#'   }
#' @param space_res numeric. Space resolution (km) for the raster(s) to compute.
#'   Default value is set to 10
#' @param time_res numeric. Time discretization (hours) used to compute the
#'   analytic raster(s). Allowed values are 1 (60min), 0.75 (45min), 0.5
#'   (30min), and 0.25 (15min). Default value is set to 1
#' @param verbose logical. Whether or not the function must be verbose and
#'   display a text progress bar. Default value is set to FALSE
#' @param focus_loi logical. Whether or not the computations must only be
#'   overtaken within the spatial.loi.buffer from sts object. Default value
#'   is set to TRUE, otherwise, computations are extended over the whole track
#'   of the storms
#'
#' @returns Depending on format input:
#' \itemize{
#'   \item  If "analytic", analytic rasters (integration in space and time over the track)
#'    are returned within a raster stack. Each layer is named after the storm and the product
#'    computed as follow: stormName_product
#'   \item If "profiles", product input is ignored and set to "MSW" and
#'   2D wind speed structures for each observation are returned within a raster
#'   stack. Each layer is named after the storm and the index of observation
#'   computed as follow: stormName_profileIndex
#'  }
#'
#'
#' @examples
#' #Compute MSW for PAM 2015 in Vanuatu using default settings
#' msw_pam <- stormBehaviour(pam)
#'
#' #Compute PDI for ERICA and NIRAN in New Caledonia using Holland model without asymmetry
#' pdi_nc <- stormBehaviour(sts_nc, time_res = 0.5, method = "Holland",
#'                         product = "PDI", verbose = TRUE)
#'
#' #Compute Exposure for PAM 2015 in Vanuatu using default settings
#' exp_pam <- stormBehaviour(pam, product = "Exposure")
#'
#' #Compute profiles wind speed for ERICA and NIRAN in New Caledonia using default settings
#' prof_nc <- stormBehaviour(sts_nc, format = "profiles", verbose = TRUE)
#'
#' @export
stormBehaviour <- function(sts, product = "MSW", method = "Willoughby", asymmetry = "V2",
                          empirical_rmw = FALSE, format = "analytic", space_res = 10,
                          time_res = 1, verbose = FALSE, focus_loi = TRUE){

  checkInputsSb(sts, product, method, asymmetry, empirical_rmw, format,
                space_res, time_res, verbose, focus_loi)


  if(format == "profiles")
    product <- "MSW"

  #Make raster template
  raster.template <- makeTemplateRaster(sts@spatial.loi.buffer, space_res)
  #Getting new extent
  ext <- terra::ext(raster.template)
  #Buffer size in degree
  buffer <- terra::res(raster.template)[1] * sts@buffer / space_res
  #Initializing final raster stack
  final.stack <- c()


  if(verbose)
    s <- 1 #Initializing count of storms

  for (st in sts@data) {

    #Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, format, focus_loi)



    #Interpolated time step dt, default value dt <- 4 --> 1h
    dt <- 1 + (1 / time_res * 3) # + 1 for the limit values

    #Getting data associated with storm st
    if(format == "profiles"){
      dataTC <- getData(st, ind, asymmetry, empirical_rmw, method)
    }else{
      dataTC <- getDataInterpolate(st, ind, dt, asymmetry, empirical_rmw, method)
    }


    #Reduce extent of raster if loi represents the whole basin
    if(sts@loi.basin)
      ext <- terra::ext(min(dataTC$lon) - buffer, max(dataTC$lon) + buffer,
                     min(dataTC$lat) - buffer, max(dataTC$lat) + buffer)

    nb.step <- dim(dataTC)[1] - 1

    if (verbose) {
      step <- 1
      cat("Computing", format, product, "rasters using", method,
          "model (time_res:", time_res, "h, space_ras:", space_res,
          "km, asymmetry:", asymmetry, ", empirical_rmw:", empirical_rmw,
          ") for", st@name, "(", s, "/", sts@nb.storms, ")\n")
      pb <- utils::txtProgressBar(min = step, max = nb.step, style = 3)
    }

    aux.stack <- c()

    for (j in 1:nb.step) {

      #Making template to compute wind profiles
      raster.template.model <- makeTemplateModel(raster.template, buffer, dataTC, j)
      raster.wind <- raster.template.model

      #Computing coordinates to the eye of the storm for x and y axes
      x <- (terra::crds(raster.wind, na.rm = FALSE)[, 1] - dataTC$lon[j])
      y <- (terra::crds(raster.wind, na.rm = FALSE)[, 2] - dataTC$lat[j])

      #Computing distances to the eye of the storm in m
      dist.m <- terra::distance(x = terra::crds(raster.wind, na.rm = FALSE)[, ],
                                y = cbind(dataTC$lon[j], dataTC$lat[j]),
                                lonlat = T)

      #Computing wind profile
      terra::values(raster.wind) <- computeWindProfile(dataTC, j, dist.m, method,
                                                       asymmetry, sts@basin, x, y)

      #Stacking product
      aux.stack <- stackProduct(product, aux.stack, raster.template,
                                raster.wind, sts@loi.basin, ext)


      if (verbose){
        utils::setTxtProgressBar(pb, step)
        step <- step + 1
      }

    }


    if (verbose)
      close(pb)

      #Rasterize final product
      aux.stack <- terra::rast(aux.stack)
      final.stack <- rasterizeProduct(product, format, final.stack, aux.stack,
                                     time_res, st@name, ind)

    if(verbose)
      s <- s + 1
  }


  final.stack <- terra::rast(final.stack)
  final.stack <- maskProduct(final.stack, focus_loi,
                             sts@spatial.loi.buffer, raster.template)

  return(final.stack)
}


#' Compute product on given points coordinates for given storms
#'
#' This function computes products for each storm of a `Storms` object
#' among TS (time series of wind speed), Power Dissipation Index and Category exposure for each
#' given points provided in points input
#'
#' @param sts Storms object
#' @param points data.frame. Contains longitude/latitude coordinates within column
#'   names "lon" and "lat".
#' @param product character. Product to compute that is either
#'   \itemize{
#'     \item "MSW", Maximum Sustained Wind
#'     \item "PDI", Power Dissipation Index
#'     \item "Exposure", hours spent for each and all categories together
#'   }
#'   Default value is set to "MSW"
#' @param method character. Cyclonic model used to compute product, that is either
#'   \itemize{
#'     \item "Willoughby"
#'     \item "Holland"
#'   }
#'  Default value is set to "Willoughby"
#' @param asymmetry character. Indicates which version of asymmetry to use in
#'   the computations, that is either
#'   \itemize{
#'     \item "None", no asymmetry
#'     \item "V1", first version
#'     \item "V2, second version
#'   }
#'  Default value is set to "None".
#' @param empirical_rmw logical. Whether to compute the Radius of Maximum Wind
#'   empirically, according to getRmw function or using the Radius of Maximum Wind
#'    from the observations. Default value is set to FALSE
#' @param time_res numeric. Time discretization (hours) used to compute the
#'   analytic raster(s). Allowed values are 1 (60min), 0.75 (45min), 0.5
#'   (30min), and 0.25 (15min). Default value is set to 1
#' @param focus_loi logical. Whether or not the computations must only be
#'   overtaken within the spatial.loi.buffer from sts object. Default value
#'   is set to TRUE, otherwise, computations are extended over the whole track
#'   of the storms
#'
#' @returns Computed product for each points are returned
#'   through a named list. Each slot is named after the storm and has dimensions:
#'    \itemize{
#'      \item (number of observations:number of point coordinates), if product == "MSW".
#'       Correspond in fact of time series of wind speed on every observations (real and interpolated)
#'      \item (1:number of point coordinates), if product == "PDI"
#'      \item (6:number of point coordinates), if product == "Exposure"
#'    }
#'
#' @examples
#'
#' df <- data.frame(lon = c(166.5, 163), lat = c(-22, -19))
#' #Compute time series of wind speed for ERICA and NIRAN on points provided in df using default settings
#' ts_nc <- Unknow(sts_nc, points = df)
#' #Compute PDI for ERICA and NIRAN on points provided in df using default settings
#' pdiPt_nc <- Unknow(sts_nc, points = df, product = "PDI)
#' #Compute Exposure for ERICA and NIRAN on points provided in df using default settings
#' expPt_nc <- Unknow(sts_nc, points = df, product = "Exposure")
#'
#' @export
Unknow <- function(sts, points, product = "TS", method = "Willoughby", asymmetry = "V2",
                   empirical_rmw = FALSE, time_res = 1, focus_loi = TRUE){

  checkInputsUnknow(sts, points, product, method, asymmetry, empirical_rmw, time_res, focus_loi)



  #Initializing final result
  final.result <- list()

  for (st in sts@data) {

    #Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, "none", focus_loi)

    #Interpolated time step dt, default value dt <- 4 --> 1h
    dt <- 1 + (1 / time_res * 3) # + 1 for the limit values

    #Getting data associated with storm st
    dataTC <- getDataInterpolate(st, ind, dt, asymmetry, empirical_rmw, method)

    #Computing distances from the eye of storm for every observations x, and
    #every points y
    dist.m <- terra::distance(
      x <- cbind(dataTC$lon,dataTC$lat),
      y <- cbind(points$lon, points$lat),
      lonlat <- T
    )

    res <- c()
    #For each point
    for(i in 1:dim(points)[1]){

      #Computing coordinates between eye of storm and point P
      x <- points$lon[i] - dataTC$lon
      y <- points$lat[i] - dataTC$lat

      #Computing wind profiles
      dist2p <- dist.m[,i]
      vr <- computeWindProfile(dataTC, i, dist2p, method, asymmetry, sts@basin, x, y)

      #Computing product
      res <- computeProduct(product, vr, time_res, res)

    }

    final.result <- finalizeResult(final.result, res, product, points, ind, st@name)

  }

  return(final.result)
}








