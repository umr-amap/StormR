




#' Compute the Radius of Maximum Wind
#'
#' It is an empirical formula extracted from Willoughby et al. 2006 model
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the storm
#'
#' @returns Radius of Maximum Wind (km)
getRmw <- function(msw, lat) {
  return(round(46.4 * exp(-0.0155 * msw + 0.0169 * abs(lat))))
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
    x1 <- 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat)
    x2 <- 25
    a <- 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat)
    vr <- msw * ((1 - a) * exp(-abs((r - rmw) / x1)) + a * exp(-abs(r - rmw) / x2))
  } else{
    n <- 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat)
    vr <- msw * abs((r / rmw) ^ n)
  }

  return(round(vr,3))
}

#Vectorize version of the above model
Willoughby <- Vectorize(Willoughby_profile, vectorize.args = "r")





#' Holland 1980 model
#'
#' Compute radial wind speed according to Holland  1980 model
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

  return(round(vr,3))

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
    res <- NA

  }else if(vr <= 18){
    res <- 0

  }else if(vr > 18 & vr <= 31.5){
    res <- (0.8 + 0.06 * vr) * 0.001

  }else if(vr > 31.5){
    res <- (0.55 + 2.97 * vr/31.5 - 1.49 * (vr/ 31.5) ** 2) * 0.001
  }

  return(round(res, 3))
}

#Vectorize version of the above function
rasterizeCd <- Vectorize(compute_Cd, vectorize.args = "vr")





#' Check inputs for spatialBehaviour function
#'
#' @noRd
#' @param sts StormsList object
#' @param product character
#' @param wind_threshold numeric
#' @param method character
#' @param asymmetry character
#' @param empirical_rmw logical
#' @param space_res character
#' @param time_res numeric
#' @param verbose numeric
#' @return NULL
checkInputsSb <- function(sts, product, wind_threshold, method, asymmetry,
                         empirical_rmw, space_res, time_res, verbose){

  #Checking sts input
  stopifnot("no data found" = !missing(sts))

  #Checking product input
  stopifnot("Invalid product" = product %in% c("MSW", "PDI", "Exposure", "Profiles"))

  #Checking wind_threshold input
  if("Exposure" %in% product){
      stopifnot("wind_threshold must be numeric" = identical(class(wind_threshold), "numeric"))
      stopifnot("invalid value(s) for wind_threshold input (must be > 0)" = wind_threshold > 0)
  }

  #Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland"))
  stopifnot("Only one method must be chosen" = length(method) == 1)
  if(method == "Holland"){
    stopifnot("Cannot perform Holland method (missing pressure input)" = "pres" %in% colnames(sts@data[[1]]@obs.all))
    stopifnot("Cannot perform Holland method (missing poci input)" = "poci" %in% colnames(sts@data[[1]]@obs.all))
  }

  #Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "Boose01"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  #Checking empirical_rmw input
  stopifnot("empirical_rmw must be logical" = identical(class(empirical_rmw), "logical"))

  #Checking space_res input
  stopifnot("space_res must be character" = identical(class(space_res), "character"))
  stopifnot("space_res must be length 1" = length(space_res) == 1)
  stopifnot("invalid space_res: must be either 30s, 2.5min, 5min or 10min" = space_res %in% c("30sec", "2.5min", "5min", "10min"))

  #Checking time_res input
  stopifnot("time_res must be numeric" = identical(class(time_res), "numeric"))
  stopifnot("time_res must be length 1" = length(time_res) == 1)
  stopifnot("invalid time_res: must be either 1, 0.75, 0.5 or 0.25" = time_res %in% c(1, 0.75, 0.5, 0.25))

  #Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1, 2 or 3" = verbose %in% c(0, 1, 2, 3))

}





#' Generate raster template for the computations
#'
#' @noRd
#' @param buffer sf object. LOI + buffer extention
#' @param res numeric. Space resolution min for the template
#'
#' @return a SpatRaster
makeTemplateRaster <- function(buffer, res){

  #Deriving the raster template
  ext <- terra::ext(sf::st_bbox(buffer)$xmin,
                    sf::st_bbox(buffer)$xmax,
                    sf::st_bbox(buffer)$ymin,
                    sf::st_bbox(buffer)$ymax)



  template <- terra::rast(
    xmin = ext$xmin,
    xmax = ext$xmax,
    ymin = ext$ymin,
    ymax = ext$ymax,
    resolution = res,
    vals = NA,
  )
  terra::origin(template) <- c(0,0)

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
#' @param product character. product input from spatialBehaviour
#'
#' @return numeric vector gathering the indices of observation to use to perform
#' the further computations
getIndices <- function(st, offset, product){

  #Use observations within the loi for the computations
  ind <- seq(st@obs[1], st@obs[length(st@obs)], 1)

  if("MSW" %in% product | "PDI" %in% product | "Exposure" %in% product){
    #Handling indices and offset (outside of loi at entry and exit)
    for(o in 1:offset){
      ind <- c(st@obs[1] - o, ind)
      ind <- c(ind, st@obs[length(st@obs)] + o)
    }

    #Remove negative values and values beyond st@numobs.all
    ind <- ind[ind > 0 & ind <= st@numobs.all]

  }

  return(ind)
}





#' Get data associated with one storm to perform further computations
#'
#' @noRd
#' @param st Storm object
#' @param indices numeric vector extracted from getIndices
#' @param dt numeric. time step
#' @param time_diff numeric. Time diff in database
#' @param empirical_rmw logical. Whether to use rmw from the data or to compute them
#' according to getRmw function
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
#'    \item storm.speed: numeric. Velocity of the storm (m/s)
#'    \item vx.deg: numeric. Velocity of the speed in the x direction (deg/h)
#'    \item vy.deg: numeric Velocity of the speed in the y direction (deg/h)
#'  }
getDataInterpolate <- function(st, indices, dt, time_diff, empirical_rmw, method){

  len.indices <- length(indices)
  len.data <- dt * (len.indices - 1) - (len.indices-2)
  indices.obs <- seq(1, len.data, dt-1)

  data <- data.frame(lon = rep(NA, len.data),
                    lat = rep(NA, len.data),
                    storm.speed = rep(NA,len.data),
                    vx.deg = rep(NA,len.data),
                    vy.deg = rep(NA,len.data),
                    msw = rep(NA,len.data),
                    rmw = rep(NA,len.data),
                    indices = rep(NA,len.data),
                    isoTimes = rep(NA,len.data))

  #Filling indices and isoTimes
  ind = c()
  isoT = c()
  timeRes = 1/((dt-1)/time_diff) * 60

  for(i in indices[1:len.indices-1]){
    lab = as.character(i)
    t = st@obs.all$iso.time[i]
    for(j in 1:(dt-2)){
      lab = c(lab, paste0(as.character(i),".",as.character(j)))
      t = c(t, as.character(as.POSIXct(st@obs.all$iso.time[i]) + j * 60 * timeRes ))
    }
    ind = c(ind, lab)
    isoT = c(isoT, t)
  }
  ind = c(ind, as.character(indices[len.indices]))
  isoT = c(isoT, st@obs.all$iso.time[indices[len.indices]])

  data$indices <- ind
  data$isoTimes <- isoT


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


  data$msw[indices.obs] <- st@obs.all$msw[indices]


  if(empirical_rmw){
    data$rmw[indices.obs] <- getRmw(data$msw[indices.obs], lat)
  }else{

    if(!("rmw" %in% colnames(st@obs.all))){
      warning("Missing rmw data to perform model. empirical_rmw set to TRUE")
      data$rmw[indices.obs] <- getRmw(data$msw[indices.obs], lat)
    }else if(all(is.na(st@obs.all$rmw[indices]))){
      warning("Missing rmw data to perform model. empirical_rmw set to TRUE")
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
                         resolution = terra::res(raster_template),
                         vals = NA)
  terra::origin(template) <- c(0,0)

  return(template)

}





#' Compute wind direction
#' @noRd
#' @param x numeric vector. Distance(s) to the eye of the storm in the x direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y direction (deg)
#' @param northenH logical. Whether it is northen hemisphere or not
#' @param I numeric array. 1 if coordinates intersect with land, 0 otherwise
#'
#' @return wind directions (rad) at each (x,y) position
computeDirection <- function(x, y, I, northenH){

  azimuth <- -(atan2(y, x) - pi/2)

  azimuth[azimuth < 0] <-  azimuth[azimuth < 0] + 2*pi

  if(northenH){
    direction <- azimuth *180/pi - 90
    direction[I == 1] <- direction[I == 1] - 40
    direction[I == 0] <- direction[I == 0] - 20
  }else{
    direction <- azimuth *180/pi + 90
    direction[I == 1] <- direction[I == 1] + 40
    direction[I == 0] <- direction[I == 0] + 20
  }

  direction[direction < 0] = direction[direction < 0] + 360
  direction[direction > 360] = direction[direction > 360] - 360

  return(direction)
}





#' Compute asymmetry
#'
#' @noRd
#' @param asymmetry character. Asymmetry input form stormBehaviour
#' @param wind numeric vector. Wind values
#' @param x numeric vector. Distance(s) to the eye of the storm in the x direction (deg)
#' @param y numeric vector. Distance(s) to the eye of the storm in the y direction (deg)
#' @param vx numeric. Velocity of the storm in the x direction (deg/h)
#' @param vy numeric. Velociy of the storm in the y direction (deg/h)
#' @param vh numeric. Velociy of the storm (m/s)
#' @param northenH logical. Whether it is northen hemisphere or not
#'
#' @return numeric vectors. Wind speed values (m/s) and wind direction (rad) at each (x,y) position
computeAsymmetry <- function(asymmetry, wind, x, y, vx, vy, vh, northenH){

  if(asymmetry == "None"){
    wind <- wind
  }else if(asymmetry == "Boose01"){

    if(northenH){
      #Northern Hemisphere, t is clockwise
      angle <- atan2(vy,vx) - atan2(y,x)
    }else{
      #Southern Hemisphere, t is counterclockwise
      angle <- atan2(y,x) - atan2(vy,vx)
    }

    wind <- wind - (1 - sin(angle)) * vh/2

  }

  return(round(wind,3))
}





#' Compute wind profile according to the selected method and asymmetry
#'
#' @noRd
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use for
#' the computations
#' @param dist_m numeric array. Distance in meter from the eye of the storm for
#' each coordinate of the raster_template_model
#' @param method character. method input form spatialBehaviour
#' @param asymmetry character. asymmetry input from stormBehviour
#' @param x numeric array. Distance in degree from the eye of storm in the x direction
#' @param y numeric array. Distance in degree from the eye of storm in the y direction
#' @param buffer numeric. Buffer size (in degree) for the storm
#'
#' @return  numeric vector. Wind speed values (m/s)
computeWindProfile <- function(data, index, dist_m, method, asymmetry, x, y, buffer){


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


  if(data$lat[index] > 0){
    northenH = TRUE
  }else{
    northenH = FALSE
  }

  #Adding asymmetry
  wind <- computeAsymmetry(asymmetry, wind, x, y,
                           data$vx.deg[index], data$vy.deg[index],
                           data$storm.speed[index], northenH)

  #Remove cells outside of buffer
  dist <- sqrt(x*x + y*y)
  wind[dist > buffer] <- NA

  return(wind)

}





#' Compute wind direction according to the selected method and asymmetry
#'
#' @noRd
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use for
#' the computations
#' @param x numeric array. Distance in degree from the eye of storm in the x direction
#' @param y numeric array. Distance in degree from the eye of storm in the y direction
#' @param I numeric array. 1 if coordinates intersect with land, 0 otherwise
#' @param buffer numeric. Buffer size (in degree) for the storm
#'
#' @return  numeric vector. Wind directions (degree)
computeWindDirection <- function(data, index, x, y, I, buffer){


  if(data$lat[index] > 0){
    northenH = TRUE
  }else{
    northenH = FALSE
  }


  #Computing wind direction
  direction <- computeDirection(x, y, I, northenH)

  #Remove cells outside of buffer
  dist <- sqrt(x*x + y*y)
  direction[dist > buffer] <- NA

  return(direction)

}











#' Stack a computed layer in a raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#'
#' @return list of SpatRaster
stackRaster <- function(stack, raster_template, raster_wind){

  ras <- raster_template
  extent = terra::ext(ras)
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
#'
#' @return list of SpatRaster
stackRasterPDI <- function(stack, raster_template, raster_wind){

  raster.cd <- raster_wind
  terra::values(raster.cd) <- rasterizeCd(terra::values(raster_wind))

  rho <- 0.001
  #Raising to power 3
  raster_wind <- raster_wind ** 3
  #Applying both rho and surface drag coefficient
  raster_wind <- raster_wind * rho * raster.cd

  return(stackRaster(stack, raster_template, raster_wind))

}





#' Stack a computed Exposure layer in a Exposure raster stack
#'
#' @noRd
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#' @param threshold numeric. Wind threshold
#'
#' @return list of SpatRaster
stackRasterExposure <- function(stack, raster_template, raster_wind, threshold){

  for(t in threshold){
    raster_c_model <- raster_wind
    terra::values(raster_c_model) <- NA
    ind <- which(terra::values(raster_wind) >= t)
    raster_c_model[ind] <- 1
    stack <- stackRaster(stack, raster_template, raster_c_model)
  }

  return(stack)
}





#' Select the stack function to use depending on the product
#'
#' @noRd
#' @param product character. Product input from spatialBehaviour
#' @param stack list of SpatRaster. where to stack the layer
#' @param raster_template SpatRaster. Raster template generated with makeTemplateRaster function
#' @param raster_wind SpatRaster. Layer to add to the stack
#' @param threshold numeric vector. Wind threshold
#'
#' @return list of SpatRaster
stackProduct <- function(product, stack, raster_template, raster_wind, threshold){

  if (product == "MSW") {
    stack <- stackRaster(stack, raster_template, raster_wind)

  }else if (product == "PDI"){
    stack <- stackRasterPDI(stack, raster_template, raster_wind)

  }else if (product == "Exposure"){
    stack <- stackRasterExposure(stack, raster_template, raster_wind, threshold)
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
#' @param space_res character. space_res input from spatialBehaviour
#'
#' @return list of SpatRaster
rasterizeMSW <- function(final_stack, stack, space_res, name){

  nbg = switch(space_res,"30sec" = 25, "2.5min" = 7, "5min" = 5, "10min" = 3)

  msw <- max(stack, na.rm = T)
  #Applying focal function twice to smooth results
  msw <- terra::focal(msw, w = matrix(1, nbg, nbg), max, na.rm = T, pad = T)
  msw <- terra::focal(msw, w = matrix(1, nbg, nbg), mean, na.rm = T, pad = T)
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
#' @param space_res character. space_res input from spatialBehaviour
#' @param product characher
#' @param threshold numeric vector. Wind threshold
#'
#'
#' @return list of SpatRaster
rasterizePDI <- function(final_stack, stack, time_res, space_res, name, product, threshold){

  nbg = switch(space_res,"30sec" = 25, "2.5min" = 7, "5min" = 5, "10min" = 3)

  #Integrating over the whole track
  prod <- sum(stack, na.rm = T) * time_res
  #Applying focal function to smooth results
  prod <- terra::focal(prod, w = matrix(1, nbg, nbg), mean, na.rm = T, pad = T)
  names(prod) <- paste0(name,"_",product)


  return(c(final_stack, prod))
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
#' @param space_res character. space_res input from spatialBehaviour
#' @param product characher
#' @param threshold numeric vector. Wind threshold
#'
#'
#' @return list of SpatRaster
rasterizeExp <- function(final_stack, stack, time_res, space_res, name, product, threshold){

  nbg = switch(space_res,"30sec" = 25, "2.5min" = 7, "5min" = 5, "10min" = 3)


  for(l in 1:length(threshold)){
    ind <- seq(l,terra::nlyr(stack),length(threshold))
    #Integrating over the whole track
    prod <- sum(terra::subset(stack, ind), na.rm = T) * time_res
    #Applying focal function to smooth results
    prod <- terra::focal(prod, w = matrix(1, nbg, nbg), max, na.rm = T, na.rm = T)
    names(prod) <- paste0(name,"_",product,"_",threshold[l])
    final_stack <- c(final_stack, prod)
  }


  return(final_stack)
}





#' Select the rasterizeProduct function to use depending on the product
#'
#' @noRd
#' @param product character. Product input from spatialBehaviour
#' @param final_stack list of SpatRaster. Where to add the computed MSW raster
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param stack SpatRaster stack. All the Exposure rasters used to compute MSW
#' @param name character. Name of the storm. Used to give the correct layer name
#' in final_stack
#' @param space_res character. space_res input from spatialBehaviour
#' @param threshold numeric vector. Wind threshold
#'
#' @return list of SpatRaster
rasterizeProduct <- function(product, final_stack, stack, time_res, space_res, name, threshold){

  if(product == "MSW") {
    #Computing MSW analytic raster
    final_stack <- rasterizeMSW(final_stack, stack, space_res, name)

  }else if (product == "PDI") {
    #Computing PDI analytic raster
    final_stack <- rasterizePDI(final_stack, stack, time_res, space_res, name, "PDI", NULL)

  }else if (product == "Exposure") {
    #Computing Exposure analytic raster
    final_stack <- rasterizeExp(final_stack, stack, time_res, space_res, name,"Exposure", threshold)

  }

  return(final_stack)

}





#' Whether or not to mask final computed products
#'
#' @noRd
#' @param final_stack SpatRaster stack. Where all final computed products
#' are gathered
#' @param loi sf object. loi used as template to rasterize the mask
#' @param template SpatRaster. raster.template generated with makeTemplateRaster
#' function, used as a template to rasterize the mask
#'
#' @return final_stack masked or not
maskProduct <- function(final_stack, loi, template){

  #Masking the stack to fit loi
  v <- terra::vect(loi)
  m <- terra::rasterize(v, template)
  return(terra::mask(final_stack, m))

}





#' Compute indicators of storm behaviour
#'
#' This function computes/rasterizes analytic products for each storm of a
#' StormsList object, including Maximum Sustained Wind, Power Dissipation Index,
#' Category exposure and 2D wind speed structures/direction of wind speed for
#' every observations
#'
#' @param sts StormsList object
#' @param product character. Product to compute among:
#'   \itemize{
#'     \item "MSW": Maximum Sustained Wind
#'     \item "PDI": Power Dissipation Index
#'     \item "Exposure": hour exposition for wind greater than wind_threshold
#'           input
#'     \item "Profiles", 2D wind speed structures of wind speed with wind
#'           direction for each observation
#'   }
#'   Default value is set to "MSW"
#' @param wind_threshold numeric vector. Minimal wind threshold(s) (m/s) to
#' compute Exposure product. Ignored if Exposure is not part of the products to
#' compute. Default value is set to Saffir Simpson Hurricane Scale thresholds
#' @param method character. Cyclonic model used to compute product. Must be
#' either:
#'   \itemize{
#'   \item "Willoughby": model based on fits performed on cyclonic observations
#'   \item "Holland": model based on both basic cyclonic Physics and parameters
#'         fitting according to cyclonic observations
#'   }
#'  Default value is set to "Willoughby" (See Details)
#' @param asymmetry character. Indicates which version of asymmetry to use in
#' the computations. Must be either:
#'   \itemize{
#'   \item "Boose01": formula that substract wind speed to the original wind
#'         field depending on the position to the eye of the storm and its
#'         velocity
#'   \item "None": no asymmetry is added
#'   }
#'  Default value is set to "Boose01" (See Details)
#' @param empirical_rmw logical. Whether to compute the radius of maximum wind
#' empirically or using the radius of maximum wind from the observations.
#' Default value is set to FALSE. If TRUE, a formula extracted from Willoughby
#' et al. 2006 is used to compute rmw
#' @param space_res character. Space resolution for the raster(s) to compute.
#' Either 30sec, 2.5min, 5min or 10min. Default value is set to 2.5min
#' @param time_res numeric. Period of time used to interpolate data.
#' Allowed values are 1 (60min), 0.75 (45min), 0.5 (30min), and 0.25 (15min).
#' Default value is set to 1
#' @param verbose numeric. Whether or not the function should display
#' informations about the process and/or outputs and additional notes.
#' Allowed values are:
#' \itemize{
#' \item 0: Nothing is displayed
#' \item 1: Informations about the process are displayed
#' \item 2: Outputs are also displayed
#' \item 3: Additional notes are also displayed
#' }
#' Default value is set to 2
#' @returns SpatRaster stack which provides the desired product computed,
#' projected in WGS84 and spanning over the extented LOI of the StormsList object.
#' Number of layers depends on the number of storm available in sts input and
#' also product and time_res inputs:
#' \itemize{
#'    \item "MSW" produces one layer per storm. Name of layer is "STORMNAME_MSW"
#'    \item "PDI" produces one layer per storm. Name of layer is "STORMNAME_PDI"
#'    \item "Exposure" produces one layer for each wind values available
#'           in wind_threshold input and for each storm. Name of layers are
#'           "STORMNAME_Exposure_threshold1", "STORMNAME_Exposure_threshold2"...
#'    \item "Profiles" produces two layers for each observations
#'          (real and interpolated) and each storm. Name of layers are
#'          "STORMNAME_Profile_observation", "STORMNAME_WindDirection_observation"
#' }
#'
#' @details Add details on Willoughy/ Holland method Boose asymmetry and getRMW method ...
#' @examples
#' \dontrun{
#' #Compute MSW product for Pam 2015 in Vanuatu using default settings
#' msw.pam <- spatialBehaviour(pam)
#'
#' #Compute PDI product for Erica and Niran in New Caledonia using Holland model without asymmetry
#' pdi.nc <- spatialBehaviour(sts_nc, method = "Holland", product = "PDI", asymmetry = "None")
#'
#' #Compute Exposure for Pam 2015 in Vanuatu using default settings
#' exp.pam <- spatialBehaviour(pam, product = "Exposure")
#'
#' #Compute profiles wind speed for Erica and Niran in New Caledonia using default settings
#' prof.nc <- spatialBehaviour(sts_nc, product = "Profiles")
#' }
#'
#' @export
spatialBehaviour <- function(sts,
                              product = "MSW",
                              wind_threshold = c(18, 33, 42, 49, 58, 70),
                              method = "Willoughby",
                              asymmetry = "Boose01",
                              empirical_rmw = FALSE,
                              space_res = "2.5min",
                              time_res = 1,
                              verbose = 2){

  start_time <- Sys.time()

  checkInputsSb(sts, product, wind_threshold, method, asymmetry,
                empirical_rmw, space_res, time_res, verbose)


  #Make raster template
  raster.template <- makeTemplateRaster(sts@spatial.loi.buffer, resolutions[space_res])
  #Getting new extent
  ext <- terra::ext(raster.template)
  #Buffer size in degree
  buffer <- 2.5
  #Initializing final raster stacks
  final.stack.msw <- c()
  final.stack.pdi <- c()
  final.stack.exp <- c()


  if(verbose > 0){
    s <- 1 #Initializing count of storms
    cat("=== spatialBehaviour processing ... ===\n\n")

    cat("Computation settings:\n")
    cat("  (*) Time interpolation: Every", switch(as.numeric(time_res),"1" = 60, "0.75" = 45, "0.5" = 30, "0.25" = 15),"min\n")
    cat("  (*) Space resolution:",names(resolutions[space_res]),"\n")
    cat("  (*) Method used:", method ,"\n")
    cat("  (*) Product(s) to compute:", product ,"\n")
    cat("  (*) Asymmetry used:", asymmetry ,"\n")
    if(empirical_rmw){
      cat("  (*) rmw computed according to the empirical formula (See Details section)")
    }

    cat("\nStorm(s):\n")
    cat("  (",sts@nb.storms,") ",getNames(sts),"\n\n")

  }

  for (st in sts@data) {

    #Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, product)


    it1 <- st@obs.all$iso.time[1]
    it2 <- st@obs.all$iso.time[2]
    time.diff <- as.numeric(as.POSIXct(it2) - as.POSIXct(it1))
    #Interpolated time step dt, default value dt <- 4 --> 1h
    dt <- 1 + (1 / time_res * time.diff) # + 1 for the limit values

    #Getting data associated with storm st
    dataTC <- getDataInterpolate(st, ind, dt, time.diff, empirical_rmw, method)

    nb.step <- dim(dataTC)[1] - 1

    if (verbose > 0) {
      step <- 1
      cat(st@name," (", s, "/", sts@nb.storms, ")\n")
      pb <- utils::txtProgressBar(min = step, max = nb.step, style = 3)
    }

    aux.stack.msw <- c()
    aux.stack.pdi <- c()
    aux.stack.exp <- c()
    aux.stack.prof <- c()
    aux.stack.direction <- c()



    for (j in 1:nb.step) {

      #Making template to compute wind profiles
      raster.template.model <- makeTemplateModel(raster.template, buffer, dataTC, j)
      raster.wind <- raster.template.model

      #Computing coordinates of raster
      crds <- terra::crds(raster.wind, na.rm = FALSE)

      #Computing distances in degree to the eye of the storm for x and y axes
      x <- crds[, 1] - dataTC$lon[j]
      y <- crds[, 2] - dataTC$lat[j]

      #Computing distances to the eye of the storm in m
      dist.m <- terra::distance(x = crds,
                                y = cbind(dataTC$lon[j], dataTC$lat[j]),
                                lonlat = T)

      #Computing wind profile
      terra::values(raster.wind) <- computeWindProfile(dataTC, j, dist.m, method, asymmetry, x, y, buffer)

      #Computing wind direction
      if("Profiles" %in% product){
        raster.direction <- raster.template.model
        raster.I <- raster.template.model
        #Intersect points coordinates with LOI
        pts <- sf::st_as_sf(as.data.frame(crds), coords = c("x","y"))
        sf::st_crs(pts) <- wgs84
        ind <- which(sf::st_intersects(pts, sts@spatial.loi, sparse <- FALSE) == TRUE)
        terra::values(raster.I) <- 0
        terra::values(raster.I)[ind] <- 1
        terra::values(raster.direction) <- computeWindDirection(dataTC, j, x, y, terra::values(raster.I), buffer)
      }

      #Stacking products
      if("MSW" %in% product)
        aux.stack.msw <- stackProduct("MSW", aux.stack.msw, raster.template, raster.wind, NULL)
      if("PDI" %in% product)
        aux.stack.pdi <- stackProduct("PDI", aux.stack.pdi, raster.template, raster.wind, NULL)
      if("Exposure" %in% product)
        aux.stack.exp <- stackProduct("Exposure", aux.stack.exp, raster.template, raster.wind, wind_threshold)
      if("Profiles" %in% product){
        names(raster.wind) <- paste0(st@name,"_","Profiles","_",dataTC$indices[j])
        names(raster.direction) <- paste0(st@name,"_","WindDirection","_",dataTC$indices[j])
        aux.stack.prof <- stackRaster(aux.stack.prof, raster.template, raster.wind)
        aux.stack.direction <- stackRaster(aux.stack.direction, raster.template, raster.direction)
      }


      if (verbose > 0){
        utils::setTxtProgressBar(pb, step)
        step <- step + 1
      }

    }


    if (verbose > 0)
      close(pb)


    #Rasterize final products
    if("MSW" %in% product){
      aux.stack.msw <- terra::rast(aux.stack.msw)
      final.stack.msw <- rasterizeProduct("MSW", final.stack.msw, aux.stack.msw,
                                          time_res, space_res, st@name, NULL)
    }
    if("PDI" %in% product){
      aux.stack.pdi <- terra::rast(aux.stack.pdi)
      final.stack.pdi <- rasterizeProduct("PDI", final.stack.pdi, aux.stack.pdi,
                                          time_res, space_res, st@name, NULL)
    }
    if("Exposure" %in% product){
      aux.stack.exp <- terra::rast(aux.stack.exp)
      final.stack.exp <- rasterizeProduct("Exposure", final.stack.exp, aux.stack.exp,
                                          time_res, space_res, st@name, wind_threshold)
    }
    if("Profiles" %in% product){
      aux.stack.prof <- terra::rast(aux.stack.prof)
      aux.stack.direction <- terra::rast(aux.stack.direction)
    }

    if(verbose > 0)
      s <- s + 1
  }

  final.stack = c()

  if("MSW" %in% product)
    final.stack <- c(final.stack, final.stack.msw)
  if("PDI" %in% product)
    final.stack <- c(final.stack, final.stack.pdi)
  if("Exposure" %in% product)
    final.stack <- c(final.stack, final.stack.exp)
  if("Profiles" %in% product)
    final.stack <- c(final.stack, aux.stack.prof, aux.stack.direction)

  final.stack <- terra::rast(final.stack)
  final.stack <- maskProduct(final.stack, sts@spatial.loi.buffer, raster.template)

  end_time <- Sys.time()

  if(verbose > 0){
    cat("\n=== DONE with run time",as.numeric(round(end_time - start_time, 3)),"sec ===\n\n")

    if(verbose > 1){
      cat("Output:\n")
      cat("SpatRaster stack with",terra::nlyr(final.stack),"layers:\n")
      cat("index - name of layers\n")
      n = names(final.stack)
      names(n) = seq(1,terra::nlyr(final.stack))
      for(i in 1:length(n))
        cat(" ",names(n[i]),"   ",n[i],"\n")
      cat("\n")
    }

    if(verbose > 2){
      cat("\nAdditional notes:\n")
      cat("  (*) You can access each layer using:\n")
      cat("       1 - terra::subset function (See terra documentation)\n")
      cat("       2 - the accessor syntax output[[\"nameOfLayer\"]].\n")
      cat("       3 - the accessor syntax output[[index]].\n")
      cat("  (*) If any layers share the same name (e.g: same storm name), please use method 1 or 2.")
    }

  }

  return(final.stack)
}





#' Check inputs for temporalBehaviour function
#'
#' @noRd
#' @param sts StormsList object
#' @param points data.frame
#' @param product character
#' @param wind_threshold numeric
#' @param method character
#' @param asymmetry character
#' @param empirical_rmw logical
#' @param time_res numeric
#' @return NULL
checkInputsSbPt <- function(sts, points, product, wind_threshold, method, asymmetry,
                            empirical_rmw, time_res){

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

  #Checking wind_threshold input
  if("Exposure" %in% product){
    stopifnot("wind_threshold must be numeric" = identical(class(wind_threshold), "numeric"))
    stopifnot("invalid value(s) for wind_threshold input (must be > 0)" = wind_threshold > 0)
  }

  #Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland"))
  stopifnot("Only one method must be chosen" = length(method) == 1)

  #Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "Boose01"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  #Checking empirical_rmw input
  stopifnot("empirical_rmw must be logical" = identical(class(empirical_rmw), "logical"))

  #Checking time_res input
  stopifnot("time_res must be numeric" = identical(class(time_res), "numeric"))
  stopifnot("time_res must be length 1" = length(time_res) == 1)
  stopifnot("invalid time_res: must be either 1, 0.75, 0.5 or 0.25" = time_res %in% c(1, 0.75, 0.5, 0.25))

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
  pdi <- sum(pdi, na.rm = T) * time_res

  return(round(pdi,3))
}





#' rasterizeExposure counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param threshold numeric vector. Wind threshold
#'
#' @return numeric vector of length 5 (for each category).
#'  Exposure computed using the wind speed values in wind
computeExposure <- function(wind, time_res, threshold){

  exposure = c()
  for(t in threshold){
    ind <- which(wind >= t)
    expo <- rep(0,length(wind))
    expo[ind] <- 1
    exposure <- c(exposure, sum(expo, na.rm = T) * time_res)
}

  return(exposure)
}





#' rasterizeProduct counterpart function for non raster data
#'
#' @noRd
#' @param product character. Product input from temporalBehaviour
#' @param wind numeric vector. Wind speed values
#' @param direction numeric vector. Wind direction
#' @param time_res numeric. Time resolution, used for the numerical integration
#' over the whole track
#' @param result numeric array. Similar as final_stack, i.e where to add the
#' computed product
#' @param threshold numeric vector. Wind threshold
#'
#' @return numeric array of dimension:
#' \itemize{
#'   \item number of observations: number of points. If product == "MSW"
#'   \item 1 : number of points. If product == "PDI"
#'   \item 5 : number of points. If product == "Exposure"
#' }
computeProduct <- function(product, wind, direction, time_res, result, threshold){

  if(product == "TS"){
    prod <- cbind(wind, direction)
  }else if (product == "PDI") {
    prod <- computePDI(wind, time_res)

  }else if (product == "Exposure") {
    prod <- computeExposure(wind, time_res, threshold)

  }

  return(cbind(result, prod))
}





#' Arrange result before the end of temporalBehaviour
#'
#' @noRd
#' @param final_result list of data.frame. Where to add the computed product
#' @param result result output from computeProduct function
#' @param product character. Product input from temporalBehaviour
#' @param points points input from temporalBehaviour
#' @param isoT numeric vector. Iso Times of observations
#' @param indices numeric vector. Indices of observations
#' @param st Storm object.
#' @param threshold numeric vector. Wind threshold
#'
#' @return final_result
finalizeResult <- function(final_result, result, product, points, isoT, indices, st, threshold){

  if(product == "TS"){
    df <- data.frame(result, indices = indices, isoTimes = isoT)
    c <- c()
    for(i in 1:dim(points)[1]){
      c <- c(c, paste0("(",points$lon[i],",",points$lat[i],")w"), paste0("(",points$lon[i],",",points$lat[i],")dir"))
    }
    c <- c(c, "indices", "isoTimes")
    colnames(df) <- c
  }else if(product == "PDI"){
    df <- data.frame(result, row.names = "PDI")
    colnames(df) <- c(paste0("(",points$lon,",",points$lat,")"))
  }else{
    df <- data.frame(result, row.names = paste("Min threshold:",threshold,"m/s"))
    colnames(df) <- c(paste0("(",points$lon,",",points$lat,")"))
  }

  dfn <- list(df)
  names(dfn) <- st@name
  final_result <- append(final_result, dfn)

  return(final_result)
}





#' Compute indicators of storm behaviour
#'
#' This function is the pointwise version of spatialBehaviour. Available
#' products are Time Series of wind speed (TS), Power Dissipation Index (PDI)
#' and Exposure
#'
#' @param sts StormsList object
#' @param points data.frame. Contains longitude/latitude coordinates within
#' column names "lon" and "lat", on which to compute the desired product
#' @param product character. Product to compute. Must be either:
#'   \itemize{
#'     \item "TS": Time Series of wind speed
#'     \item "PDI": Power Dissipation Index
#'     \item "Exposure": hour exposition for wind greater than wind_threshold
#'           input
#'   }
#'  Default value is set to "TS"
#' @param wind_threshold numeric vector. Minimal wind threshold(s) (m/s) to
#' compute Exposure product. Ignored if Exposure is not part of the products to
#' compute. Default value is set to Saffir Simpson Hurricane Scale thresholds
#' @param method character. Cyclonic model used to compute product. Must be
#' either:
#'   \itemize{
#'   \item "Willoughby": model based on fits performed on cyclonic observations
#'   \item "Holland": model based on both basic cyclonic Physics and parameters
#'         fitting according to cyclonic observations
#'   }
#'  Default value is set to "Willoughby" (See Details)
#' @param asymmetry character. Indicates which version of asymmetry to use in
#' the computations. Must be either:
#'   \itemize{
#'   \item "Boose01": formula that substract wind speed to the original wind
#'         field depending on the position to the eye of the storm and its
#'         velocity
#'   \item "None": no asymmetry is added
#'   }
#'  Default value is set to "Boose01" (See Details)
#' @param empirical_rmw logical. Whether to compute the radius of maximum wind
#' empirically or using the radius of maximum wind from the observations.
#' Default value is set to FALSE. If TRUE, a formula extracted from Willoughby
#' et al. 2006 is used to compute rmw
#' @param time_res numeric. Period of time used to interpolate data.
#' Allowed values are 1 (60min), 0.75 (45min), 0.5 (30min), and 0.25 (15min).
#' Default value is set to 1
#'
#' @returns Computed product for each points are returned through data.frames
#' contained in a named list. Each slot, named after the storm, is a data.frame
#' that has the following dimensions:
#' \itemize{
#'   \item If product == "TS": a data frame whose number of rows corresponds to
#'         the number of interpolated observations. The columns provides
#'         respectively the wind speed values (m/s), the wind directions
#'         (in degree), the indices and the ISO time of observation
#'    \item If product == "PDI": a data.frame with one row and one column that
#'          contains PDI value for each point in points.
#'    \item If product == "Exposure": a data.frame with one row for each wind
#'          threshold and one column that contains Exposure value for each point
#'          in points.
#'    }
#'
#' @examples
#' \dontrun{
#' pts <- data.frame(lon = c(166.5, 163), lat = c(-22, -19))
#'
#' #Compute time series of wind speed for ERICA and NIRAN on points
#' #provided in pts using default settings
#' ts_nc <- temporalBehaviour(sts_nc, points = pts)
#'
#' #Compute PDI for ERICA and NIRAN on points provided in pts using default settings
#' pdiPt_nc <- temporalBehaviour(sts_nc, points = pts, product = "PDI")
#'
#' #Compute Exposure for ERICA and NIRAN on points provided in df using default settings
#' expPt_nc <- temporalBehaviour(sts_nc, points = pts, product = "Exposure", wind_threshold = c(20,30))
#' }
#'
#' @export
temporalBehaviour <- function(sts,
                              points,
                              product = "TS",
                              wind_threshold = c(18, 33, 42, 49, 58, 70),
                              method = "Willoughby",
                              asymmetry = "Boose01",
                              empirical_rmw = FALSE,
                              time_res = 1){

  checkInputsSbPt(sts, points, product, wind_threshold, method, asymmetry, empirical_rmw, time_res)

  buffer <- 2.5
  #Initializing final result
  final.result <- list()

  for(st in sts@data) {

    #Handling indices inside loi.buffer or not
    ind <- getIndices(st, 2, "none")

    it1 <- st@obs.all$iso.time[1]
    it2 <- st@obs.all$iso.time[2]
    time.diff <- as.numeric(as.POSIXct(it2) - as.POSIXct(it1))
    #Interpolated time step dt, default value dt <- 4 --> 1h
    dt <- 1 + (1 / time_res * time.diff) # + 1 for the limit values

    #Getting data associated with storm st
    dataTC <- getDataInterpolate(st, ind, dt, time.diff, empirical_rmw, method)


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


      pt <- points[i,]

      #Computing distance between eye of storm and point P
      x <- pt$lon - dataTC$lon
      y <- pt$lat - dataTC$lat


      #Intersect points coordinates with LOI
      pts <- sf::st_as_sf(as.data.frame(pt), coords = c("lon","lat"))
      sf::st_crs(pts) <- wgs84
      if(sf::st_intersects(pts, sts@spatial.loi, sparse <- FALSE) == TRUE){
        I <- 1
      }else{
        I <- 0
      }

      #Computing wind profiles
      dist2p <- dist.m[,i]
      vr <- computeWindProfile(dataTC, i, dist2p, method, asymmetry, x, y, buffer)

      #Computing wind direction
      dir <- computeWindDirection(dataTC, i, x, y, I, buffer)

      #Computing product
      res <- computeProduct(product, vr, dir, time_res, res, wind_threshold)

    }

    final.result <- finalizeResult(final.result, res, product, points,
                                   dataTC$isoTimes, dataTC$indices, st,
                                   wind_threshold)

  }

  return(final.result)
}
