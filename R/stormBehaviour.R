



#' Linear interpolation of a vector
#'
#' @noRd
#' @param x numeric vector, to approximate
#'
#' @return linear interpolated x vector
linearInterpolation = function(x){

  l = length(x)
  step = (x[l] - x[1])/l

  return(seq(x[1], x[l], step))

}





#' Compute the Radius of Maximum Wind
#'
#' It is an empirical formula extracted from Willoughby et al. 2006 model
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the storm
#'
#' @returns Radius of Maximum Wind (km)
getRmw = function(msw, lat) {
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
Willoughby_profile = function(r, rmw, msw, lat){

  if (r >= rmw) {
    xx1 = 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat)
    xx2 = 25
    aa = 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat)
    vr = msw * ((1 - aa) * exp(-abs((r - rmw) / xx1)) + aa * exp(-abs(r -
                                                                          rmw) / xx2))
  } else{
    nn = 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat)
    vr = msw * abs((r / rmw) ^ nn)
  }

  return(vr)
}

#Vectorize version of the above model
Willoughby = Vectorize(Willoughby_profile, vectorize.args = "r")





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
Holland80_profile = function(r, rmw, msw, pc, poci, lat){

  rho = 1.15  #air densiy
  f = 2 * 7.29 *10**(-5) * sin(lat) #Coriolis parameter
  b = rho * exp(1) * msw**2 / (poci - pc)

  vr = sqrt(b/rho * (rmw/r)**b * (poci - pc)*exp(-(rmw/r)**b) + (r*f/2)**2) - r*f/2

  return(vr)

}

#Vectorize version of the above model
Holland80 = Vectorize(Holland80_profile, vectorize.args = "r")





#' Parametrization of surface drag coefficient according to Wang, G. et al. 2022
#'
#' @noRd
#' @param vr numeric. Radial wind speed (m/s)
#'
#' @return Associated surface drag coefficient
compute_Cd = function(vr){
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
rasterizeCd = Vectorize(compute_Cd, vectorize.args = "vr")





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
checkInputsSb = function(sts, product, method, asymmetry,
                         empirical_rmw, format, space_res,
                         time_res, verbose, focus_loi){

  #Checking sts input
  stopifnot("no data found" = !missing(sts))

  #Checking product input
  stopifnot("Invalid product" = product %in% c("MSW", "PDI", "Exposure"))
  stopifnot("Only one product must be chosen" = length(product) == 1)

  #Checking method input
  stopifnot("Invalid method input" = method %in% c("Willoughby", "Holland80"))
  stopifnot("Only one method must be chosen" = length(method) == 1)

  #Checking asymmetry input
  stopifnot("Invalid asymmetry input" = asymmetry %in% c("None", "V1", "V2"))
  stopifnot("Only one asymmetry must be chosen" = length(asymmetry) == 1)

  #Checking empirical_rmw input
  stopifnot("empirical_rmw must be logical" = identical(class(empirical_rmw), "logical"))

  #Checking format input
  if(identical(class(format),"data.frame")){
    stopifnot("colnames of format must be lon, lat" = colnames(format) == c("lon","lat"))
    stopifnot("Invalid format coordinates" = format$lon >= 0 & format$lon <= 360 &
                format$lat >= -90 & format$lat <= 90)

  }else{
    stopifnot("Invalid format input" = format %in% c("profiles","analytic"))
    stopifnot("format should be length 1" = length(format) == 1)
  }

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





#' Generate raster template for the computations
#'
#' @noRd
#' @param buffer sf object. LOI + buffer extention
#' @param res numeric. Space resolution (km) for the template
#'
#' @return a SpatRaster
makeTemplateRaster = function(buffer, res){

  #Derivating the raster template
  ext = terra::ext(sf::st_bbox(buffer)$xmin,
                    sf::st_bbox(buffer)$xmax,
                    sf::st_bbox(buffer)$ymin,
                    sf::st_bbox(buffer)$ymax)

  ras = terra::rast(
    xmin = ext$xmin,
    xmax = ext$xmax,
    ymin = ext$ymin,
    ymax = ext$ymax,
    vals = NA
  )
  #Projection in Mercator
  ras = terra::project(ras, "EPSG:3857")
  #Resampling in new resolution in Mercator
  template = ras
  terra::res(template) = c(res * 1000, res * 1000)
  template = terra::resample(ras, template)
  #Reprojection in lon/lat
  template = terra::project(template, "EPSG:4326")

  #Handling time line crossing
  if(terra::ext(template)$xmin < 0)
    template = terra::rast(resolution = terra::res(template), extent = ext)

  return(template)

}





#' Get indices for computations
#'
#' Whether to get only observations inside LOI + buffer extention (+ offset) or
#' getting all the observations
#'
#' @noRd
#' @param st Storm Object
#' @param format character. format input from stormBehaviour
#' @param focus_loi logical. logical input from stormBehaviour
#'
#' @return numeric vector gathering the indices of observation to use to perform
#' the further computations
getIndices = function(st, format, focus_loi){

  #Handling indices inside loi.buffer or not
  if (focus_loi) {
    #Use observations within the loi for the computations
    ind = seq(st@obs[1], st@obs[st@numobs], 1)

    if(!identical(class(format),"data.frame")){
      if(format == "analytic"){
        #Handling indices and offset (2 outside of loi at entry and exit)
        if (st@obs[1] >= 3) {
          ind = c(st@obs[1] - 2, st@obs[1] - 1, ind)

        } else if (st@obs[1] == 2) {
          ind = c(st@obs[1] - 1, ind)
        }

        if (st@obs[st@numobs] <= st@numobs.all - 2) {
          ind = c(ind, st@obs[st@numobs] + 1, st@obs[st@numobs] + 2)

        } else if (st@obs[st@numobs] == st@numobs.all - 1) {
          ind = c(ind, st@obs[st@numobs] + 1)
        }
      }
    }
  } else{
    #Use all observations available for the computations
    ind = seq(1, st@numobs.all, 1)
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
getData = function(st, indices , asymmetry, empirical_rmw, method){

  data = data.frame(
    lon = st@obs.all$lon[indices],
    lat = st@obs.all$lat[indices]
  )

  data$storm.speed = NA
  data$vx.deg = NA
  data$vy.deg = NA

  #Computing storm velocity (m/s)
  for(i in 1:(dim(data)[1]-1)){
    data$storm.speed[i] = terra::distance(x = cbind(data$lon[i],data$lat[i]),
                                         y = cbind(data$lon[i+1],data$lat[i+1]),
                                         lonlat = T) * (0.001 / 3) / 3.6

    #component wise velocity in both x and y direction (degree/h)
    data$vx.deg[i] = (data$lon[i + 1] - data$lon[i]) / 3
    data$vy.deg[i] = (data$lat[i + 1] - data$lat[i]) / 3
  }

  if(asymmetry == "V2"){
    data$msw = st@obs.all$msw[indices] - data$storm.speed
  }else{
    data$msw = st@obs.all$msw[indices]
  }

  if(empirical_rmw){
    data$rmw = getRmw(data$msw, data$lat)
  }else{
    if(all(is.na(st@obs.all$rmw[indices])))
      stop("Missing rmw data to perform model. Consider setting empirical_rmw to TRUE")
    data$rmw = st@obs.all$rmw[indices]
  }

  if(method == "Holland80"){
    if(all(is.na(st@obs.all$poci[indices])) || all(is.na(st@obs.all$pres[indices])))
      stop("Missing pressure data to perform Holland80 model")

    data$poci = st@obs.all$poci[indices]
    data$pc = st@obs.all$pres[indices]
  }


  return(data)

}





#' Compute number of step to perform for the further computations
#'
#' (only if verbose == T)
#'
#' @noRd
#' @param format format input from stormBehaviour
#' @param last_obs numeric. Indicates the number of observations to further
#' interpolate and compute results
#' @param dt numeric. time step
getNbStep = function(format, last_obs, dt){

  if(format == "analytic"){
    nb_step = dt * last_obs - last_obs
  }else if (format == "profiles"){
    nb_step = last_obs
  }

  return(nb_step)
}




#' Interpolate a variable from the data to perfom further computations
#'
#' @noRd
#' @param var numeric vector from data extracted from getData
#' @param dt numeric. Time step
#' @param index numeric. Starting index in which var should be interpolated
#'
#' @return numeric vector of length d. Interpolated var
interpolateVariable = function(var, dt, index){

  res = rep(NA, dt)
  res[1] = var[index]
  res[dt] = var[index + 1]
  res = zoo::na.approx(res)

  return(res)

}




#' Interpolate a set of data associated with one storm to perform further computations
#'
#' @noRd
#' @param data data object
#' @param index numeric. Index of the storm to interpolate data in the overall
#' dataset generated with getData function
#' @param dt numeric. time step
#' @param method character. method input from stormBehvaiour
#'
#' @return a data.frame of dimension  dt: 10. Interpolated data starting at observations
#' index until index + dt. Columns are
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
getInterpolatedData = function(data, index, dt, method){

  if(method == "Holland80"){
    df = data.frame(lon = interpolateVariable(data$lon, dt, index),
                    lat = interpolateVariable(data$lat, dt, index),
                    msw = interpolateVariable(data$msw, dt, index),
                    rmw = interpolateVariable(data$rmw, dt, index),
                    pc  = interpolateVariable(data$pc, dt, index),
                    poci = interpolateVariable(data$poci, dt, index),
                    vx.deg = rep(data$vx.deg[index], dt),
                    vy.deg = rep(data$vy.deg[index], dt),
                    storm.speed = rep(data$storm.speed[index], dt))
  }else{
    df = data.frame(lon = interpolateVariable(data$lon, dt, index),
                    lat = interpolateVariable(data$lat, dt, index),
                    msw = interpolateVariable(data$msw, dt, index),
                    rmw = interpolateVariable(data$rmw, dt, index),
                    vx.deg = rep(data$vx.deg[index], dt),
                    vy.deg = rep(data$vy.deg[index], dt),
                    storm.speed = rep(data$storm.speed[index], dt))
  }


  return(df)

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
makeTemplateModel = function(raster_template, buffer, data, index){

  template = terra::rast(xmin = data$lon[index] - buffer,
                         xmax = data$lon[index] + buffer,
                         ymin = data$lat[index] - buffer,
                         ymax = data$lat[index] + buffer,
                         res = terra::res(raster_template),
                         vals = NA)
  terra::origin(template) = terra::origin(raster_template)

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
asymmetryV1 = function(x, y, vx, vy, basin){

  if(basin %in% c("SA", "SP", "SI")){
    #Southern Hemisphere, t is counterclockwise
    res = atan2(vy,vx) - atan2(y,x) + pi

  }else{
    #Northern Hemisphere, t is clockwise
    res = atan2(y,x) - atan2(vy,vx)  + pi
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
asymmetryV2 = function(x, y, vx, vy, basin){

  if(basin %in% c("SA", "SP", "SI")){
    #Southern Hemisphere, t is clockwise
    res = acos((y * vx - x * vy) / (sqrt(vx**2 + vy**2) * sqrt(x**2 + y**2)))

  }else{
    #Northern Hemisphere, t is counterclockwise
    res = acos((- y * vx + x * vy) / (sqrt(vx**2 + vy**2) * sqrt(x**2 + y**2)))
  }

  return(res)
}





#' Compute wind profile according to the selected method and asymmetry
#'
#' @noRd
#' @param method character. method input form stormBehaviour
#' @param asymmetry character. asymmetry input from stormBehviour
#' @param format format input from stormBehaviour
#' @param basin numeric. basin name to choose wind orientation
#' @param data data.frame. Data generated with getInterpolatedData function
#' @param index numeric. Index of interpolated observation in data to use for
#' the computations
#' @param dist_m numeric array. Distance in meter from the eye of the storm for
#' each coordinate of the raster_template_model (or points if format is a data frame)
#' @param x numeric array. Distance in degree from the eye of storm in the x direction
#' @param y numeric array. Distance in degree from the eye of storm in the y direction
#'
#' @return
#'   \itemize{
#'     \item If format is a data frame, the wind speed values at each observation
#'     \item Otherwise, SpatRaster that contains the wind speed profile at observations index of data
#'  }
computeWindProfile = function(method, asymmetry, format, basin, data, index, dist_m, x, y){


  if(identical(class(format),"data.frame"))
    dist_m = dist_m[,index]


  #Computing sustained wind according to the input model
  if (method == "Willoughby") {
    wind = Willoughby(
      msw = data$msw[index],
      lat = data$lat[index],
      r = dist_m * 0.001,
      rmw = data$rmw[index]
    )

  }else if (method == "Holland80") {
    wind = Holland80(
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
    angle = asymmetryV1(x, y, data$vx.deg[index], data$vy.deg[index], basin)
    wind = wind - (1 - sin(angle))*(data$storm.speed[index]/3.6)/2

  } else if(asymmetry == "V2"){
    angle = asymmetryV2(x, y, data$vx.deg[index], data$vy.deg[index], basin)
    wind = wind + cos(angle)* data$storm.speed[index]
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
stackRaster = function(stack, raster_template, raster_wind, is_basin, extent){

  ras = raster_template
  if(is_basin)
    ras = terra::crop(ras, extent)

  ras = terra::merge(raster_wind, ras)
  ras = terra::crop(ras, extent)

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
stackRasterPDI = function(stack, raster_template, raster_wind, is_basin, extent){

  raster.cd = raster_wind
  terra::values(raster.cd) = rasterizeCd(terra::values(raster_wind))

  rho = 0.001
  #Raising to power 3
  raster_wind = raster_wind ** 3
  #Applying both rho and surface drag coefficient
  raster_wind = raster_wind * rho * raster.cd

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
stackRasterExposure = function(stack, raster_template, raster_wind, is_basin, extent){

  for(c in 2:6){
    raster_c_model = raster_wind
    terra::values(raster_c_model) = NA
    ind = which(terra::values(raster_wind) >= sshs[c] &
                  terra::values(raster_wind) < sshs[c+1])
    raster_c_model[ind] = 1
    stack = stackRaster(stack, raster_template, raster_c_model, is_basin, extent)
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
stackProduct = function(product, stack, raster_template, raster_wind, is_basin, extent){

  if (product == "MSW") {
    stack = stackRaster(stack, raster_template, raster_wind, is_basin, extent)

  }else if (product == "PDI"){
    stack = stackRasterPDI(stack, raster_template, raster_wind, is_basin, extent)

  }else if (product == "Exposure"){
    stack = stackRasterExposure(stack, raster_template, raster_wind, is_basin, extent)
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
rasterizeMSW = function(final_stack, stack, name){

  msw = max(stack, na.rm = T)
  #Applying focal function twice to smooth results
  msw = terra::focal(msw, w = matrix(1, 3, 3), max, na.rm = T, pad = T)
  msw = terra::focal(msw, w = matrix(1, 3, 3), mean, na.rm = T, pad = T)
  names(msw) = paste0(name, "_MSW")

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
rasterizePDI = function(final_stack, stack, time_res, name){

  #Integrating over the whole track
  pdi = sum(stack, na.rm = T) * time_res
  #Applying focal function to smooth results
  pdi = terra::focal(pdi, w = matrix(1, 3, 3), sum, na.rm = T, pad = T)
  names(pdi) = paste0(name, "_PDI")

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
rasterizeExposure = function(final_stack, stack, time_res, name){

  #For each category in SSHS
  all.categories = c()
  for (i in c(1, 2, 3, 4, 0)) {
    ind = which(seq(1, terra::nlyr(stack)) %% 5 == i)
    #Integrating over the whole track
    exposure = sum(terra::subset(stack, ind), na.rm = T) * time_res
    #Applying focal function to smooth results
    exposure = terra::focal(exposure, w = matrix(1, 3, 3), sum, na.rm = T, pad = T)
    if (i == 0)
      i = 5

    names(exposure) = paste0(name, "_Exposure", i)
    final_stack = c(final_stack, exposure)
    all.categories = c(all.categories, exposure)
  }

  #Adding all categories
  all.categories = terra::rast(all.categories)
  all.categories = sum(all.categories, na.rm = T)
  names(all.categories) = paste0(name, "_ExposureAll")

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
rasterizeProduct = function(product, format, final_stack, stack, time_res, name, indices){


  if (product == "MSW") {
    if(format == "profiles"){
      names(stack) = paste0(name, "_profile", indices[1:length(indices)-1])
      final_stack = c(final_stack, stack)

    }else{
      #Computing MSW analytic raster
      final_stack = rasterizeMSW(final_stack, stack, name)
    }

  } else if (product == "PDI") {
    #Computing PDI analytic raster
    final_stack = rasterizePDI(final_stack, stack, time_res, name)

  } else if (product == "Exposure") {
    #Computing Exposure analytic raster
    final_stack = rasterizeExposure(final_stack, stack, time_res, name)

  }

  return(final_stack)

}





#' rasterizePDI counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#'
#' @return numeric. PDI computed using the wind speed values in wind
computePDI = function(wind){
  #Computing surface drag coefficient
  cd = rasterizeCd(wind)

  rho = 0.001
  #Raising to power 3
  pdi = wind ** 3
  #Applying both rho and surface drag coefficient
  pdi = wind * rho * cd
  #Integrating over the whole track
  pdi = sum(pdi, na.rm = T) * 3

  return(pdi)
}





#' rasterizeExposure counterpart function for non raster data
#'
#' @noRd
#' @param wind numeric vector. Wind speed values
#'
#' @return numeric vector of length 5 (for each category).
#'  Exposure computed using the wind speed values in wind
computeExposure = function(wind){

  exposure = c()

  for(c in 2:6){
    ind = which(wind >= sshs[c] & wind < sshs[c+1])
    wind.aux = rep(0,length(wind))
    wind.aux[ind] = 1
    wind.aux = sum(wind.aux, na.rm = T) * 3
    exposure  = c(exposure, wind.aux)
  }

  return(exposure)
}





#' rasterizeProduct counterpart function for non raster data
#'
#' @noRd
#' @param product character. Product input from stormBehaviour
#' @param wind numeric vector. Wind speed values
#' @param result numeric array. Similar as final_stack, i.e where to add the
#' computed product
#'
#' @return numeric array of dimension:
#' \itemize{
#'   \item number of observations: number of points. If product == "MSW"
#'   \item 1 : number of points. If product == "PDI"
#'   \item 5 : number of points. If product == "Exposure"
#' }
computeProduct = function(product, wind, result){

  if(product == "MSW"){
    #Computing MSW
    prod = wind
  }else if (product == "PDI") {
    #Computing PDI
    prod = computePDI(wind)

  }else if (product == "Exposure") {
    #Computing Exposure
    prod = computeExposure(wind)

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
maskProduct = function(final_stack, focus_loi, loi, template){

  if (focus_loi) {
    #Masking the stack to fit loi
    v = terra::vect(loi)
    m = terra::rasterize(v, template)
    return(terra::mask(final_stack, m))
  }else{
    return(final_stack)
  }
}





#' Arrange result before the end of stormBehaviour
#'
#' @noRd
#' @param final_result list of data.frame. Where to add the computed product
#' @param result result output from computeProduct function
#' @param product character. Product input from stormBehaviour
#' @param format format input from stormBehaviour
#' @param indices numeric vector. Indices of observations
#' @param name character. Name of the storm
#'
#' @return final_result
finalizeResult = function(final_result, result, product, format, indices, name){

  if(product == "MSW"){
    df = data.frame(result, row.names = indices)
  }else if(product == "PDI"){
    df = data.frame(result, row.names = "PDI")
  }else{
    df = data.frame(result ,row.names = c("Cat.1", "Cat.2", "Cat.3", "Cat.4", "Cat.5"))
  }

  colnames(df) = paste0("(",format$lon,",",format$lat,")")


  dfn = list(df)
  names(dfn) = name
  final_result = append(final_result, dfn)

  return(final_result)
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
#'   \item "Holland80"
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
#' @param format either a character among "analytic" and "profiles", or a
#'   data.frame which contains longitude/latitude coordinates within column
#'   names "lon" and "lat". Represents the format results the function
#'   should compute.
#'   \itemize{
#'     \item  If "analytic", analytic rasters (integration in space and
#'      time over the track) are returned.
#'     \item  If "profiles", product input is
#'   ignored and set to "MSW" and 2D wind speed structures for each observation
#'   are returned.
#'     \item If `data.frame`, computed product for each coordinates are
#'   returned (time series if product == "MSW")
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
#'  computed as follow: stormName_profileIndex
#'   \item If `data.frame`, computed product for each coordinates are returned
#'   through a name numeric vector of dimension
#'    \itemize{
#'    \item (number of observations:number of point coordinates), if product == "MSW".
#'     Correspond in fact of time series of wind speed along every observations
#'    \item (1:number of point coordinates), if product == "PDI"
#'    \item (6:number of point coordinates), if product == "Exposure"
#'    }
#' }
#'
#' @examples
#' #Compute analytic MSW for PAM 2015 in Vanuatu using Willougbhy model with
#' #version 2 of asymmetry
#' msw_pam = stormBehaviour(pam, asymmetry = "V2", verbose = TRUE)
#'
#' #Compute analytic PDI for ERICA and NIRAN in New Caledonia using Holland
#' # model without asymmetry
#' pdi_nc = stormBehaviour(sts_nc, time_res = 0.5, method = "Holland80",
#'                         product = "PDI", verbose = TRUE)
#'
#' #Compute profiles wind speed for ERICA and NIRAN in New Caledonia using
#' #Willoughby model without asymmetry
#' prof_nc = stormBehaviour(sts_nc, format = "profiles", verbose = TRUE)
#'
#' #Compute time series of wind speed for ERICA and NIRAN in New Caledonia using
#' #Willoughby model without asymmetry
#' df = data.frame(lon = c(166.5, 163), lat = c(-22, -19))
#' ts_nc = stormBehaviour(sts_nc, format = df)
#'
#' @export
stormBehaviour = function(sts, product = "MSW", method = "Willoughby", asymmetry = "None",
                          empirical_rmw = FALSE, format = "analytic", space_res = 10,
                          time_res = 1, verbose = FALSE, focus_loi = TRUE){

  checkInputsSb(sts, product, method, asymmetry, empirical_rmw, format,
                space_res, time_res, verbose, focus_loi)


  if(!identical(class(format),"data.frame")){

    if(format == "profiles")
      product = "MSW"

    #Make raster template
    raster.template = makeTemplateRaster(sts@spatial.loi.buffer, space_res)
    #Getting new extent
    ext = terra::ext(raster.template)
    #Buffer size in degree
    buffer = terra::res(raster.template)[1] * sts@buffer / space_res
    #Initializing final raster stack
    final.stack = c()
  }else{
    #Initializing final result
    final.result = list()
  }

  if(verbose)
    s = 1 #Initializing count of storms

  for (st in sts@data) {

    #Handling indices inside loi.buffer or not
    ind = getIndices(st, format, focus_loi)

    #Getting data associated with storm st
    dat = getData(st, ind, asymmetry, empirical_rmw, method)

    #Reduce extent of raster if loi represents the whole basin
    if(sts@loi.basin & !identical(class(format),"data.frame"))
      ext = terra::ext(min(dat$lon) - buffer, max(dat$lon) + buffer,
                     min(dat$lat) - buffer, max(dat$lat) + buffer)


    if(!identical(class(format),"data.frame")){

      #Interpolated time step dt, default value dt = 4 --> 1h
      dt = 1 + (1 / time_res * 3) # + 1 for the limit values
      last.obs = dim(dat)[1] - 1

      if (verbose) {
        #Computing number of steps
        nb.step = getNbStep(format, last.obs, dt)
        step = 1
        cat("Computing", format, product, "rasters using", method,
            "model (time_res:", time_res, "h, space_ras:", space_res,
            "km, asymmetry:", asymmetry, ", empirical_rmw:", empirical_rmw,
            ") for", st@name, "(", s, "/", sts@nb.storms, ")\n")
        pb = utils::txtProgressBar(min = step, max = nb.step, style = 3)
      }

      aux.stack = c()

      #For every general 3H observations
      for (j in 1:last.obs) {

        #Interpolate variables
        dataInter = getInterpolatedData(dat, j, dt, method)

        #For every interpolated time steps dt
        for (i in 1:dt) {

          if (format == "analytic" & i == dt)
            break #avoid redondance

          #Making template to compute wind profiles
          raster.template.model = makeTemplateModel(raster.template, buffer, dataInter, i)
          raster.wind = raster.template.model

          #Computing coordinates to the eye of the storm for x and y axes
          x = (terra::crds(raster.wind, na.rm = FALSE)[, 1] - dataInter$lon[i])
          y = (terra::crds(raster.wind, na.rm = FALSE)[, 2] - dataInter$lat[i])

          #Computing distances to the eye of the storm in m
          dist.m = terra::distance(x = terra::crds(raster.wind, na.rm = FALSE)[, ],
                                   y = cbind(dataInter$lon[i], dataInter$lat[i]),
                                   lonlat = T)

          #Computing wind profile
          terra::values(raster.wind) = computeWindProfile(method, asymmetry,
                                                           format, sts@basin,
                                                           dataInter, i, dist.m,
                                                           x, y)

          #Stacking product
          aux.stack = stackProduct(product, aux.stack, raster.template,
                                   raster.wind, sts@loi.basin, ext)


          if(format == "profiles")
            break #profiles only computed on observations

          if (verbose){
            utils::setTxtProgressBar(pb, step)
            step = step + 1
          }

        }
      }

      if (verbose)
        close(pb)

      #Rasterize final product
      aux.stack = terra::rast(aux.stack)
      final.stack = rasterizeProduct(product, format, final.stack, aux.stack,
                                     time_res, st@name, ind)

    }else{
      #Computing distances from the eye of storm for every observations x, and
      #every points y
      dist.m = terra::distance(
        x = cbind(dat$lon,dat$lat),
        y = cbind(format$lon, format$lat),
        lonlat = T
      )

      res = c()
      #For each point
      for(i in 1:dim(format)[1]){

        #Computing coordinates between eye of storm and point P
        x = format$lon[i] - dat$lon
        y = format$lat[i] - dat$lat

        #Computing wind profiles
        vr = computeWindProfile(method, asymmetry, format, sts@basin, dat, i, dist.m, x, y)

        #Computing product
        res = computeProduct(product, vr, res)

      }

      final.result = finalizeResult(final.result,  res, product, format, ind, st@name)


    }

    if(verbose)
      s = s + 1
  }


  if(!identical(class(format),"data.frame")){
    final.stack = terra::rast(final.stack)
    final.stack = maskProduct(final.stack, focus_loi,
                              sts@spatial.loi.buffer, raster.template)

    return(final.stack)

  }else{

    return(final.result)
  }

}
