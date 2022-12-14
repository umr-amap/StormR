




#' Compute the Radius of Maximum Wind
#'
#' It is an empirical formula extracted from Willoughby et al. 2006 model
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param lat numeric. Should be between -90 and 90. Latitude of the eye of the storm
#'
#' @returns Radius of Maximum Wind (km)
getRmw = function(msw,
                   lat) {
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
Willoughby_profile = function(r,
                              rmw,
                              msw,
                              lat
                              ){

  rmw = rep(rmw, length(r))
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
Holland80_profile = function(r,
                            rmw,
                            msw,
                            pc,
                            poci,
                            lat){

  rho = 1.15  #air densiy
  f = 2 * 7.29 *10**(-5) * sin(lat) #Coriolis parameter
  b = rho * exp(1) * msw**2 / (poci - pc)

  vr = sqrt(b/rho * (rmw/r)**b * (poci - pc)*exp(-(rmw/r)**b) + (r*f/2)**2) - r*f/2


  return(vr)

}

#Vectorize version of the above model
Holland80 <- Vectorize(Holland80_profile, vectorize.args = "r")





#' Parametrization of surface drag coefficient according to Wang, G. et al. 2022
#'
#'@noRd
#' @param vr numeric. Radial wind speed (m/s)
#'
#' @return Associated surface drag coefficient

compute_Cd = function(vr){
  if(vr <= 18){
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
  stopifnot("space_res must be as integer" = is_wholenumber(space_res))
  stopifnot("space_res must be length 1" = length(space_res) == 1)
  stopifnot("space_res must be positif" = space_res > 0)

  #Checking time_res input
  stopifnot("time_res must be numeric" = identical(class(time_res), "numeric"))
  stopifnot("invalid time_res" = time_res %in% c(1, 0.75, 0.5, 0.25))
  stopifnot("time_res must be length 1" = length(time_res) == 1)

  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose), "logical"))

  #Checking focus_loi input
  stopifnot("focus_loi must be logical" = identical(class(focus_loi), "logical"))


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
#'    \item (number of observations ,number of point coordinates), if product == "MSW"
#'    \item (1 ,number of point coordinates), if product == "PDI"
#'    \item (6,number of point coordinates), if product == "Exposure"
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
                space_res,time_res, verbose, focus_loi)

  if(!identical(class(format),"data.frame")){
    if(format == "profiles")
      product = "MSW"
  }


  if(!identical(class(format),"data.frame")){

    #Derivating the raster template
    ext <- terra::ext(sf::st_bbox(sts@spatial.loi.buffer)$xmin,
                    sf::st_bbox(sts@spatial.loi.buffer)$xmax,
                    sf::st_bbox(sts@spatial.loi.buffer)$ymin,
                    sf::st_bbox(sts@spatial.loi.buffer)$ymax)

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
    raster.template = ras
    terra::res(raster.template) = c(space_res * 1000, space_res * 1000)
    raster.template <- terra::resample(ras, raster.template)
    #Reprojection in lon/lat
    raster.template = terra::project(raster.template, "EPSG:4326")

    #Handling time line crossing
    if(terra::ext(raster.template)$xmin < 0)
      raster.template = terra::rast(resolution = terra::res(raster.template), extent = ext)

    #Getting new extent
    ext = terra::ext(raster.template)

    #Buffer size in degree
    buffer = terra::res(raster.template)[1] * sts@buffer / space_res

    #Initializing final raster stack
    final.stack = c()
  }

  s = 1 #Initializing count of storms

  for (st in sts@data) {


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


    #Getting all variables and removing NAs
    dat = data.frame(
      lon = st@obs.all$lon[ind],
      lat = st@obs.all$lat[ind],
      msw = st@obs.all$msw[ind],
      rmw = st@obs.all$rmw[ind],
      roci = st@obs.all$roci[ind],
      pc = st@obs.all$pres[ind],
      poci = st@obs.all$poci[ind]
    )
    dat = dat[stats::complete.cases(dat), ]
    dat$storm.speed = NA
    dat$vx.deg = NA
    dat$vy.deg = NA

    #To reduce size of raster if loi represents the whole basin
    if(sts@loi.basin & !identical(class(format),"data.frame"))
      ext = terra::ext(min(dat$lon) - buffer,
                     max(dat$lon) + buffer,
                     min(dat$lat) - buffer,
                     max(dat$lat) + buffer)


    #Computing storm velocity (m/s)
    for(i in 1:(dim(dat)[1]-1)){
      dat$storm.speed[i] = terra::distance(
        x = cbind(dat$lon[i],dat$lat[i]),
        y = cbind(dat$lon[i+1],dat$lat[i+1]),
        lonlat = T
      ) * (0.001 / 3) / 3.6

      #component wise velocity in both x and y direction (degree/h)
      dat$vx.deg[i] = (dat$lon[i + 1] - dat$lon[i]) / 3
      dat$vy.deg[i] = (dat$lat[i + 1] - dat$lat[i]) / 3

    }


    if(!identical(class(format),"data.frame")){

      #Interpolated time step dt, default value dt = 4 --> 1h
      dt = 1 + (1 / time_res * 3) # + 1 for the limit values

      #Computing number of steps
      last.obs = dim(dat)[1]
      if(format == "analytic"){
        last.obs = dim(dat)[1] -1
        nb.steps = dt * last.obs - (last.obs - 1)
      }else{
        last.obs = dim(dat)[1]
        nb.steps = last.obs
      }
      step = 1

      if (verbose) {
        cat("Computing",
            format,
            product,
            "rasters using",
            method,
            "model (time_res:",
            time_res,
            "h, space_ras:",
            space_res,
            "km, asymmetry:",
            asymmetry,
            ", empirical_rmw:",
            empirical_rmw,
            ") for",
            st@name,
            "(",
            s,
            "/",
            sts@nb.storms,
            ")\n")
        pb = utils::txtProgressBar(min = 1,
                                   max = last.obs - 1,
                                   style = 3)
      }

      aux.stack = c()

      #For every general 3H observations
      for (j in 1:last.obs) {

        lon = rep(NA, dt)
        lon[1] = dat$lon[j]
        lon[dt] = dat$lon[j + 1]
        lon = zoo::na.approx(lon)

        lat = rep(NA, dt)
        lat[1] = dat$lat[j]
        lat[dt] = dat$lat[j + 1]
        lat = zoo::na.approx(lat)

        msw = rep(NA, dt)
        msw[1] = dat$msw[j]
        msw[dt] = dat$msw[j + 1]
        msw = zoo::na.approx(msw)

        pc = rep(NA, dt)
        pc[1] = dat$pc[j]
        pc[dt] = dat$pc[j + 1]
        pc = zoo::na.approx(pc)

        poci = rep(NA, dt)
        poci[1] = dat$poci[j]
        poci[dt] = dat$poci[j + 1]
        poci = zoo::na.approx(poci)

        rmw = rep(NA, dt)
        rmw[1] = dat$rmw[j]
        rmw[dt] = dat$rmw[j + 1]
        rmw = zoo::na.approx(rmw)

        vx.deg = dat$vx.deg[j]
        vy.deg = dat$vy.deg[j]
        storm.speed = dat$storm.speed[j]

        #For every interpolated time steps dt
        for (i in 1:dt) {
          if (format == "analytic" & i == dt & j != last.obs)
            break #avoid redondance

          #Raster to compute model
          raster.template.model = terra::rast(xmin = lon[i] - buffer,
                                       xmax = lon[i] + buffer,
                                       ymin = lat[i] - buffer,
                                       ymax = lat[i] + buffer,
                                       res = terra::res(raster.template),
                                       vals = NA)
          terra::origin(raster.template.model) = terra::origin(raster.template)
          raster.model = raster.template.model

          #Computing distances to the eye of the storm for x and y axes
          x = (terra::crds(raster.model, na.rm = FALSE)[, 1] - lon[i])
          y = (terra::crds(raster.model, na.rm = FALSE)[, 2] - lat[i])
          #Computing distances to the eye of the storm in m
          dist.m = terra::distance(
            x = terra::crds(raster.model, na.rm = FALSE)[, ],
            y = cbind(lon[i], lat[i]),
            lonlat = T
          )

          if(asymmetry == "V2")
            msw[i] = msw[i] - storm.speed

          if(empirical_rmw)
            rmw[i] = getRmw(msw[i],lat[i])

          #Computing sustained wind according to the input model
          if (method == "Willoughby") {
            terra::values(raster.model) = Willoughby(
              msw = msw[i],
              lat = lat[i],
              r = dist.m * 0.001,
              rmw = rmw[i]
            )

          }else if (method == "Holland80") {
            terra::values(raster.model) = Holland80(
              r = dist.m * 0.001,
              rmw = rmw[i],
              msw = msw[i],
              pc = pc[i] * 100,
              poci = poci[i] * 100,
              lat = lat[i]
            )
          }

          #Adding asymmetry
          if (asymmetry == "V1") {
            #Boose version
            raster.t = raster.template.model
            if(sts@basin %in% c("SA", "SP", "SI")){
              #Southern Hemisphere, t is counterclockwise
              terra::values(raster.t) = atan2(vy.deg,vx.deg) - atan2(y,x) + pi

            }else{
              #Northern Hemisphere, t is clockwise
              terra::values(raster.t) = atan2(y,x) - atan2(vy.deg,vx.deg)  + pi
            }
            terra::values(raster.model) = terra::values(raster.model) - (1 - sin(terra::values(raster.t)))*(storm.speed/3.6)/2

          } else if(asymmetry == "V2"){
            raster.t = raster.template.model
            if(sts@basin %in% c("SA", "SP", "SI")){
              #Southern Hemisphere, t is clockwise
              terra::values(raster.t) = acos((y * vx.deg - x * vy.deg) / (sqrt(vx.deg**2 + vy.deg**2) * sqrt(x**2 + y**2)))

            }else{
              #Northern Hemisphere, t is counterclockwise
              terra::values(raster.t) = acos((- y * vx.deg + x * vy.deg) / (sqrt(vx.deg**2 + vy.deg**2) * sqrt(x**2 + y**2)))
            }
            terra::values(raster.model) = terra::values(raster.model) + cos(terra::values(raster.t))* storm.speed
          }

          if (product == "MSW") {
            raster.msw = raster.template
            if(sts@loi.basin)
              raster.msw = terra::crop(raster.msw,ext)

            raster.msw = terra::merge(raster.msw,raster.model)
            raster.msw = terra::crop(raster.msw,ext)
            aux.stack = c(aux.stack, raster.msw)

          }else if (product == "PDI"){
            raster.cd = raster.template.model
            terra::values(raster.cd) = rasterizeCd(terra::values(raster.model))

            rho = 0.001
            #Raising to power 3
            raster.model = raster.model ** 3
            #Applying both rho and surface drag coefficient
            raster.model = raster.model * rho * raster.cd

            raster.msw = raster.template
            if(sts@loi.basin)
              raster.msw = terra::crop(raster.msw,ext)

            raster.msw = terra::merge(raster.msw,raster.model)
            raster.msw = terra::crop(raster.msw,terra::ext(raster.template))
            aux.stack = c(aux.stack, raster.msw)

          }else if (product == "Exposure"){
            for(c in 2:6){
              raster.c = raster.template
              if(sts@loi.basin)
                raster.c = terra::crop(raster.c,ext)

              raster.c.model = raster.template.model
              ind = which(terra::values(raster.model) >= sshs[c] &
                            terra::values(raster.model) < sshs[c+1])
              raster.c.model[ind] = 1
              raster.c = terra::merge(raster.c,raster.c.model)
              raster.c = terra::crop(raster.c,terra::ext(raster.template))
              aux.stack = c(aux.stack, raster.c)
            }
          }

          step = step + 1
          if(format == "profiles")
            break
        }

        if (verbose)
          utils::setTxtProgressBar(pb, j)
      }

      if (verbose)
        close(pb)


      aux.stack = terra::rast(aux.stack)
      product.raster = raster.template

      if (product == "MSW") {
        if(format == "profiles"){
          names(aux.stack) = paste0(st@name, "_profile",ind)
          final.stack = c(final.stack,aux.stack)

        }else{
          #Computing MSW analytic raster
          product.raster = max(aux.stack, na.rm = T)
          #Applying focal function twice to smooth results
          product.raster = terra::focal(
            product.raster,
            w = matrix(1, 3, 3),
            max,
            na.rm = T,
            pad = T
          )
          product.raster = terra::focal(
            product.raster,
            w = matrix(1, 3, 3),
            mean,
            na.rm = T,
            pad = T
          )
          names(product.raster) = paste0(st@name, "_", product)
          final.stack = c(final.stack, product.raster)
        }

      } else if (product == "PDI") {
        #Integrating over the whole track
        product.raster = sum(aux.stack, na.rm = T) * time_res
        #Applying focal function to smooth results
        product.raster = terra::focal(
          product.raster,
          w = matrix(1, 3, 3),
          sum,
          na.rm = T,
          pad = T
        )
        names(product.raster) = paste0(st@name, "_", product)
        final.stack = c(final.stack, product.raster)

      } else if (product == "Exposure") {
        #For each category in SSHS
        raster.c = c()
        for (i in c(1, 2, 3, 4, 0)) {
          ind = which(seq(1, terra::nlyr(aux.stack)) %% 5 == i)
          #Integrating over the whole track
          product.raster = sum(terra::subset(aux.stack, ind), na.rm = T) * time_res
          #Applying focal function to smooth results
          product.raster = terra::focal(
            product.raster,
            w = matrix(1, 3, 3),
            sum,
            na.rm = T,
            pad = T
          )
          if (i == 0)
            i = 5
          names(product.raster) = paste0(st@name, "_", product, i)
          final.stack = c(final.stack, product.raster)
          raster.c = c(raster.c, product.raster)
        }

        #Adding all categories
        raster.c = terra::rast(raster.c)
        raster.c = sum(raster.c, na.rm = T)
        names(raster.c) = paste0(st@name, "_", product,"all")
        final.stack = c(final.stack, raster.c)
      }

    }else{
      #Compute distances from the eye of storm for every observations x, and
      #every points y
      dist.m = terra::distance(
        x = cbind(dat$lon,dat$lat),
        y = cbind(format$lon, format$lat),
        lonlat = T
      )

      res = c()
      #For each point
      for(i in 1:dim(format)[1]){

        #Compute distance in deg between eye of storm and point P
        x = format$lon[i] - dat$lon
        y = format$lat[i] - dat$lat

        if(asymmetry == "V2"){
          msw = dat$msw[i] - dat$storm.speed[i]
        }else{
          msw = dat$msw[i]
        }

        if(empirical_rmw){
          rmw = getRmw(msw,dat$lat[i])
        }else{
          rmw = dat$rmw[i]
        }

        if (method == "Willoughby") {
           vr = Willoughby(
            msw = msw,
            lat = dat$lat[i],
            r = dist.m[,i] * 0.001,
            rmw = rmw
          )

        }else if (method == "Holland80") {
          vr = Holland80(
            r = dist.m[,i] * 0.001,
            rmw = rmw,
            msw = msw,
            pc = dat$pc[i] * 100,
            poci = dat$poci[i] * 100,
            lat = dat$lat[i]
          )
        }

        if (asymmetry == "V1") {
          #Boose version
          if(sts@basin %in% c("SA", "SP", "SI")){
            #Southern Hemisphere, t is counterclockwise
            t = (atan2(dat$vy.deg[i],dat$vx.deg[i])) - atan2(y,x) + pi
          }else{
            #Northern Hemisphere, t is clockwise
            t = (atan2(dat$vy.deg[i],dat$vx.deg[i])) - atan2(y,x) + pi
          }
          vr = vr - (1 - sin(t)) * dat$storm.speed[i]/2

        } else if(asymmetry == "V2"){
          if(sts@basin %in% c("SA", "SP", "SI")){
            #Southern Hemisphere, t is clockwise
            t = acos((y * dat$vx.deg[i] - x * dat$vy.deg[i]) / (sqrt(dat$vx.deg[i]**2 + dat$vy.deg[i]**2) * sqrt(x**2 + y**2)))
          }else{
            #Northern Hemisphere, t is counterclockwise
            t = acos((y * dat$vx.deg[i] - x * dat$vy.deg[i]) / (sqrt(dat$vx.deg[i]**2 + dat$vy.deg[i]**2) * sqrt(x**2 + y**2)))
          }
          vr = vr + cos(t) * dat$storm.speed[i]
        }

        if (product == "PDI") {
          #Computing surface drag coefficient
          cd = rasterizeCd(vr)

          rho = 0.001

          #Raising to power 3
          vr = vr ** 3

          #Applying both rho and surface drag coefficient
          vr = vr * rho * cd

          #Integrating over the whole track
          vr = sum(vr, na.rm = T) * 3

        }else if (product == "Exposure") {
          vr.c = c()
          for(c in 2:6){
            ind = which(vr >= sshs[c] & vr < sshs[c+1])
            vr.aux = rep(0,length(vr))
            vr.aux[ind] = 1
            vr.aux = sum(vr.aux, na.rm = T) * 3
            vr.c = c(vr.c,vr.aux)
          }
          # vr = vr.c
        }
        res = cbind(res,vr)
      }
    }
    s = s + 1
  }


  if(!identical(class(format),"data.frame")){
    final.stack = terra::rast(final.stack)
    if (focus_loi) {
      #Masking the stack to fit loi.buffer
      v = terra::vect(sts@spatial.loi.buffer)
      m = terra::rasterize(v, product.raster)
      final.stack = terra::mask(final.stack, m)
    }

    return(final.stack)

  }else{

    if(product == "MSW"){
      res = data.frame(res, row.names = ind)
    }else if(product == "PDI"){
      res = data.frame(res,row.names = "PDI")
    }else{
      res = data.frame(res,row.names = c("Cat.1", "Cat.2", "Cat.3", "Cat.4", "Cat.5"))
    }

    colnames(res) =paste0("(",format$lon,",",format$lat,")")

    return(res)
  }

}
