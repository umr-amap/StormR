
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





####################################
# Helpers to make template rasters #
####################################


#' Generate raster template for the computations
#'
#' @noRd
#' @param buffer sf object. LOI + buffer extention
#' @param res numeric. Space resolution min for the template
#'
#' @return a SpatRaster
makeTemplateRaster <- function(buffer, res) {
  # Deriving the raster template
  ext <- terra::ext(buffer)

  template <- terra::rast(
    extent = ext,
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
#' @param data data.frame. Subset of data generated with getInterpolatedData function at given index.
#'
#' @return SpatRaster
makeTemplateModel <- function(rasterTemplate, buffer, data) {
  template <- terra::rast(
    xmin = data$lon - buffer,
    xmax = data$lon + buffer,
    ymin = data$lat - buffer,
    ymax = data$lat + buffer,
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

  return(list(stormSpeed=stormSpeed, vxDeg=vxDeg, vyDeg=vyDeg))
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

  timeInit <- timeObs[1]
  timeEnd <- timeObs[lenIndices]
  timeInterpolated <- format(seq.POSIXt(as.POSIXct(timeInit),
                                        as.POSIXct(timeEnd),
                                        by = paste0(tempRes, " min")),
                             "%Y-%m-%d %H:%M:%S")
  timeInterpolatedNumeric <- as.numeric(as.POSIXct(timeInterpolated))
  lenInterpolated <- length(timeInterpolated)

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

  data$lon <- zoo::na.approx(st@obs.all$lon[indices],
                             as.numeric(as.POSIXct(timeObs)),
                             xout = timeInterpolatedNumeric,
                             rule = 2)
  data$lat <- zoo::na.approx(st@obs.all$lat[indices],
                             as.numeric(as.POSIXct(timeObs)),
                             xout = timeInterpolatedNumeric,
                             rule = 2)
  stormDisplacementSpeed <- stormDisplacement(data$lon, data$lat, tempRes)
  data$stormSpeed <- stormDisplacementSpeed$stormSpeed
  data$vxDeg <- stormDisplacementSpeed$vxDeg
  data$vyDeg <- stormDisplacementSpeed$vyDeg
  data$msw <- zoo::na.approx(st@obs.all$msw[indices],
                             as.numeric(as.POSIXct(timeObs)),
                             xout = timeInterpolatedNumeric,
                             rule = 2)

  if (empiricalRMW) {
    data$rmw <- getRmw(data$msw, data$lat)
  } else {
    if (!("rmw" %in% colnames(st@obs.all)) || (mean(is.na(st@obs.all$rmw[indices])) > 0.7)) {
      warning("Missing or too few non NA rmw data to perform model. empiricalRMW set to TRUE")
      data$rmw <- getRmw(data$msw, data$lat)
    } else {
      data$rmw <- zoo::na.approx(st@obs.all$rmw[indices],
                                 as.numeric(as.POSIXct(timeObs)),
                                 xout = timeInterpolatedNumeric,
                                 rule = 2)
    }
  }

  indicesInterpolated <- formatC(
    zoo::na.approx(indices,
                   as.numeric(as.POSIXct(timeObs)),
                   xout = timeInterpolatedNumeric),
      digits = 2,
      format = "f")
  data$indices <- gsub(".00", "", indicesInterpolated)
  data$isoTimes <- timeInterpolated

  if (method == "Holland" || method == "Boose") {
    if (all(is.na(st@obs.all$poci[indices])) || all(is.na(st@obs.all$pres[indices]))) {
      stop("Missing pressure data to perform Holland model")
    }

    data$poci <- zoo::na.approx(st@obs.all$poci[indices],
                                as.numeric(as.POSIXct(timeObs)),
                                xout = timeInterpolatedNumeric,
                                rule = 2)
    data$pc <- zoo::na.approx(st@obs.all$pres[indices],
                              as.numeric(as.POSIXct(timeObs)),
                              xout = timeInterpolatedNumeric,
                              rule = 2)
  }

  return(data)
}



###########################
# Helpers to stack products#
###########################


#' Project a computed layer localy onto a template raster
#'
#' @noRd
#' @param rasterTemplate SpatRaster. Raster template generated with
#'   makeTemplateRaster function. Reference grid.
#' @param rasterTemplateExtent SpatRaster. Extents of the template raster.
#' @param layer SpatRaster. Layer to extend to the template grid
#'
#' @return a SpatRaster
toRasterTemplate <- function(rasterTemplate, rasterTemplateExtent, layer, ...) {
   UseMethod("toRasterTemplate", layer)
}

toRasterTemplate.list <- function(rasterTemplate, rasterTemplateExtent, layer) {
  merged <- c()
  for (l in seq_along(layer)) {
    merged <- c(merged, toRasterTemplate(rasterTemplate, rasterTemplateExtent, layer[[l]]))
  }
  return(merged)
}

toRasterTemplate.SpatRaster <- function(rasterTemplate, rasterTemplateExtent, layer) {
  merged <- terra::merge(layer, rasterTemplate)
  merged <- terra::crop(merged, rasterTemplateExtent)

  return(merged)
}

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


smoothRaster <- function(rast, nbg, pad=TRUE) {
  return(terra::focal(rast, w = matrix(1, nbg, nbg), mean, na.rm = TRUE, pad = pad))
}


nameAndTimeRaster <- function(rast, name, product, timeRaster, threshold=NULL) {
  if (!is.null(threshold)) {
    names(rast) <- paste0(name, "_", product, "_", threshold)
  } else {
    names(rast) <- paste0(name, "_", product)
  }
  terra::time(rast) <- terra::time(timeRaster)
  return(rast)
}

rasterizeStormProduct <- function(product, stormRastersProduct, spaceRes, name, windThreshold) {
  nbg <- switch(spaceRes,
    "30sec" = 59,
    "2.5min" = 11,
    "5min" = 5,
    "10min" = 3
  )

  if (product == "MSW") {
    rast <- max(stormRastersProduct, na.rm = TRUE)
    rast <- smoothRaster(rast, nbg)
    rast <- nameAndTimeRaster(rast, name, product, stormRastersProduct[[1]])
  } else if (product == "PDI") {
    rast <- sum(stormRastersProduct, na.rm = TRUE)
    rast <- smoothRaster(rast, nbg)
    rast <- nameAndTimeRaster(rast, name, product, stormRastersProduct[[1]])
  } else if (product == "Exposure") {
    for (l in seq_along(windThreshold)) {
      ind <- seq(l, terra::nlyr(stormRastersProduct), length(windThreshold))
      rast <- sum(terra::subset(stormRastersProduct, ind), na.rm = TRUE)
      rast <- smoothRaster(rast, nbg, pad=FALSE)
      rast <- nameAndTimeRaster(rast, name, product, stormRastersProduct[[1]], windThreshold[l])
    }
  }

  return(rast)
}



#' Compute the products for a given storm at a given time step
#'
#' @noRd
#' @param stormRasters list of SpatRaster. Where to stack the computed products.
#' @param rasterTemplate SpatRaster. Raster template that need to be used for storing products computated.
#' @param rasterTemplateExtent SpatExtent. Extents of the template raster.
#' @param layerWind SpatRaster. Raster with one layer containing wind speeds.
#' @param layerDirection SpatRaster. Raster with one layer containing wind directions.
#' @param products list of character. Product(s) to compute.
#' @param name character. Name of the storm. Used to give the correct layer name.
#' @param indice numeric. Index of the time step.
#' @param tempRes numeric. Time resolution, used for the numerical integration
#' @param windThreshold numeric. Wind threshold used fior "Exposure" product computation.
#'
#' @return list of SpatRaster. Layers computed for the current time step are added at the end of the list.

computeProducts <- function(stormRasters, rasterTemplate, rasterTemplateExtent, layerWind, layerDirection, products, name, indice, tempRes, windThreshold) {
  if ("MSW" %in% products) {
    stormRasters$MSW <- c(
      stormRasters$MSW,
      toRasterTemplate(rasterTemplate, rasterTemplateExtent, layerWind)
    )
  }
  if ("PDI" %in% products) {
    layerPDI <- computePDI(layerWind, tempRes)
    stormRasters$PDI <- c(
      stormRasters$PDI,
      toRasterTemplate(rasterTemplate, rasterTemplateExtent, layerPDI)
    )
  }
  if ("Exposure" %in% products) {
    layersExposure <- computeExposure(layerWind, tempRes, windThreshold)
    stormRasters$EXP <- c(
      stormRasters$EXP,
      toRasterTemplate(rasterTemplate, rasterTemplateExtent, layersExposure)
    )
  }
  if ("Profiles" %in% products) {
    names(layerWind) <- paste0(name, "_", "Speed", "_", indice)
    names(layerDirection) <- paste0(name, "_", "Direction", "_", indice)
    stormRasters$Speed <- c(
      stormRasters$Speed,
      toRasterTemplate(rasterTemplate, rasterTemplateExtent, layerWind)
      )
    stormRasters$Direction <- c(
      stormRasters$Direction,
      toRasterTemplate(rasterTemplate, rasterTemplateExtent, layerDirection)
    )
  }

  return(stormRasters)

}

rasterizeStorm <- function(stormRasters, product, spaceRes, name, windThreshold = c(0)){
  raster <- list(MSW = NULL, PDI = NULL, EXP = NULL, Speed = NULL, Direction = NULL)
  if ("MSW" %in% product) {
    raster$MSW <- rasterizeStormProduct("MSW", terra::rast(stormRasters$MSW), spaceRes, name)
  }
  if ("PDI" %in% product) {
    raster$PDI <- rasterizeStormProduct("PDI", terra::rast(stormRasters$PDI), spaceRes, name)
  }
  if ("Exposure" %in% product) {
    raster$EXP <- rasterizeStormProduct("Exposure", terra::rast(stormRasters$EXP), spaceRes, name, windThreshold)
  }
  if ("Profiles" %in% product) {
    # In this case, only need to convert list of rasters to a multi-layers raster
    raster$Speed <- terra::rast(stormRasters$Speed)
    raster$Direction <- terra::rast(stormRasters$Direction)
  }

  return(raster)

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
  rasterTemplateExtent <- terra::ext(rasterTemplate)
  # Buffer size in degree
  buffer <- 2.5

  # Initializing final raster stacks
  rasters = list(MSW = c(), PDI = c(), EXP = c(), Speed = c(), Direction = c())

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

    # Initialize rasters product for current storm
    stormRasters <- list(MSW = c(), PDI = c(), EXP = c(), Speed = c(), Direction = c())

    for (j in 1:nbStep) {
      # Making template to compute wind profiles
      templateModel <- makeTemplateModel(rasterTemplate, buffer, lapply(dataTC, "[", j))
      layerSpeed <- templateModel
      layerDirection <- templateModel

      # Computing coordinates of raster
      crds <- terra::crds(layerSpeed, na.rm = FALSE)

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
        dataTC, j, method, asymmetry, x, y, crds, distEye, buffer,
        sts@spatialLoiBuffer, world, indCountries
      )

      terra::values(layerSpeed) <- output$speed
      terra::values(layerDirection) <- output$direction
      rm(output)

      # Compute products for current time step and stack them on same rasterTemplate
      stormRasters <- computeProducts(
          stormRasters, rasterTemplate, rasterTemplateExtent,
          layerSpeed, layerDirection, product, st@name,
          dataTC$indices[j], tempRes, windThreshold
          )

      if (verbose > 0) {
        utils::setTxtProgressBar(pb, step)
        step <- step + 1
      }
    }

    if (verbose > 0) {
      close(pb)
    }

    # Rasterize to produce final products for the current Storm
    rasters <- Map(c, rasters, rasterizeStorm(stormRasters, product, spaceRes, st@name, windThreshold))

    if (verbose > 0) {
      s <- s + 1
    }
  }

  finalStack <- c()

  if ("MSW" %in% product) {
    finalStack <- c(finalStack, rasterMSW)
  }
  if ("PDI" %in% product) {
    finalStack <- c(finalStack, rasterPDI)
  }
  if ("Exposure" %in% product) {
    finalStack <- c(finalStack, rasterEXP)
  }
  if ("Profiles" %in% product) {
    finalStack <- c(finalStack, rasterWind)
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
