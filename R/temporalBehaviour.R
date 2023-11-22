

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
      j <- 2 * (i - 1) + 1

      df <- data.frame(result[, j], result[, j:j + 1], indices = indices, isoTimes = isoT)
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
#' # Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
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
