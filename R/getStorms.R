





#' Gather all the informations to model a single Storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot  sshs numeric. Category in the Saffir Simpson Hurricane Scale
#' @slot numobs.all numeric. Total number of observations available.
#' @slot obs.all  data.frame. Contains all of the observations available.
#' An observation is made up of several slots which are:
#' Basin, subbasin, iso.time, lon, lat, msw (Maximum Sustained Wind in), rmw (Radius of Maximum Wind),
#' roci (Radius of the Outermost Closed Isobar), pres (pressure at the center),
#' poci (Pressure of the Outermost Closed Isobar), sshs (Category in the Saffir Simpson Hurricane Scale),
#' landfall (Minimum distance to land over next 3 hours,  = 0 means landfall)).
#' @slot numobs numeric. Total number of observations available within the location of interest + buffer
#' @slot obs numerics. Indices of observations within the location of interest + buffer
#' @slot lty.track numeric. Indicates which line type is used to plot the storm
#' @return A S4 object gathering all the above informations
#' @importFrom methods new
#' @export
Storm <- methods::setClass(
  "Storm",
  slots = c(
    name = "character",
    season = "numeric",
    sshs = "numeric",
    numobs.all = "numeric",
    obs.all = "data.frame",
    obs = "numeric",
    numobs = "numeric",
    lty.track = "numeric"
  )
)



#' Gather all the informations to model a set of Storms
#'
#' @slot data A list of Storm object
#' @slot time.period numerics. Range of the cyclonic seasons of Storms available
#'  in `data`
#' @slot names characters. Names of Storms available in `data`
#' @slot  sshs numerics. Maximum category in the Saffir Simpson Hurricane Scale
#'  of all Storms available in `data`
#' @slot nb.storms nuleric. Total Number of Storms available in `data`
#' @slot basin  character. Basin in which the Storms have occured
#' @slot spatial.loi sf object. Represents the location of interest
#' Projection is EPSG:4326
#' @slot buffer numeric. Buffer used to extent `spatial.loi` (in km)
#' @slot spatial.loi.buffer buffer extension of `spatial.loi`
#' @return A S4 object gathering all the above informations
#' @importFrom methods new
#' @import sp
#' @export
Storms <- methods::setClass(
  "Storms",
  slots = c(
    data = "list",
    time.period = "numeric",
    names = "list",
    sshs = "list",
    nb.storms = "numeric",
    basin = "character",
    spatial.loi = "sf",
    buffer = "numeric",
    spatial.loi.buffer = "sf"
  )
)









#' Initialize a Storms object depending on a selection
#'
#' @param time_period numerics. Should be either one cyclonic season or a range
#' of cyclonic season. It could also be a vector of cyclonic season provided
#' that it has the same length as `name` and matches the season of each Storm
#' listed in `name`. Default value is set to c(1980, 2021)
#' @param name characters. Name(s) of Storm(s). Default value is set to NULL,
#' otherwise `time_period` and `name` must have the same length, and these two
#' informations must match
#' @param loi Location of Interest. Should be either a `SpatialPolygon`, a `sf`
#' object, a point of coordinates in lon/lat, a character representing a country,
#' or a basin. Default value is set to `"SP"` which will focus on the whole South
#' West Pacific Basin. Depending on the input, it will set up the `spatial.loi.`
#' slot.
#' @param max_dist numeric. Indicates the buffer used buffer to generate
#' `spatail.loi.buffer` (in km). Default value is set to 300
#' @param verbose logical. Whether or not the function must be verbose and display
#' a text progress bar. Default value is set to `FALSE`
#' @param remove_weak_TC logical. Whether or not to remove Stomrs under category
#' 1 in the Saffir Simpson Hurricane Scale. Default value is set to TRUE.
#'
#' @return a S4 Storms object that gathers all the above informations
#' @importFrom methods as
#' @export
getStorms <- function(time_period = c(1980, 2022),
                      name = NULL,
                      loi = "SP",
                      max_dist = 300,
                      verbose = FALSE,
                      remove_weak_TC = TRUE) {
  #Check time_period input
  stopifnot("time_period must be numeric" = identical(class(time_period), "numeric"))
  stopifnot("time_period must be as integers" = ds4psy::is_wholenumber(time_period))
  stopifnot("lower bound of time range is not valid" = time_period > 1979)
  stopifnot("upper bound of time range is not valid" = time_period < 2023)
  o = order(time_period)
  time_period = time_period[o]


  #Check name input
  if (!is.null(name)) {
    stopifnot("name must be a vector of characters" = identical(class(name), "character"))
    stopifnot("name and time_period must be the same length" = length(time_period) == length(name))
    name = name[o]
  } else{
    stopifnot(
      "time_period must be either length 1 or 2" = length(time_period) == 1 ||
        length(time_period) == 2
    )
  }


  #Check loi input
  loi.is.basin = FALSE
  if (!is.character(loi)) {
    #loi should be either SpatialPolygon, or sf or points coordinates
    if (identical(class(loi), c("SpatialPolygons"))) {
      loi.id = "SpatialPolygons"
    } else if (identical(class(loi), c("sf", "data.frame"))) {
      loi.id = "sf"
    } else if (identical(class(loi), c("numeric"))) {
      stopifnot(
        "loi must have valid lon/lat coordinates " = length(loi) == 2 &
          loi[1] >= 0 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90
      )
      loi.id = "Coordinates"
    } else{
      stop("invalid class for loi")
    }
  } else{
    #loi is a character that represents either a country or a basin
    stopifnot("loi must be length 1 " = length(loi) == 1)
    if (loi == "SP") {
      #Focus on South Pacific Ocean
      loi = sf::st_polygon(list(cbind(
        c(150, 150, 200, 200, 150),
        c(-30, -5, -5, -30, -30)
      )))
      loi = sf::st_sfc(loi, crs = 4326)
      loi = sf::st_as_sf(loi)
      loi.is.basin = TRUE
      loi.id = "sf"
    } else{
      map = rworldmap::getMap(resolution = "high")
      id.country = which(map@data$ADMIN == loi)
      stopifnot("invalid entry for loi" = length(id.country) > 0)
      loi.id = "Country"
    }
  }

  #Check max_dist input
  stopifnot("max_dist must be numeric " = identical(class(max_dist), "numeric"))
  stopifnot("max_dist must be a length 1 vector " = length(max_dist) == 1)

  #Check verbose input
  stopifnot("verbose must be logical" = identical(class(verbose), "logical"))

  #Check remove_weak_TC input
  stopifnot("verbose must be logical" = identical(class(remove_weak_TC), "logical"))


  #Open data_base
  ############################################################
  #-----The following 3lines must be eventually changed-----#
  ############################################################
  file_name = paste0("./inst/extdata/IBTrACS.SP.v04r00.nc")
  TC_data_base = ncdf4::nc_open(file_name)
  cyclonic_seasons = ncdf4::ncvar_get(TC_data_base, "season")


  if (verbose)
    cat("Identification of Storms: ")
  #Retrieving the matching indices, handling time_period and name
  if (!is.null(name)) {
    #we are interested in one or several storms
    storm.names = ncdf4::ncvar_get(TC_data_base, "name")
    indices = c()
    for (n in 1:length(name)) {
      seasons.id = which(cyclonic_seasons == time_period[n])
      storm.id = NULL
      storm.id = which(storm.names == name[n])
      stopifnot("Storm not found" = !is.null(storm.id))
      indices = c(indices, seasons.id[stats::na.omit(match(storm.id, seasons.id))[1]])
      stopifnot("Storm not found" = length(indices) > 0)
    }
  } else{
    if (length(time_period) == 1) {
      #we are interested in only one cyclonic season
      indices = which(cyclonic_seasons == time_period[1])
    } else{
      #we are interested in successive cyclonic seasons
      indices = seq(
        from = which(cyclonic_seasons == time_period[1])[1],
        to = max(which(cyclonic_seasons == time_period[2])),
        by = 1)
    }
  }

  if (verbose)
    cat("Done\n Make buffer: ")


  #Handle loi
  if (loi.id == "Coordinates") {
    loi.df   <- data.frame(pt = 1,
                           lon = loi[1],
                           lat = loi[2])
    loi.sf   <- sf::st_as_sf(loi.df, coords = c("lon", "lat"))
  } else if (loi.id == "SpatialPolygons") {
    loi.sf = sf::st_as_sf(loi)
  } else if (loi.id == "sf") {
    loi.sf = loi
    if (sf::st_crs(loi.sf) != 4326) {
      sf::st_transform(loi.sf, crs = 4326)
    }
  } else{
    #loi.id == "Country"
    loi.sf = sf::st_as_sf(sp::SpatialPolygons(list(map@polygons[[id.country]])))
  }

  #Handling time line for Fiji
  sf::st_crs(loi.sf) = 4326
  loi.sf = sf::st_shift_longitude(loi.sf)


  #Handle buffer
  if (!loi.is.basin) {
    loi.sf.buffer = sf::st_buffer(loi.sf, dist = max_dist * 1000)
    loi.sf.buffer = sf::st_shift_longitude(loi.sf.buffer)
  } else{
    loi.sf.buffer = loi.sf
  }

  if (verbose)
    cat("Done\n")





  #Get data associated with indices
  sts = Storms()
  sts@time.period = time_period
  sts@names = list()
  sts@nb.storms = 0
  sts@buffer = max_dist
  storm.list = list()
  k = 2 #init line type
  count = 0 #init count for progression bar


  if (verbose) {
    cat("Gathering storms\n")
    pb = utils::txtProgressBar(min = count,
                               max = length(indices),
                               style = 3)
  }


  for (i in indices) {
    #create sf points coordinates to intersect with loi.sf
    numobs = ncdf4::ncvar_get(TC_data_base, "numobs")[i]
    lon = ncdf4::ncvar_get(TC_data_base, "lon")[1:numobs, i]
    lat = ncdf4::ncvar_get(TC_data_base, "lat")[1:numobs, i]
    coords = data.frame(lon = lon, lat = lat)
    pts = sf::st_as_sf(coords, coords = c("lon", "lat"))
    sf::st_crs(pts) = 4326


    #which coordinates are within loi.sf.buffer
    ind = which(sf::st_intersects(pts, loi.sf.buffer,
                                  sparse = FALSE) == TRUE)

    #to check if storm is not NOT_NAMED
    name.storm = ncdf4::ncvar_get(TC_data_base, "name")[i]

    #to check if sshs is over categoy 1 in case remove_weak_TC == T
    sshs = ncdf4::ncvar_get(TC_data_base, "usa_sshs")[1:numobs, i]

    if (remove_weak_TC & max(sshs,na.rm = T) < 1) {
      #it is a TC below category 1, thus we do not consider it
      ind = NULL
    }

    #Add TC only if it intersect the LOI or it is not 'NOT_NAMED'
    if (length(ind) > 0 & name.storm != "NOT_NAMED") {
      sts@nb.storms = sts@nb.storms + 1

      storm = Storm()
      storm@name = name.storm
      storm@season = ncdf4::ncvar_get(TC_data_base, "season")[i]
      storm@numobs.all = numobs
      storm@obs.all = data.frame(
        subbasin = ncdf4::ncvar_get(TC_data_base, "subbasin")[1:numobs, i],
        iso.time = ncdf4::ncvar_get(TC_data_base, "iso_time")[1:numobs, i],
        lon = ncdf4::ncvar_get(TC_data_base, "usa_lon")[1:numobs, i],
        lat = ncdf4::ncvar_get(TC_data_base, "usa_lat")[1:numobs, i],
        msw = ncdf4::ncvar_get(TC_data_base, "usa_wind")[1:numobs, i] * 0.514,
        rmw = ncdf4::ncvar_get(TC_data_base, "usa_rmw")[1:numobs, i],
        roci = ncdf4::ncvar_get(TC_data_base, "usa_roci")[1:numobs, i],
        pres = ncdf4::ncvar_get(TC_data_base, "usa_pres")[1:numobs, i],
        poci = ncdf4::ncvar_get(TC_data_base, "usa_poci")[1:numobs, i],
        sshs = sshs,
        landfall = ncdf4::ncvar_get(TC_data_base, "landfall")[1:numobs, i]
      )

      #wrap longitudes -180/180 to 0/360
      lg = which(storm@obs.all$lon < 0)
      storm@obs.all$lon[lg] = storm@obs.all$lon[lg] + 360

      storm@obs = ind
      storm@numobs = length(ind)
      storm@lty.track = k
      storm@sshs = max(storm@obs.all$sshs,na.rm = T)
      storm.list = append(storm.list, storm)
      k = k + 1
      sts@names = append(sts@names, storm@name)
      sts@sshs = append(sts@sshs, storm@sshs)
    }

    if (verbose)
      utils::setTxtProgressBar(pb, count)

    count = count + 1
  }
  if (verbose)
    close(pb)

  ncdf4::nc_close(TC_data_base)


  sts@basin = "SP" #DO NOT FORGET TO CHANGE
  sts@spatial.loi = loi.sf
  sts@spatial.loi.buffer = loi.sf.buffer
  sts@data = storm.list
  names(sts@data) = sts@names

  return(sts)
}
