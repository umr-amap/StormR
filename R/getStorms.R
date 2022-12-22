





#' Gather all the informations to model a single Storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot basin  character. Basin in which the storm has occured
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
    basin = "character",
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
#' @param basin character. Name of basin where the Storms should be extracted.
#' Default value is set to `"SP"`
#' @param time_period numerics. Should be either one cyclonic season or a range
#' of cyclonic season. It could also be a vector of cyclonic season provided
#' that it has the same length as `name` and matches the season of each Storm
#' listed in `name`. Default value is set to c(1980, 2021)
#' @param name characters. Name(s) of Storm(s). Default value is set to NULL,
#' otherwise `time_period` and `name` must have the same length, and these two
#' informations must match
#' @param loi Location of Interest. Should be either a `SpatialPolygon`, a `sf`
#' object, a point of coordinates in lon/lat, a character representing a country,
#' or a basin. Default value is set to `NULL` which will set the `spatial.loi.`
#' on the whole `basin`
#' @param max_dist numeric. Indicates the buffer used buffer to generate
#' `spatail.loi.buffer` (in km). Default value is set to 300
#' @param verbose logical. Whether or not the function must be verbose and display
#' a text progress bar. Default value is set to `FALSE`
#' @param remove_TD logical. Whether or not to remove Tropical Depression (< 18 m/s).
#' Default value is set to TRUE.
#'
#' @return a S4 Storms object that gathers all the above informations
#' @importFrom methods as
#' @export
getStorms <- function(basin = "SP",
                      time_period = c(1980, 2022),
                      name = NULL,
                      loi = NULL,
                      max_dist = 300,
                      verbose = FALSE,
                      remove_TD = TRUE) {


  #Check basin input
  stopifnot("Invalid basin input" = basin %in% c("SP", "SI", "SA", "NI", "WP", "EP", "NA", "ALL"))

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
  if(is.null(loi)){
    loi = basin
    loi.is.basin = TRUE

    ext = switch(basin,
                 "SP" = cbind(
                   c(135, 290, 290, 135, 135),
                   c(0, 0, -60, -60, 0)
                   ),
                 "SI" = cbind(
                   c(10, 135, 135, 10, 10),
                   c(0, 0, -60, -60, 0)
                   ),
                 "SA" = cbind(
                   c(290, 359, 359, 290, 290),
                   c(0, 0, -60, -60, 0)
                 ),
                 "NI" = cbind(
                   c(30, 100, 100, 30, 30),
                   c(30, 30, 0, 0, 30)
                 ),
                 "WP" = cbind(
                   c(100, 180, 180, 100, 100),
                   c(60, 60, 0, 0, 60)
                 ),
                 "EP" = cbind(
                   c(180, 290, 290, 180, 180),
                   c(60, 60, 0, 0, 60)
                 ),
                 "NA" = cbind(
                   c(270, 359, 359, 270, 270),
                   c(60, 60, 0, 0, 60)
                 ),
                 "ALL" = cbind(
                   c(0, 359, 359, 0, 0),
                   c(60, 60, -60, -60, 60)
                 )

    )

    loi = sf::st_polygon(list(ext))
    loi = sf::st_sfc(loi, crs = 4326)
    loi = sf::st_as_sf(loi)
    loi.id = "sf"
  } else{
    loi.is.basin = FALSE
    if (!is.character(loi)) {
      #loi should be either SpatialPolygon, or sf or points coordinates
      if (identical(class(loi), c("SpatialPolygons"))) {
        loi.id = "SpatialPolygons"
      } else if (identical(class(loi), c("sf", "data.frame"))) {
        loi.id = "sf"
      } else if (identical(class(loi), c("numeric"))){
        stopifnot(
          "loi must have valid lon/lat coordinates " = length(loi) == 2 &
            loi[1] >= 0 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90
        )
        loi.id = "Coordinates"
      } else{
        stop("invalid class for loi")
      }
    } else{
      #loi is a character that represents a country
      stopifnot("loi must be length 1 " = length(loi) == 1)
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

  #Check remove_TD input
  stopifnot("verbose must be logical" = identical(class(remove_TD), "logical"))

  #Open data_base
  filename = paste0("IBTrACS.ALL.v04r00.nc")
  TC.data.base = ncdf4::nc_open(system.file("extdata", filename, package = "StormR"))


  if (verbose)
    cat("Identifying of Storms: ")


  #Retrieving the matching indices, handling time_period and name
  cyclonic.seasons = ncdf4::ncvar_get(TC.data.base, "season")
  basins = ncdf4::ncvar_get(TC.data.base, "basin")
  if (!is.null(name)) {
    #we are interested in one or several storms
    storm.names = ncdf4::ncvar_get(TC.data.base, "name")
    indices = c()
    for (n in 1:length(name)) {
      seasons.id = which(cyclonic.seasons == time_period[n])
      storm.id = NULL
      storm.id = which(storm.names == name[n])
      stopifnot("Storm not found" = !is.null(storm.id))
      indices = c(indices, seasons.id[stats::na.omit(match(storm.id, seasons.id))[1]])
      stopifnot("Storm not found" = length(indices) > 0)
    }
  } else{
    if (length(time_period) == 1) {
      #we are interested in only one cyclonic season
      indices = which(cyclonic.seasons == time_period[1])
    } else{
      #we are interested in successive cyclonic seasons
      indices = seq(
        from = which(cyclonic.seasons == time_period[1])[1],
        to = max(which(cyclonic.seasons == time_period[2])),
        by = 1)
    }
  }

  #Filter by basin
  if(basin != "ALL")
    indices = indices[which(basins[1,indices] == basin)]

  #Remove NOT_NAMED storms
  storm.names = ncdf4::ncvar_get(TC.data.base, "name")
  indices = indices[which(storm.names[indices] != "NOT_NAMED")]

  #Remove TD id remove_TD == T
  sshs = ncdf4::ncvar_get(TC.data.base, "usa_sshs")
  dim = dim(sshs)[1]
  sshs = array(sshs[,indices], dim = c(dim,length(indices)))

  if(remove_TD)
      indices = indices[which(apply(sshs,2,max, na.rm = T) >= 0)]



  if (verbose)
    cat("Done\nLoading data: ")


  #Get remaining data associated with indices
  storm.names = storm.names[indices]
  num.observations = ncdf4::ncvar_get(TC.data.base, "numobs")[indices]
  basins = basins[indices]
  subbasin = array(ncdf4::ncvar_get(TC.data.base, "subbasin")[, indices],
                   dim = c(dim,length(indices)))
  cyclonic.seasons = cyclonic.seasons[indices]
  iso.times = array(ncdf4::ncvar_get(TC.data.base, "iso_time")[, indices],
                    dim = c(dim,length(indices)))
  longitude = array(ncdf4::ncvar_get(TC.data.base, "usa_lon")[, indices],
                    dim = c(dim,length(indices)))
  latitude = array(ncdf4::ncvar_get(TC.data.base, "usa_lat")[, indices],
                   dim = c(dim,length(indices)))
  msw = array(round(ncdf4::ncvar_get(TC.data.base, "usa_wind")[, indices] * 0.514),
              dim = c(dim,length(indices)))
  rmw = array(ncdf4::ncvar_get(TC.data.base, "usa_rmw")[, indices],
              dim = c(dim,length(indices)))
  roci = array(ncdf4::ncvar_get(TC.data.base, "usa_roci")[, indices],
               dim = c(dim,length(indices)))
  pres = array(ncdf4::ncvar_get(TC.data.base, "usa_pres")[, indices],
               dim = c(dim,length(indices)))
  poci = array(ncdf4::ncvar_get(TC.data.base, "usa_poci")[, indices],
               dim = c(dim,length(indices)))
  sshs = sshs
  landfall = array(ncdf4::ncvar_get(TC.data.base, "landfall")[, indices],
                   dim = c(dim,length(indices)))


  if (verbose)
    cat("Done\nMaking buffer: ")


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
    max_dist = 0
  }

  sts = Storms()
  sts@time.period = time_period
  sts@names = list()
  sts@nb.storms = 0
  sts@buffer = max_dist

  storm.list = list()
  k = 2 #init line type
  count = 1 #init count for progression bar

  if(verbose)
    cat("Done\n")

  if (verbose & length(indices) > 1) {
    cat("Gathering storms \n")
    pb = utils::txtProgressBar(min = count,
                               max = length(indices),
                               style = 3)
  }

  if(length(indices) > 0){

    for (i in 1:length(indices)) {


      numobs = num.observations[i]
      lon = longitude[1:numobs, i]
      lat = latitude[1:numobs, i]
      coords = data.frame(lon = lon, lat = lat)

      #Remove invalid iso_time
      iso.time = iso.times[1:numobs, i]
      list.iso.time = as.numeric(stringr::str_sub(iso.time,12,13))
      ind.iso.time = which(list.iso.time %% 3 == 0)
      coords = coords[ind.iso.time,]
      row.names(coords) = seq(1,dim(coords)[1])
      coords = coords[stats::complete.cases(coords),]


      #create sf points coordinates to intersect with loi.sf
      pts = sf::st_as_sf(coords, coords = c("lon", "lat"))
      sf::st_crs(pts) = 4326


      #which coordinates are within loi.sf.buffer
      if(!loi.is.basin){
        ind = which(sf::st_intersects(pts, loi.sf.buffer,
                                      sparse = FALSE) == TRUE)
      }else{
        ind = 1
      }




      #Add TC only if it intersects the LOI
      if (length(ind) > 0) {
        sts@nb.storms = sts@nb.storms + 1

        storm = Storm()
        storm@name = storm.names[i]
        storm@season =cyclonic.seasons[i]
        storm@basin = basins[i]
        storm@obs.all = data.frame(
          subbasin = subbasin[1:numobs, i],
          iso.time = iso.time,
          lon = lon,
          lat = lat,
          msw = round(msw[1:numobs, i]),
          rmw = rmw[1:numobs, i],
          roci = roci[1:numobs, i],
          pres = pres[1:numobs, i],
          poci = poci[1:numobs, i],
          sshs = sshs[1:numobs, i],
          landfall = landfall[1:numobs, i]
        )

        #wrap longitudes -180/180 to 0/360
        lg = which(storm@obs.all$lon < 0)
        storm@obs.all$lon[lg] = storm@obs.all$lon[lg] + 360

        #Remove invalid iso_time
        storm@obs.all = storm@obs.all[ind.iso.time,]
        storm@numobs.all = dim(storm@obs.all)[1]
        row.names(storm@obs.all) = seq(1,storm@numobs.all)
        if(!loi.is.basin){
          ind = as.numeric(row.names(storm@obs.all[stats::complete.cases(storm@obs.all),])[ind])
        }else{
          ind = seq(1,storm@numobs.all)
        }


        storm@obs = ind
        storm@numobs = length(ind)
        storm@lty.track = k
        storm@sshs = max(storm@obs.all$sshs,na.rm = T)
        storm.list = append(storm.list, storm)
        k = k + 1
        sts@names = append(sts@names, storm@name)
        sts@sshs = append(sts@sshs, storm@sshs)
      }


      if (verbose & length(indices) > 1)
        utils::setTxtProgressBar(pb, count)

      count = count + 1
    }
    if (verbose & length(indices) > 1)
      close(pb)

  }

  ncdf4::nc_close(TC.data.base)

  sts@basin = basin
  sts@spatial.loi = loi.sf
  sts@spatial.loi.buffer = loi.sf.buffer
  sts@data = storm.list
  names(sts@data) = sts@names

  return(sts)
}




