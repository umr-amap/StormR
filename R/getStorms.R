




#' Storm  object
#'
#' Gather all the needed informations to model a single storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot basin  character. Basin in which the storm has occured
#' @slot  sshs numeric. Maximum category reached in the Saffir Simpson Hurricane Scale
#' @slot numobs.all numeric. Total number of observations available.
#' @slot obs.all  data.frame. Contains all of the observations available.
#' An observation is made up of several slots which are:
#' \itemize{
#'   \item basin
#'   \item subbasin
#'   \item iso.time, Date and hours of observations (UTC)
#'   \item lon, Longitude coordinate (deg)
#'   \item lat, Latitude coordinate (deg)
#'   \item msw, Maximum Sustained Wind (m/s)
#'   \item rmw, Radius of Maximum Wind (km)
#'   \item roci, Radius of the Outermost Closed Isobar (km)
#'   \item pres, pressure at the center (mb)
#'   \item poci, Pressure of the Outermost Closed Isobar (mb)
#'   \item sshs, Category in the Saffir Simpson Hurricane Scale,
#' }
#' @slot numobs numeric. Total number of observations available within the location of interest
#' extented with its corresponding buffer (See class Storms)
#' @slot obs numeric vector. Indices of observations within the location of interest
#' extented with its corresponding buffer (See class Storms)
#' @slot lty.track numeric. Indicates which line type is used to plot the storm on a map
#' @returns A S4 object gathering all the following informations
#' @importFrom methods new
#' @export
Storm = methods::setClass(
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




setOldClass("sf")
#' Storms object
#'
#' Gather all the needed informations to model a set of storms
#'
#' @slot data A list of Storm objects
#' @slot time.period numeric vector. (Range of the) cyclonic seasons of Storms available
#'  in `data`
#' @slot names character vector. Names of Storms available in data
#' @slot  sshs numeric vector. Maximum category reached in the Saffir Simpson Hurricane Scale
#'  for all storms available in data
#' @slot nb.storms numeric. Total number of storms available in data
#' @slot basin  character. Basin in which the Storms have occured
#' @slot loi.basin logical. Whether the Location Of Interest represents the whole basin or not
#' @slot spatial.loi sf object. Represents the location of interest. Projection is EPSG:4326
#' @slot buffer numeric. Buffer used to extent spatial.loi (km)
#' @slot spatial.loi.buffer buffer extension of spatial.loi
#' @return A S4 object gathering all the following informations
#' @importFrom methods new
#' @import sp
#' @export
Storms = methods::setClass(
  "Storms",
  slots = c(
    data = "list",
    time.period = "numeric",
    names = "character",
    sshs = "numeric",
    nb.storms = "numeric",
    basin = "character",
    loi.basin = "logical",
    spatial.loi = "sf",
    buffer = "numeric",
    spatial.loi.buffer = "sf"
  )
)





#' Check inputs for getStorms function
#'
#' @noRd
#' @param basin character
#' @param time_period, numeric vector
#' @param name character vector
#' @param loi Either:
#'   \itemize{
#'     \item a SpatialPolygon (shapefile)
#'     \item a sf object
#'     \item a point of longitude/latitude coordinates
#'     \item a character representing a country,
#'     \item a character representing the basin
#'   }
#' @param max_dist numeric
#' @param verbose logical
#' @param remove_TD logical
#' @return NULL
checkInputsGs = function(basin, time_period, name, loi, max_dist, verbose, remove_TD){

  #Checking basin input
  stopifnot("Invalid basin input" = basin %in% c("SP", "SI", "SA", "NI", "WP", "EP", "NA", "ALL"))

  #Checking time_period input
  stopifnot("time_period must be numeric" = identical(class(time_period), "numeric"))
  stopifnot("time_period must be as integer" = all(round(time_period) == time_period))
  stopifnot("lower bound of time range is not valid" = time_period > 1979)
  stopifnot("upper bound of time range is not valid" = time_period < 2023)

  #Checking name input
  if (!is.null(name)) {
    stopifnot("name must be a vector of character" = identical(class(name), "character"))
    stopifnot("name and time_period must be the same length" = length(time_period) == length(name))
  } else{
    stopifnot(
      "Incompatible format for time_period (must be either length 1 or 2)" = length(time_period) == 1 ||
        length(time_period) == 2
    )
  }

  #Checking loi input
  if(!is.null(loi)){

    stopifnot("Invalid class for loi" = identical(class(loi), c("sf", "data.frame")) ||
                identical(class(loi), "SpatialPolygons") ||
                identical(class(loi), "numeric") ||
                identical(class(loi), "character"))


    if(identical(class(loi), "numeric"))
      stopifnot("loi must have valid lon/lat coordinates " = length(loi) == 2
                & loi[1] >= 0 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90)

    if(identical(class(loi), "character"))
      stopifnot("loi must be length 1 " = length(loi) == 1)

  }

  #Checking max_dist input
  stopifnot("max_dist must be numeric " = identical(class(max_dist), "numeric"))
  stopifnot("max_dist must be a length 1 vector " = length(max_dist) == 1)
  stopifnot("max_dist must > 0 " = max_dist > 0)

  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose), "logical"))

  #Checking remove_TD input
  stopifnot("verbose must be logical" = identical(class(remove_TD), "logical"))

}





#' Convert loi into a sf object
#'
#' @noRd
#' @param loi loi input from getStorms
#' @param basin basin input form getStorms
#' @param projection projection used for the sf object
#'
#' @return list with 2 slots : loi in a sf format and logical (Whether loi is whole basin or not)
convertLoi = function(loi, basin, projection){

  if(is.null(loi)){

    loi = basin
    loi.is.basin = TRUE
    ext = terra::ext(Basins[basin,1], Basins[basin,2],
                     Basins[basin,3], Basins[basin,4])
    poly = cbind(c(ext$xmin, ext$xmax, ext$xmax, ext$xmin, ext$xmin),
                 c(ext$ymin, ext$ymin, ext$ymax, ext$ymax, ext$ymin))

    loi.sf = sf::st_polygon(list(poly))
    loi.sf = sf::st_sfc(loi.sf, crs = projection)
    loi.sf = sf::st_as_sf(loi.sf)

  } else{

    loi.is.basin = FALSE
    if (identical(class(loi), c("SpatialPolygons"))) {
      loi.id = "SpatialPolygons"
      loi.sf = sf::st_as_sf(loi)
      if (sf::st_crs(loi.sf) != projection) {
        sf::st_transform(loi.sf, crs = projection)
      }

    } else if (identical(class(loi), c("sf", "data.frame"))) {

      loi.sf = loi

    } else if (identical(class(loi), c("numeric"))){

      loi.df = data.frame(lon = loi[1], lat = loi[2])
      loi.sf = sf::st_as_sf(loi.df, coords = c("lon", "lat"))

    } else if (identical(class(loi), c("character"))){

      if(loi %in% c("SP", "SI", "SA", "NI", "WP", "EP", "NA", "ALL")){
        loi = basin
        loi.is.basin = TRUE
        ext = terra::ext(Basins[basin,1], Basins[basin,2],
                         Basins[basin,3], Basins[basin,4])

        poly = cbind(c(ext$xmin, ext$xmax, ext$xmax, ext$xmin, ext$xmin),
                     c(ext$ymin, ext$ymin, ext$ymax, ext$ymax, ext$ymin))
        loi.sf = sf::st_polygon(list(poly))
        loi.sf = sf::st_sfc(loi.sf, crs = projection)
        loi.sf = sf::st_as_sf(loi.sf)

      }else{

        map = rworldmap::getMap(resolution = "high")
        id.country = which(map@data$ADMIN == loi)
        stopifnot("invalid entry for loi" = length(id.country) > 0)
        loi.sf = sf::st_as_sf(sp::SpatialPolygons(list(map@polygons[[id.country]])))

      }
    }
  }

  #Handling time line for Fiji
  sf::st_crs(loi.sf) = projection
  loi.sf = sf::st_shift_longitude(loi.sf)

  return(list(sf = loi.sf, basin = loi.is.basin))
}




#' make loi buffer
#'
#' @noRd
#' @param loi sf object. loi input from getStorms already convert into sf object
#' @param buffer numeric. Buffer size to use
#' @param is_basin logical. Whether the loi represents the whole basin. In this case
#' the loi is not extented.
#'
#' @return list with 2 slots : loi in a sf format and logical (Whether loi is whole basin or not)
makeBuffer = function(loi, buffer, is_basin){
  if (!is_basin) {
    loi.buffer = sf::st_buffer(loi, dist = buffer)
    loi.buffer = sf::st_shift_longitude(loi.buffer)
  } else{
    loi.buffer = loi
  }

  return(loi.buffer)
}




#' Retrieving the matching indices of storms
#'
#' @noRd
#' @param filter_names character vector. Contains name input from the getStorms inputs
#' @param filter_time_period numeric vector.  Contains time_period input from the getStorms inputs
#' @param filter_basin character. Contains basin input from the getStorms inputs
#' @param names character vector. All of the names of storms in the data base to filter
#' @param seasons numeric 1d array. All of the seasons of storms in the data base to filter
#' @param basins character vector. All of the basins of storms in the data base to filter
#'
#' @return indices of storms in the data base, that match the filter inputs
retrieveStorms = function(filter_names, filter_time_period, filter_basin, names, seasons, basins){

  if (!is.null(filter_names)) {
    #we are interested in one or several storms given by their name and season
    indices = c()
    for (n in 1:length(filter_names)) {
      seasons.id = which(seasons == filter_time_period[n])
      storm.id = NULL
      storm.id = which(names == filter_names[n])
      stopifnot("Storm not found" = !is.null(storm.id))
      id = seasons.id[stats::na.omit(match(storm.id, seasons.id))[1]]
      stopifnot("Storm not found" = !all(is.na(id)))
      indices = c(indices, id)
    }
  } else{
    if (length(filter_time_period) == 1) {
      #we are interested in only one cyclonic season
      indices = which(seasons == filter_time_period[1])
    } else{
      #we are interested in successive cyclonic seasons
      indices = seq(
        from = which(seasons == filter_time_period[1])[1],
        to = max(which(seasons == filter_time_period[2])),
        by = 1)
    }
  }

  #Filtering by basin
  if(filter_basin != "ALL")
    indices = indices[which(basins[indices] == filter_basin)]

  #Removing NOT_NAMED storms
  indices = indices[which(names[indices] != "NOT_NAMED")]

  return(indices)

}





#' Load data that match the indices from the database
#'
#' @noRd
#' @param TC_data_base data base
#' @param max_obs numeric. Number of maximum observation
#' @param indices numeric vector, indices in the database of TC to extract data
#' @param storm_names character vector, storm names previously loaded
#' @param seasons numeric vector, seasons previously loaded
#' @param basins character vector, basins previously loaded
#' @param sshs numeric vector, sshs previously loaded
#' @return A list of 14 slots
loadData = function(TC_data_base, max_obs, indices, storm_names, seasons, basins, sshs){

  return(list(stormNames  = storm_names[indices],
              seasons = seasons[indices],
              basins = basins[indices],
              numObservations = ncdf4::ncvar_get(TC_data_base, "numobs")[indices],
              subbasin = array(ncdf4::ncvar_get(TC_data_base, "subbasin")[, indices],
                               dim = c(max_obs,length(indices))),
              iso.times = array(ncdf4::ncvar_get(TC_data_base, "iso_time")[, indices],
                                dim = c(max_obs,length(indices))),
              longitude = array(ncdf4::ncvar_get(TC_data_base, "usa_lon")[, indices],
                                dim = c(max_obs,length(indices))),
              latitude = array(ncdf4::ncvar_get(TC_data_base, "usa_lat")[, indices],
                               dim = c(max_obs,length(indices))),
              msw = array(ncdf4::ncvar_get(TC_data_base, "usa_wind")[, indices],
                          dim = c(max_obs,length(indices))),
              rmw = array(ncdf4::ncvar_get(TC_data_base, "usa_rmw")[, indices],
                          dim = c(max_obs,length(indices))),
              roci = array(ncdf4::ncvar_get(TC_data_base, "usa_roci")[, indices],
                           dim = c(max_obs,length(indices))),
              pres = array(ncdf4::ncvar_get(TC_data_base, "usa_pres")[, indices],
                           dim = c(max_obs,length(indices))),
              poci = array(ncdf4::ncvar_get(TC_data_base, "usa_poci")[, indices],
                           dim = c(max_obs,length(indices))),
              sshs = array(sshs,
                           dim = c(max_obs,length(indices)))))

}





#' Write data to initialize a Storms object
#'
#' Whether or not to add storm at n° index in TC_data in the upcomoing Storm object
#'
#' @noRd
#' @param storm_list list of Storm object. To further integrate in a Storms object
#' @param storm_names list of storm name. To further integrate in a Storms object
#' @param storm_sshs list of storm sshs. To further integrate in a Storms object
#' @param nb_storms numeric. number of storm to already integrate in a Storms object
#' @param TC_data TC database
#' @param index numeric, index of the storm in the TC_data
#' @param loi_sf_buffer sf object. Location of interest extended with buffer
#' @param loi_is_basin logical. Whether loi is whole basin or not
#' @param k numeric. linetype
#' @param projection projection used for sf object
#'
#' @return a list with 3 slots:
#'   \itemize{
#'     \item list of storm objects
#'     \item list of character
#'     \item list of numeric
#'     \item numeric
#'   }
writeStorm = function(storm_list, storm_names, storm_sshs, nb_storms,
                       TC_data, index, loi_sf_buffer, loi_is_basin, k,
                       projection){
  knt2ms = 0.514

  #Getting number of observations
  numobs = TC_data$numObservations[index]

  #Getting lon/lat coordinates
  lon = TC_data$longitude[1:numobs, index]
  lat = TC_data$latitude[1:numobs, index]
  coords = data.frame(lon = lon, lat = lat)

  #Keep only non NA data (that are either the first or last observations)
  valid_indices = which(!is.na(coords$lon))
  coords = coords[valid_indices,]

  #Removing invalid iso_time
  iso.time = TC_data$iso.times[valid_indices, index]
  list.iso.time = as.numeric(stringr::str_sub(iso.time,12,13))
  #Keep only 03H 06H 09H 12H 15H 18h 21H 00h iso times
  ind.iso.time = which(list.iso.time %% 3 == 0)
  coords = coords[ind.iso.time,]
  row.names(coords) = seq(1,dim(coords)[1])
  #print(coords)

  #Creating sf point coordinates to intersect with loi_sf_buffer
  pts = sf::st_as_sf(coords, coords = c("lon", "lat"))
  sf::st_crs(pts) = projection

  if(!loi_is_basin){
    ind = which(sf::st_intersects(pts, loi_sf_buffer, sparse = FALSE) == TRUE)
  }else{
    ind = 1
  }

  #Add TC only if it intersects with loi_sf_buffer
  if (length(ind) > 0) {

    nb_storms = nb_storms + 1

    storm = Storm()
    storm@name = TC_data$stormNames[index]
    storm@season = TC_data$seasons[index]
    storm@basin = TC_data$basins[index]
    storm@obs.all = data.frame(
      subbasin = TC_data$subbasin[valid_indices, index],
      iso.time = iso.time,
      lon = lon[valid_indices],
      lat = lat[valid_indices],
      msw = zoo::na.approx(round(TC_data$msw[valid_indices, index] * knt2ms), na.rm = F, rule = 2),
      rmw = zoo::na.approx(TC_data$rmw[valid_indices, index], na.rm = F, rule = 2),
      roci = zoo::na.approx(TC_data$roci[valid_indices, index], na.rm = F, rule = 2),
      pres = zoo::na.approx(TC_data$pres[valid_indices, index], na.rm = F, rule = 2),
      poci = zoo::na.approx(TC_data$poci[valid_indices, index], na.rm = F, rule = 2),
      sshs = TC_data$sshs[valid_indices, index]
    )


    #Removing wrong approximation to clean data
    # storm@obs.all$msw[is.na(storm@obs.all$lon)] = NA
    # storm@obs.all$rmw[is.na(storm@obs.all$lon)] = NA
    # storm@obs.all$roci[is.na(storm@obs.all$lon)] = NA
    # storm@obs.all$poci[is.na(storm@obs.all$lon)] = NA
    # storm@obs.all$pres[is.na(storm@obs.all$lon)] = NA


    #Wrapping longitudes from -180/180 to 0/360
    lg = which(storm@obs.all$lon < 0)
    storm@obs.all$lon[lg] = storm@obs.all$lon[lg] + 360

    #Removing invalid iso_time from obs.all
    storm@obs.all = storm@obs.all[ind.iso.time,]
    storm@numobs.all = dim(storm@obs.all)[1]
    row.names(storm@obs.all) = seq(1,storm@numobs.all)


    if(!loi_is_basin){
      #ind = as.numeric(row.names(storm@obs.all[stats::complete.cases(storm@obs.all$lon),])[ind])
      ind = ind
    }else{
      ind = seq(1,storm@numobs.all)
    }

    storm@obs = ind
    storm@numobs = length(ind)
    storm@lty.track = k
    storm@sshs = max(storm@obs.all$sshs,na.rm = T)


    return(list(append(storm_list, storm),
                append(storm_names, storm@name),
                append(storm_sshs, storm@sshs),
                nb_storms))

  }else{

    return(list(NULL,NULL,NULL,NULL))

  }

}





#' Initialize a Storms object
#'
#' This function returns a Storms object that
#' gathers all the storms the user is interested in
#'
#' @param basin character. Name of basin where the storms should be extracted.
#' Default value is set to "SP"
#' @param time_period numeric vector. Should be either one cyclonic season or a range
#' of cyclonic seasons. It could also be a vector of cyclonic season provided
#' that it has the same length as name input and matches the season of each storm
#' listed in name input. Default value is set to c(1980, 2021)
#' @param name character vector. Name(s) of storm(s). Default value is set to NULL,
#' otherwise time_period and name must have the same length, and these two informations must match
#' @param loi Location of Interest. Should be either:
#' \itemize{
#' \item a SpatialPolygon (shapefile)
#' \item a sf object
#' \item a point of longitude/latitude coordinates
#' \item a character representing a country,
#' \item a character representing the basin
#' }
#  Default value is set to NULL which will set the spatial.loi.buffer on the whole basin
#' @param max_dist numeric. Indicates the buffer used to generate spatial.loi.buffer (km).
#' Default value is set to 300. This value also represents the maximum distance from the track
#' of the storm where computations should be performed afterwards.
#' @param verbose logical. Whether or not the function must be verbose and display
#' a text progress bar. Default value is set to FALSE
#' @param remove_TD logical. Whether or not to remove Tropical Depression (< 18 m/s).
#' Default value is set to TRUE.
#'
#' @returns a Storms object that gathers all storms that match the criteria given
#' in the inputs
#'
#' @examples
#' #Focus on a single storm
#' pam = getStorms(time_period = 2015, name = "PAM", loi = "Vanuatu")
#'
#' #Focus on several storms over Vanuatu
#' sts_nc = getStorms(time_period = c(2003,2021), name = c("ERICA","NIRAN"), loi = "New Caledonia")
#'
#' #Focus on every storms that occured in the WP basin between 2010 and 2020
#' sts_wp = getStorms(basin = "WP", time_period = c(2010,2020), verbose = TRUE)
#'
#' @importFrom methods as
#' @export
getStorms = function(basin = "SP", time_period = c(1980, 2022), name = NULL, loi = NULL,
                     max_dist = 300, verbose = FALSE, remove_TD = TRUE){


  checkInputsGs(basin,time_period,name,loi,max_dist,verbose,remove_TD)

  o = order(time_period)
  time_period = time_period[o]
  if (!is.null(name))
    name = name[o]


  #Internal variables
  km = 1000
  wgs84 = 4326

  if (verbose)
    cat("Making buffer: ")

  #Converting loi
  args = convertLoi(loi, basin, wgs84)
  loi.sf = args$sf
  loi.is.basin = args$basin

  #Handling buffer
  loi.sf.buffer = makeBuffer(loi.sf, max_dist * km, loi.is.basin)

  if (verbose)
    cat("Done\nIdentifying Storms: ")

  #Open data_base
  filename = paste0("IBTrACS.ALL.v04r00.nc")
  TC.data.base = ncdf4::nc_open(system.file("extdata", filename, package = "StormR"))


  #Retrieving the matching indices, handling name, time_period and basin
  storm.names = ncdf4::ncvar_get(TC.data.base, "name")
  cyclonic.seasons = ncdf4::ncvar_get(TC.data.base, "season")
  basins = ncdf4::ncvar_get(TC.data.base, "basin")[1,]
  sshs = ncdf4::ncvar_get(TC.data.base, "usa_sshs")
  dim = dim(sshs)[1]

  indices = retrieveStorms(filter_names = name,
                           filter_time_period = time_period,
                           filter_basin = basin,
                           names = storm.names,
                           seasons = cyclonic.seasons,
                           basins = basins)


  #Removing TD if remove_TD == T
  sshs = array(sshs[,indices], dim = c(dim,length(indices)))

  if(remove_TD){
    i = which(apply(sshs,2,max, na.rm = T) >= 0)
    indices = indices[i]
    sshs = sshs[,i]
  }


  if (verbose)
    cat("Done\nLoading data: ")

  #Getting remaining data associated with indices
  TC.data = loadData(TC.data.base, dim, indices, storm.names, cyclonic.seasons, basins, sshs)
  ncdf4::nc_close(TC.data.base)


  if(verbose)
    cat("Done\n")


  if (verbose & length(indices) > 1) {
    cat("Gathering storms \n")
    count = 1 #initializing count for progression bar
    pb = utils::txtProgressBar(min = count,
                               max = length(indices),
                               style = 3)
  }

  if(length(indices) > 0){

    storm.list = list()
    storm.names = list()
    storm.sshs = list()
    nb.storms = 0
    k = 2 #initializing line type

    for (i in 1:length(indices)) {

      sts.output = writeStorm(storm_list = storm.list,
                               storm_names = storm.names,
                               storm_sshs = storm.sshs,
                               nb_storms = nb.storms,
                               TC_data = TC.data,
                               index = i,
                               loi_sf_buffer = loi.sf.buffer,
                               loi_is_basin = loi.is.basin,
                               k = k,
                               projection = wgs84)

      if(!is.null(sts.output[[1]])){
        storm.list = sts.output[[1]]
        storm.names = sts.output[[2]]
        storm.sshs = sts.output[[3]]
        nb.storms = sts.output[[4]]
      }


      if (verbose & length(indices) > 1){
        utils::setTxtProgressBar(pb, count)
        count = count + 1
      }

      k = k + 1
    }

    if (verbose & length(indices) > 1)
      close(pb)

    #Initializing Storms object
    sts = Storms()
    sts@time.period = time_period
    sts@nb.storms = nb.storms
    sts@buffer = max_dist
    sts@names = unlist(storm.names)
    sts@sshs = unlist(storm.sshs)
    sts@basin = basin
    sts@loi.basin = loi.is.basin
    sts@spatial.loi = loi.sf
    sts@spatial.loi.buffer = loi.sf.buffer
    sts@data = storm.list
    names(sts@data) = sts@names

    return(sts)
  }

}




