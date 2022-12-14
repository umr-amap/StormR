




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
  stopifnot("time_period must be as integers" = ds4psy::is_wholenumber(time_period))
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

  #Checking verbose input
  stopifnot("verbose must be logical" = identical(class(verbose), "logical"))

  #Checking remove_TD input
  stopifnot("verbose must be logical" = identical(class(remove_TD), "logical"))

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
#' Default value is set to 300
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
  knt2ms = 0.514
  km = 1000
  wgs84 = 4326


  if (verbose)
    cat("Making buffer: ")

  #Handling loi
  if(is.null(loi)){
    loi = basin
    loi.is.basin = TRUE
    ext = terra::ext(Basins[basin,1], Basins[basin,2],
                     Basins[basin,3], Basins[basin,4])
    poly = cbind(c(ext$xmin, ext$xmax, ext$xmax, ext$xmin, ext$xmin),
                 c(ext$ymin, ext$ymin, ext$ymax, ext$ymax, ext$ymin))

    loi.sf = sf::st_polygon(list(poly))
    loi.sf = sf::st_sfc(loi.sf, crs = wgs84)
    loi.sf = sf::st_as_sf(loi.sf)
  } else{
    loi.is.basin = FALSE
    if (identical(class(loi), c("SpatialPolygons"))) {
      loi.id = "SpatialPolygons"
      loi.sf = sf::st_as_sf(loi)
      if (sf::st_crs(loi.sf) != wgs84) {
        sf::st_transform(loi.sf, crs = wgs84)
      }
    } else if (identical(class(loi), c("sf", "data.frame"))) {
      loi.sf = loi
    } else if (identical(class(loi), c("numeric"))){
      loi.df = data.frame(pt = 1,
                          lon = loi[1],
                          lat = loi[2])
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
        loi.sf = sf::st_sfc(loi.sf, crs = wgs84)
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
  sf::st_crs(loi.sf) = wgs84
  loi.sf = sf::st_shift_longitude(loi.sf)

  #Handling buffer
  if (!loi.is.basin) {
    loi.sf.buffer = sf::st_buffer(loi.sf, dist = max_dist * km)
    loi.sf.buffer = sf::st_shift_longitude(loi.sf.buffer)
  } else{
    loi.sf.buffer = loi.sf
  }


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
  msw = array(round(ncdf4::ncvar_get(TC.data.base, "usa_wind")[, indices] * knt2ms),
              dim = c(dim,length(indices)))
  rmw = array(ncdf4::ncvar_get(TC.data.base, "usa_rmw")[, indices],
              dim = c(dim,length(indices)))
  roci = array(ncdf4::ncvar_get(TC.data.base, "usa_roci")[, indices],
               dim = c(dim,length(indices)))
  pres = array(ncdf4::ncvar_get(TC.data.base, "usa_pres")[, indices],
               dim = c(dim,length(indices)))
  poci = array(ncdf4::ncvar_get(TC.data.base, "usa_poci")[, indices],
               dim = c(dim,length(indices)))
  sshs = array(sshs, dim = c(dim,length(indices)))


  #Initializing Storms object
  sts = Storms()
  sts@time.period = time_period
  sts.names = list()
  sts.sshs = list()
  sts@nb.storms = 0
  sts@buffer = max_dist

  storm.list = list()
  k = 2 #initializing line type
  count = 1 #initializing count for progression bar

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



      #Removing invalid iso_time
      iso.time = iso.times[1:numobs, i]
      list.iso.time = as.numeric(stringr::str_sub(iso.time,12,13))
      ind.iso.time = which(list.iso.time %% 3 == 0)
      coords = coords[ind.iso.time,]
      coords = coords[stats::complete.cases(coords),]
      row.names(coords) = seq(1,dim(coords)[1])



      #Creating sf point coordinates to intersect with loi.sf.buffer
      pts = sf::st_as_sf(coords, coords = c("lon", "lat"))
      sf::st_crs(pts) = wgs84

      if(!loi.is.basin){
        ind = which(sf::st_intersects(pts,
                                      loi.sf.buffer,
                                      sparse = FALSE) == TRUE)

      }else{
        ind = 1
      }


      #Add TC only if it intersects with loi.sf.buffer
      if (length(ind) > 0) {

        sts@nb.storms = sts@nb.storms + 1

        storm = Storm()
        storm@name = storm.names[i]
        storm@season = cyclonic.seasons[i]
        storm@basin = basins[i]
        storm@obs.all = data.frame(
          subbasin = subbasin[1:numobs, i],
          iso.time = iso.time,
          lon = lon,
          lat = lat,
          msw = zoo::na.approx(round(msw[1:numobs, i]), na.rm = F, rule = 2),
          rmw = zoo::na.approx(rmw[1:numobs, i], na.rm = F, rule = 2),
          roci = zoo::na.approx(roci[1:numobs, i], na.rm = F, rule = 2),
          pres = zoo::na.approx(pres[1:numobs, i], na.rm = F, rule = 2),
          poci = zoo::na.approx(poci[1:numobs, i], na.rm = F, rule = 2),
          sshs = sshs[1:numobs, i]
        )

        #Removing wrong approximation to clean data
        storm@obs.all$msw[is.na(storm@obs.all$lon)] = NA
        storm@obs.all$rmw[is.na(storm@obs.all$lon)] = NA
        storm@obs.all$roci[is.na(storm@obs.all$lon)] = NA
        storm@obs.all$poci[is.na(storm@obs.all$lon)] = NA
        storm@obs.all$pres[is.na(storm@obs.all$lon)] = NA

        #Wrapping longitudes from -180/180 to 0/360
        lg = which(storm@obs.all$lon < 0)
        storm@obs.all$lon[lg] = storm@obs.all$lon[lg] + 360

        #Removing invalid iso_time
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
        sts.names = append(sts.names, storm@name)
        sts.sshs = append(sts.sshs, storm@sshs)
      }


      if (verbose & length(indices) > 1)
        utils::setTxtProgressBar(pb, count)

      count = count + 1
    }

    if (verbose & length(indices) > 1)
      close(pb)

  }

  ncdf4::nc_close(TC.data.base)

  sts@names = unlist(sts.names)
  sts@sshs = unlist(sts.sshs)
  sts@basin = basin
  sts@loi.basin = loi.is.basin
  sts@spatial.loi = loi.sf
  sts@spatial.loi.buffer = loi.sf.buffer
  sts@data = storm.list
  names(sts@data) = sts@names

  return(sts)
}




