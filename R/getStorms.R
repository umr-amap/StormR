




#' Storm  object
#'
#' Gather all the needed informations to model a single storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot  sshs numeric. Maximum category reached in the Saffir Simpson Hurricane Scale
#' @slot numobs.all numeric. Total number of observations available.
#' @slot obs.all  data.frame. Contains all of the observations available.
#' An observation is made up of several slots which are:
#' \itemize{
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

#Getter for Storm class
setGeneric("getName", function(st) standardGeneric("getName"))
setMethod("getName", signature("Storm"), function(st) st@name)

setGeneric("getSeason", function(st) standardGeneric("getSeason"))
setMethod("getSeason", signature("Storm"), function(st) st@season)

setGeneric("getsshs", function(st) standardGeneric("getsshs"))
setMethod("getsshs", signature("Storm"), function(st) st@sshs)

setGeneric("getNbObs", function(st) standardGeneric("getNbObs"))
setMethod("getNbObs", signature("Storm"), function(st) st@numobs.all)

setGeneric("getObs", function(st) standardGeneric("getObs"))
setMethod("getObs", signature("Storm"), function(st) st@obs.all)

setGeneric("getInObs", function(st) standardGeneric("getInObs"))
setMethod("getInObs", signature("Storm"), function(st) st@obs)




setOldClass("sf")
#' Storms object
#'
#' Gather all the needed informations to model a set of storms
#'
#' @slot data A list of Storm objects
#' @slot seasons numeric vector. (Range of the) cyclonic seasons of Storms available
#'  in `data`
#' @slot names character vector. Names of Storms available in data
#' @slot  sshs numeric vector. Maximum category reached in the Saffir Simpson Hurricane Scale
#'  for all storms available in data
#' @slot nb.storms numeric. Total number of storms available in data
#' @slot spatial.loi sf object. Represents the location of interest. Projection is EPSG:4326
#' @slot buffer numeric. Buffer used to extent spatial.loi (km)
#' @slot spatial.loi.buffer buffer extension of spatial.loi
#' @return A S4 object gathering all the following informations
#' @importFrom methods new
#' @import sp
#' @export
Storms <- methods::setClass(
  "Storms",
  slots = c(
    data = "list",
    seasons = "numeric",
    names = "character",
    sshs = "numeric",
    nb.storms = "numeric",
    spatial.loi = "sf",
    buffer = "numeric",
    spatial.loi.buffer = "sf"
  )
)


#Getters for Storms class

setGeneric("getStorm", function(sts, name) standardGeneric("getStorm") )
setMethod("getStorm", signature("Storms"), function(sts, name) sts@data[[name]])

setGeneric("getSeasons", function(sts, name = NULL) standardGeneric("getSeasons") )
setMethod("getSeasons", signature("Storms"), function(sts, name = NULL){
  if(is.null(name)){
    sts@seasons
  }else{
    sts@seasons[which(sts@names == name)]
  }
})

setGeneric("getNames", function(sts) standardGeneric("getNames"))
setMethod("getNames", signature("Storms"), function(sts) sts@names)

setGeneric("getSSHS", function(sts, name = NULL) standardGeneric("getSSHS"))
setMethod("getSSHS", signature("Storms"), function(sts, name = NULL){
  if(is.null(name)){
    sts@sshs
  }else{
    sts@sshs[which(sts@names == name)]
  }
})

setGeneric("getNbStorms", function(sts) standardGeneric("getNbStorms"))
setMethod("getNbStorms", signature("Storms"), function(sts) sts@nb.storms)

setGeneric("getLOI", function(sts) standardGeneric("getLOI"))
setMethod("getLOI", signature("Storms"), function(sts) sts@spatial.loi)

setGeneric("getBuffer", function(sts) standardGeneric("getBuffer"))
setMethod("getBuffer", signature("Storms"), function(sts) sts@spatial.loi.buffer)

setGeneric("getBufferSize", function(sts) standardGeneric("getBufferSize"))
setMethod("getBufferSize", signature("Storms"), function(sts) sts@buffer)


#Getters for Storms class to access information from Storm class provided in data

setGeneric("getStormNbObs", function(sts, name) standardGeneric("getStormNbObs"))
setMethod("getStormNbObs", signature("Storms"), function(sts, name) getNbObs(getStorm(sts,name)))

setGeneric("getStormObs", function(sts, name) standardGeneric("getStormObs"))
setMethod("getStormObs", signature("Storms"), function(sts, name) getObs(getStorm(sts,name)))

setGeneric("getStormInObs", function(sts, name) standardGeneric("getStormInObs"))
setMethod("getStormInObs", signature("Storms"), function(sts, name) getInObs(getStorm(sts,name)))






#' Check inputs for getStorms function
#'
#' @noRd
#' @param sdb StormDatabase object
#' @param loi Either:
#'   \itemize{
#'     \item a SpatialPolygon (shapefile)
#'     \item a sf object
#'     \item a point of longitude/latitude coordinates
#'     \item a character representing a country,
#'   }
#' @param seasons, numeric vector
#' @param names character vector
#' @param max_dist numeric
#' @param verbose logical
#' @param remove_TD logical
#' @return NULL
checkInputsGs <- function(sdb, loi, seasons, names, max_dist, verbose, remove_TD){

  #checking sdb input
  stopifnot("sdb is missing" = !missing(sdb))

  #Checking loi input
  if(!missing(loi)){

    stopifnot("Invalid class for loi" = identical(class(loi), c("sf", "data.frame")) ||
                identical(class(loi)[1], "SpatialPolygonsDataFrame") ||
                identical(class(loi), "numeric") ||
                identical(class(loi), "character"))


    if(identical(class(loi), "numeric"))
      stopifnot("loi must have valid lon/lat coordinates " = length(loi) == 2
                & loi[1] >= 0 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90)

    if(identical(class(loi), "character"))
      stopifnot("loi must be length 1 " = length(loi) == 1)

  }else{
    stop("loi is missing")
  }


  #Checking seasons input
  stopifnot("seasons must be numeric" = identical(class(seasons), "numeric"))
  stopifnot("seasons must be as integer" = all(round(seasons) == seasons))
  stopifnot("lower bound of time range is not valid" = seasons > 1979)
  stopifnot("upper bound of time range is not valid" = seasons < 2023)

  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be a vector of character" = identical(class(names), "character"))
    stopifnot("names and seasons must be the same length" = length(seasons) == length(names))
  } else{
    stopifnot(
      "Incompatible format for seasons (must be either length 1 or 2)" = length(seasons) == 1 ||
        length(seasons) == 2
    )
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
#'
#' @return loi in a sf format
convertLoi <- function(loi){

  if (identical(class(loi)[1], c("SpatialPolygonsDataFrame"))) {

    loi.sf <- sf::st_as_sf(loi)
    loi.sf = sf::st_geometry(loi.sf)
    loi.sf = sf::st_sf(loi.sf)
    loi.sf <- sf::st_as_sf(loi.sf)
    if (sf::st_crs(loi.sf) != wgs84) {
      loi.sf = sf::st_transform(loi.sf, crs = wgs84)
    }

  } else if (identical(class(loi), c("sf", "data.frame"))) {

    loi.sf = sf::st_geometry(loi)
    loi.sf = sf::st_sf(loi.sf)
    loi.sf <- sf::st_as_sf(loi.sf)
    if (sf::st_crs(loi.sf) != wgs84) {
      loi.sf <- sf::st_transform(loi.sf, crs = wgs84)
    }

  } else if (identical(class(loi), c("numeric"))){

    loi.df <- data.frame(lon = loi[1], lat = loi[2])
    loi.sf <- sf::st_as_sf(loi.df, coords = c("lon", "lat"))
    sf::st_crs(loi.sf) = wgs84
    loi.sf <- sf::st_transform(loi.sf, crs = wgs84)

  } else if (identical(class(loi), c("character"))){

    if(loi %in% c("SP", "SI", "SA", "NI", "WP", "EP", "NA", "ALL")){
      ext <- terra::ext(Basins[loi,1], Basins[loi,2],
                        Basins[loi,3], Basins[loi,4])

      poly <- cbind(c(ext$xmin, ext$xmax, ext$xmax, ext$xmin, ext$xmin),
                    c(ext$ymin, ext$ymin, ext$ymax, ext$ymax, ext$ymin))
      loi.sf <- sf::st_polygon(list(poly))
      loi.sf <- sf::st_sfc(loi.sf, crs = wgs84)
      loi.sf <- sf::st_as_sf(loi.sf)

    }else{

      map <- rworldmap::getMap(resolution <- "high")
      id.country <- which(map@data$ADMIN == loi)
      stopifnot("invalid entry for loi" = length(id.country) > 0)
      loi.sf <- sf::st_as_sf(sp::SpatialPolygons(list(map@polygons[[id.country]])))
      sf::st_crs(loi.sf) = wgs84
    }
  }
  #Handling time line for Fiji
  loi.sf <- sf::st_shift_longitude(loi.sf)

  return(loi.sf)
}




#' make loi buffer
#'
#' @noRd
#' @param loi sf object. loi input from getStorms already convert into sf object
#' @param buffer numeric. Buffer size to use
#'
#' @return loi extended with buffer in a sf format
makeBuffer <- function(loi, buffer){

    loi.buffer <- sf::st_buffer(loi, dist = buffer)
    loi.buffer <- sf::st_shift_longitude(loi.buffer)
    return(loi.buffer)
}





#' Retrieving the matching indices of storms
#'
#' @noRd
#' @param sdb ...
#' @param filter_names character vector. Contains names input from the getStorms inputs
#' @param filter_seasons numeric vector.  Contains seasons input from the getStorms inputs
#' @param remove_TD logical. Whether or not to remove tropical depressions (< 18 m/s)
#' and include cyclones only. Default value is set to TRUE.
#'
#' @return indices of storms in the data base, that match the filter inputs
retrieveStorms <- function(sdb, filter_names, filter_seasons, remove_TD){

  if (!is.null(filter_names)) {
    #We are interested in one or several storms given by their name and season
    indices <- c()
    for (n in 1:length(filter_names)) {
      seasons.id <- which(sdb$seasons == filter_seasons[n])
      storm.id <- NULL
      storm.id <- which(sdb$names == filter_names[n])
      stopifnot("Storm not found, invalid name ?" = !is.null(storm.id))

      id <- seasons.id[match(storm.id, seasons.id)[!is.na(match(storm.id, seasons.id))]]
      stopifnot("Storm not found, seasons and names do not match" = !all(is.na(id)))
      indices <- c(indices, id)
    }
  } else{

    if (length(filter_seasons) == 1) {
      #We are interested in only one cyclonic season
      indices <- which(sdb$seasons == filter_seasons)
    } else{
      #We are interested in successive cyclonic seasons
      indices <- seq(
        from = which(sdb$seasons == filter_seasons[1])[1],
        to = max(which(sdb$seasons == filter_seasons[2])),
        by = 1)
    }
  }

  #Removing NOT_NAMED storms
  indices <- indices[which(sdb$names[indices] != "NOT_NAMED")]

  #Removing TD if remove_TD == T

  if(remove_TD){
    i <- which(apply(array(sdb$sshs[,indices], dim = c(dim(sdb$sshs)[1], length(indices))),2,max, na.rm = T) >= 0)
    indices <- indices[i]
  }

  return(indices)

}





#' Write data to initialize a Storms object
#'
#' Whether or not to add storm at n° index in TC_data in the upcomoing Storm object
#'
#' @noRd
#' @param storm_list list of Storm object. To further integrate in a Storms object
#' @param storm_names list of storm names. To further integrate in a Storms object
#' @param storm_sshs list of storm sshs. To further integrate in a Storms object
#' @param nb_storms numeric. number of storm to already integrate in a Storms object
#' @param TC_data TC database
#' @param index numeric, index of the storm in the TC_data
#' @param loi_sf_buffer sf object. Location of interest extended with buffer
#' @param k numeric. linetype
#'
#' @return a list with 3 slots:
#'   \itemize{
#'     \item list of storm objects
#'     \item list of character
#'     \item list of numeric
#'     \item numeric
#'   }
writeStorm <- function(storm_list, storm_names, storm_sshs, nb_storms,
                       TC_data, index, loi_sf_buffer, k){

  #Getting number of observations
  numobs <- TC_data$numobs[index]

  #Getting lon/lat coordinates
  lon <- TC_data$longitude[1:numobs, index]
  lat <- TC_data$latitude[1:numobs, index]
  coords <- data.frame(lon = lon, lat = lat)

  #Keep only non NA data (that are either the first or last observations)
  valid_indices <- which(!is.na(coords$lon))
  coords <- coords[valid_indices,]

  #Removing invalid iso_time
  isotime <- TC_data$isotimes[valid_indices, index]
  list.isotime <- as.numeric(stringr::str_sub(isotime,12,13))
  #Keep only 03H 06H 09H 12H 15H 18h 21H 00h iso times
  ind.isotime <- which(list.isotime %% 3 == 0)
  coords <- coords[ind.isotime,]
  row.names(coords) <- seq(1,dim(coords)[1])

  #Creating sf point coordinates to intersect with loi_sf_buffer
  pts <- sf::st_as_sf(coords, coords = c("lon", "lat"))
  sf::st_crs(pts) <- wgs84

  #Intersect points coordinates with loi_sf_buffer
  ind <- which(sf::st_intersects(pts, loi_sf_buffer, sparse <- FALSE) == TRUE)


  #Add TC only if it intersects with loi_sf_buffer
  if (length(ind) > 0) {

    nb_storms <- nb_storms + 1

    storm <- Storm()
    storm@name <- TC_data$names[index]
    storm@season <- TC_data$seasons[index]
    storm@obs.all <- data.frame(
      iso.time = isotime,
      lon = lon[valid_indices],
      lat = lat[valid_indices],
      msw = zoo::na.approx(round(TC_data$msw[valid_indices, index] * knt2ms), na.rm = F, rule = 2),
      rmw = zoo::na.approx(TC_data$rmw[valid_indices, index], na.rm = F, rule = 2),
      roci = zoo::na.approx(TC_data$roci[valid_indices, index], na.rm = F, rule = 2),
      pres = zoo::na.approx(TC_data$pres[valid_indices, index], na.rm = F, rule = 2),
      poci = zoo::na.approx(TC_data$poci[valid_indices, index], na.rm = F, rule = 2),
      sshs = TC_data$sshs[valid_indices, index]
    )

    #Wrapping longitudes from -180/180 to 0/360
    lg <- which(storm@obs.all$lon < 0)
    storm@obs.all$lon[lg] <- storm@obs.all$lon[lg] + 360

    #Removing invalid isotime from obs.all
    storm@obs.all <- storm@obs.all[ind.isotime,]
    storm@numobs.all <- dim(storm@obs.all)[1]
    row.names(storm@obs.all) <- seq(1,storm@numobs.all)

    storm@obs <- ind
    storm@numobs <- length(ind)
    storm@lty.track <- k
    storm@sshs <- max(storm@obs.all$sshs,na.rm = T)


    return(list(append(storm_list, storm),
                append(storm_names, storm@name),
                append(storm_sshs, storm@sshs),
                nb_storms))

  }else{

    return(list(NULL,NULL,NULL,NULL))

  }

}





fun <- function(index, sdb, loi.sf.buffer){
  #print(sdb$numobs[index])
  lon = sdb$longitude[,index]
  lat = sdb$latitude[,index]
  coords <- data.frame(lon = lon, lat = lat)
  coords <- coords[!is.na(coords$lon),]
  #print(coords)
  pts <- sf::st_as_sf(coords, coords = c("lon", "lat"))
  sf::st_crs(pts) <- wgs84
  return(any(sf::st_intersects(pts, loi.sf.buffer, sparse = FALSE)))
}




#' Initialize a Storms object
#'
#' This function returns a Storms object that
#' gathers all the storms specified by the user
#'
#' @param sdb list. TC data base loaded from a stormDatabase object
#' @param loi Location of Interest. Must be either:
#' \itemize{
#' \item a SpatialPolygon (shapefile)
#' \item a sf object
#' \item a point of longitude/latitude coordinates provided in a numeric vector
#' \item a character representing a country,
#' }
#' @param seasons numeric vector. Should be either one or a range of calendar
#' years. For cyclones that formed in one year and dissipated in the following
#' year, the latter should be used. It could also be a vector of cyclonic season provided
#' that it has the same length as names input and matches the season of each storm
#' listed in names input. Default value is set to c(1980, 2021)
#' @param names character vector. Name(s) of storm(s). Default value is set to NULL,
#' otherwise seasons and names must have the same length, and must match with
#' a storm on the IBTrACS database
#' @param max_dist numeric. Indicates the buffer distance (in km) used to generate
#' spatial.loi.buffer. Default value is set to 300km. This value also represents
#' the maximum distance from the track of the storm where computations can be performed
#' @param verbose logical. Whether or not the function must be verbose and display
#' a text progress bar. Default value is set to FALSE
#' @param remove_TD logical. Whether or not to remove tropical depressions (< 18 m/s)
#' and include cyclones only. Default value is set to TRUE.
#'
#' @returns a Storms object that gathers all storms that match the criteria given
#' in the inputs
#'
#' @examples
#' #Focus on a single storm
#' pam <- getStorms(seasons = 2015, names = "PAM", loi = "Vanuatu")
#'
#' #Focus on several storms over Vanuatu
#' sts_nc <- getStorms(seasons = c(2003,2021), names = c("ERICA","NIRAN"), loi = "New Caledonia")
#'
#' #Focus on every storms that occured in the WP basin between 2010 and 2020
#' poly <- cbind(c(100, 180, 180, 100, 100),c(0, 0, 60, 60, 0))
#' wp <- sf::st_polygon(list(poly))
#' wp <- sf::st_sfc(wp, crs = 4326)
#' wp <- sf::st_as_sf(wp)
#' sts_wp <- getStorms(loi = wp, seasons = c(2010,2020), verbose = TRUE)
#'
#' @importFrom methods as
#' @export
getStorms <- function(sdb = IBTRACS, loi, seasons = c(1980, 2022), names = NULL,
                      max_dist = 300, verbose = FALSE, remove_TD = TRUE){

  start_time <- Sys.time()

  checkInputsGs(sdb, loi, seasons, names, max_dist, verbose, remove_TD)

  o <- order(seasons)
  seasons <- seasons[o]
  if (!is.null(names))
    names <- names[o]

  if (verbose)
    cat("Making buffer: ")

  #Converting loi
  loi.sf <- convertLoi(loi)

  #Handling buffer
  loi.sf.buffer <- makeBuffer(loi.sf, max_dist * km)

  if (verbose)
    cat("Done\nIdentifying Storms: ")


  #Retrieving the matching indices, handling names, seasons and remove TD

  indices <- retrieveStorms(sdb,
                            filter_names = names,
                            filter_seasons = seasons,
                            remove_TD = remove_TD)


  # ind <- lapply(X = indices, FUN = fun, sdb = sdb, loi.sf.buffer = loi.sf.buffer)
  #print(ind)
  #Intersect points coordinates with loi_sf_buffer
  #ind <- which(sf::st_intersects(pts, loi_sf_buffer, sparse = FALSE) == TRUE)


  if (verbose & length(indices) > 1) {
    cat("Done\nGathering storms \n")
    count <- 1 #initializing count for progression bar
    pb <- utils::txtProgressBar(min = count,
                               max = length(indices),
                               style = 3)
  }

  if(length(indices) > 0){

    storm.list <- list()
    storm.names <- list()
    storm.sshs <- list()
    nb.storms <- 0
    k <- 2 #initializing line type

    for (i in indices) {
      sts.output <- writeStorm(storm_list = storm.list,
                               storm_names = storm.names,
                               storm_sshs = storm.sshs,
                               nb_storms = nb.storms,
                               TC_data = sdb,
                               index = i,
                               loi_sf_buffer = loi.sf.buffer,
                               k = k)

      if(!is.null(sts.output[[1]])){
        storm.list <- sts.output[[1]]
        storm.names <- sts.output[[2]]
        storm.sshs <- sts.output[[3]]
        nb.storms <- sts.output[[4]]
      }


      if (verbose & length(indices) > 1){
        utils::setTxtProgressBar(pb, count)
        count <- count + 1
      }

      k <- k + 1
    }

    if (verbose & length(indices) > 1)
      close(pb)

    stopifnot("No storms found. Please check compatibilities between inputs" = !is.null(unlist(storm.names)))

    #Initializing Storms object
    sts <- Storms()
    sts@seasons <- seasons
    sts@nb.storms <- nb.storms
    sts@buffer <- max_dist
    sts@names <- unlist(storm.names)
    sts@sshs <- unlist(storm.sshs)
    sts@spatial.loi <- loi.sf
    sts@spatial.loi.buffer <- loi.sf.buffer
    sts@data <- storm.list
    names(sts@data) <- sts@names

    end_time <- Sys.time()
    print(end_time - start_time)

    return(sts)

  }else{

    stop("No storms found")
  }


}
