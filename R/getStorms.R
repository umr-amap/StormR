





#' Storm  object
#'
#' Gather all the needed informations to model a single storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot  sshs numeric. Maximum category reached in the Saffir Simpson Hurricane Scale
#' @slot numobs.all numeric. Total number of observations available
#' @slot numobs numeric. Total number of observations available within the location of interest
#' extented with its corresponding buffer (See Storms class)
#' @slot obs numeric vector. Indices of observations within the location of interest
#' extented with its corresponding buffer (See Storms class)
#' @slot obs.all  data.frame. Contains all of the observations available.
#' An observation is made up of several fields which are:
#' \itemize{
#'   \item iso.time, Date and hours of observations (UTC)
#'   \item lon, Longitude coordinate (deg)
#'   \item lat, Latitude coordinate (deg)
#'   \item msw, Maximum Sustained Wind (m/s)
#'   \item sshs, Category in the Saffir Simpson Hurricane Scale
#'  }
#'  The following field is not mandatory but highly recommand
#'  \itemize{
#'   \item rmw, Radius of Maximum Wind (km)
#'  }
#' Also, the following fields are only mandatory to perform Holland model (See Details)
#' \itemize{
#'   \item pres, Pressure at the center (mb)
#'   \item poci, Pressure of the Outermost Closed Isobar (mb)
#' }
#' @importFrom methods new
#' @export
Storm <- methods::setClass(
  "Storm",
  slots = c(
    name = "character",
    season = "numeric",
    sshs = "numeric",
    numobs.all = "numeric",
    numobs = "numeric",
    obs = "numeric",
    obs.all = "data.frame"
  )
)

#Getter for Storm class

#' getStorm_name
#'
#' Getter to access name of the storm
#'
#' @param st Storm object
#'
#' @return character. Name of the storm
#' @export
setGeneric("getStorm_name", function(st) standardGeneric("getStorm_name"))
setMethod("getStorm_name", signature("Storm"), function(st) st@name)

#' getStorm_season
#'
#' @param st Storm object
#'
#' @return numeric. Cyclonic season of the storm
#' @export
setGeneric("getStorm_season", function(st) standardGeneric("getStorm_season"))
setMethod("getStorm_season", signature("Storm"), function(st) st@season)


#' getStorm_sshs
#'
#' @param st Storm object
#'
#' @return numeric. Maximum catergory reached in the Saffir Simpson Hurricane Scale
#' @export
setGeneric("getStorm_sshs", function(st) standardGeneric("getStorm_sshs"))
setMethod("getStorm_sshs", signature("Storm"), function(st) st@sshs)

#' getStorm_nbObs
#'
#' @param st Storm object
#'
#' @return numeric. Number of observations available within the Storm object
#' @export
setGeneric("getStorm_nbObs", function(st) standardGeneric("getStorm_nbObs"))
setMethod("getStorm_nbObs", signature("Storm"), function(st) st@numobs.all)

#' getStorm_obs
#'
#' @param st Storm object
#'
#' @return data.frame. Observations for a given Storm object
#' @export
setGeneric("getStorm_obs", function(st) standardGeneric("getStorm_obs"))
setMethod("getStorm_obs", signature("Storm"), function(st) st@obs.all)

#' getStorm_inObs
#'
#' @param st Storm object
#'
#' @return numeric. Indices within the extented LOI for a given Storm object
#' @export
setGeneric("getStorm_inObs", function(st) standardGeneric("getStorm_inObs"))
setMethod("getStorm_inObs", signature("Storm"), function(st) st@obs)




setOldClass("sf")
#' Storms object
#'
#' Gather all the needed informations to model a set of storms
#'
#' @slot data A list of Storm objects (See Storm class)
#' @slot nb.storms numeric. Total number of storms available in data
#' @slot names character vector. Names of Storms available in data
#' @slot seasons numeric vector. (Range of the) cyclonic seasons of Storms available
#'  in `data`
#' @slot  sshs numeric vector. Maximum category reached in the Saffir Simpson Hurricane Scale
#'  for all storms available in data
#' @slot buffer numeric. Buffer used to extent spatial.loi (km)
#' @slot spatial.loi sf object. Represents the location of interest. Projection is EPSG:4326
#' @slot spatial.loi.buffer sf object. Buffer extension of spatial.loi
#' @importFrom methods new
#' @import sp
#' @export
Storms <- methods::setClass(
  "Storms",
  slots = c(
    data = "list",
    nb.storms = "numeric",
    names = "character",
    seasons = "numeric",
    sshs = "numeric",
    buffer = "numeric",
    spatial.loi = "sf",
    spatial.loi.buffer = "sf"
  )
)


#Getters for Storms class

#' getStorm
#'
#' @param sts Storms object
#' @param name character. Name of the storm to extract
#' @param season numeric. Cyclonic season of the storm to extract. Use only if several
#' storms in the Storms object share the same name. Default value is set de NULL
#'
#' @return Storm object
#' @export
setGeneric("getStorm", function(sts, name, season = NULL) standardGeneric("getStorm") )
setMethod("getStorm", signature("Storms"), function(sts, name, season = NULL){
  if(!is.null(season)){
    w = which(sts@names == name)
    for(i in 1:length(w)){
      if(getStorm_season(sts@data[[w[i]]]) == season){
        j = w[i]
        break
      }
    }
    sts@data[[j]]
  }else{
    if(length(which(sts@names == name)) > 1)
      stop(paste("More than 1 storm named",name, ".Please specify season\n"))
    sts@data[[which(sts@names == name)]]
  }
})

#' getSeasons
#'
#' @param sts Storms object
#' @param name character. Name(s) of the storm(s). Default value is set to NULL
#'
#' @return numeric vector. Cyclonic seasons of each storms (or the selected one
#' through names input)provided by a given Storms object
#' @export
setGeneric("getSeasons", function(sts, name = NULL) standardGeneric("getSeasons") )
setMethod("getSeasons", signature("Storms"), function(sts, name = NULL){
  if(is.null(name)){
    sts@seasons
  }else{
    sts@seasons[which(sts@names == name)]
  }
})

#' getNames
#'
#' @param sts Storms object
#'
#' @return character vector. Names of each storms provided by a given Storms object
#' @export
setGeneric("getNames", function(sts) standardGeneric("getNames"))
setMethod("getNames", signature("Storms"), function(sts) sts@names)

#' getSSHS
#'
#' @param sts Storms object
#' @param name character. Name(s) of the storm(s). Default value is set to NULL
#'
#' @return numeric vector. Cyclonic seasons of each storms (or the selected one
#' through names input) provided by a given Storms object
#' @export
setGeneric("getSSHS", function(sts, name = NULL) standardGeneric("getSSHS"))
setMethod("getSSHS", signature("Storms"), function(sts, name = NULL){
  if(is.null(name)){
    sts@sshs
  }else{
    sts@sshs[which(sts@names == name)]
  }
})

#' getNbStorms
#'
#' @param sts Storms object
#'
#' @return numeric. Number of storms available in a given Storms object
#' @export
setGeneric("getNbStorms", function(sts) standardGeneric("getNbStorms"))
setMethod("getNbStorms", signature("Storms"), function(sts) sts@nb.storms)

#' getLOI
#'
#' @param sts Storms object
#'
#' @return sf object. Location of interest for a given Storms object
#' @export
setGeneric("getLOI", function(sts) standardGeneric("getLOI"))
setMethod("getLOI", signature("Storms"), function(sts) sts@spatial.loi)

#' getBuffer
#'
#' @param sts Storms object
#'
#' @return sf object. Extented location of interest for a given Storms object
#' @export
setGeneric("getBuffer", function(sts) standardGeneric("getBuffer"))
setMethod("getBuffer", signature("Storms"), function(sts) sts@spatial.loi.buffer)

#' getBufferSize
#'
#' @param sts Storms object
#'
#' @return numeric. Buffer size (in km) used to generate the extended LOI for a given Storms object
#' @export
setGeneric("getBufferSize", function(sts) standardGeneric("getBufferSize"))
setMethod("getBufferSize", signature("Storms"), function(sts) sts@buffer)


#Getters for Storms class to access information from Storm class provided in data

#' getNbobs
#'
#' @param sts  Storms object
#' @param name character. Name of the storm to extract
#' @param season numeric. Cyclonic season of the storm to extract. Use only if several
#' storms in the Storms object share the same name. Default value is set de NULL
#'
#' @return numeric. Number of observations available within the given storm
#' @export
setGeneric("getNbObs", function(sts, name, season = NULL) standardGeneric("getNbObs"))
setMethod("getNbObs", signature("Storms"), function(sts, name, season = NULL) getStorm_nbObs(getStorm(sts, name, season)))

#' getObs
#'
#' @param sts  Storms object
#' @param name character. Name of the storm to extract
#' @param season numeric. Cyclonic season of the storm to extract. Use only if several
#' storms in the Storms object share the same name. Default value is set de NULL
#'
#' @return data.frame. Observations of the given storm
#' @export
setGeneric("getObs", function(sts, name, season = NULL) standardGeneric("getObs"))
setMethod("getObs", signature("Storms"), function(sts, name, season = NULL) getStorm_obs(getStorm(sts, name, season)))

#' getInObs
#'
#' @param sts  Storms object
#' @param name character. Name of the storm to extract
#' @param season numeric. Cyclonic season of the storm to extract. Use only if several
#' storms in the Storms object share the same name. Default value is set de NULL
#'
#' @return numeric. Indices within the extented LOI for the given Storm object
#' @export
setGeneric("getInObs", function(sts, name, season = NULL) standardGeneric("getInObs"))
setMethod("getInObs", signature("Storms"), function(sts, name, season = NULL) getStorm_inObs(getStorm(sts, name, season)))






#' Check inputs for getStorms function
#'
#' @noRd
#' @param sds StormsDataset object
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
checkInputsGs <- function(sds, loi, seasons, names, max_dist, verbose, remove_TD){

  #checking sds input
  stopifnot("sds is missing" = !missing(sds))

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
  stopifnot("lower bound of time range is not valid" = seasons >= 1980)
  stopifnot("upper bound of time range is not valid" = seasons <= max(sds@database$seasons, na.rm = T))

  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be a vector of character" = identical(class(names), "character"))
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
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1, 2 or 3" = verbose %in% c(0, 1, 2, 3))

  #Checking remove_TD input
  stopifnot("remove_TD must be logical" = identical(class(remove_TD), "logical"))

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

    map <- rworldmap::getMap(resolution = "high")
    id.country <- which(map@data$ADMIN == loi)
    stopifnot("invalid entry for loi" = length(id.country) > 0)
    loi.sf <- sf::st_as_sf(sp::SpatialPolygons(list(map@polygons[[id.country]])))
    sf::st_crs(loi.sf) = wgs84

  }
  #Handling time line for Fiji
  loi.sf <- sf::st_shift_longitude(loi.sf)

  return(loi.sf)
}





#' make loi buffer
#'
#' @noRd
#' @param loi sf object. loi input from getStorms already converted into sf object
#' @param buffer numeric. Buffer size to use (in km)
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
#' @param database Storms database
#' @param filter_names character vector. Contains names input from the getStorms
#' @param filter_seasons numeric vector.  Contains seasons input from the getStorms
#' @param remove_TD logical. Whether or not to remove tropical depressions (< 18 m/s)
#' and include cyclones only. Default value is set to TRUE.
#'
#' @return indices of storms in the database, that match the filter inputs
retrieveStorms <- function(database, filter_names, filter_seasons, remove_TD){

  if (length(filter_seasons) == 1) {
    #We are interested in only one cyclonic season
    indices <- which(database$seasons == filter_seasons)
  } else{
    #We are interested in successive cyclonic seasons
    indices <- seq(from = which(database$seasons == filter_seasons[1])[1],
                   to = max(which(database$seasons == filter_seasons[2])))
  }

  if (!is.null(filter_names)) {
    #We are interested in one or several storms given by their name (and season)
    ind <- c()
    for (n in 1:length(filter_names)) {
      id <- NULL
      id <- which(database$names == filter_names[n])
      stopifnot("Storm not found, invalid name ?" = !is.null(id))
      ind <- c(ind, id)
    }

    indices = intersect(indices,ind)
    stopifnot("No storm(s) found "= !is.null(indices))



  }

  #Removing NOT_NAMED storms
  indices <- indices[which(database$names[indices] != "NOT_NAMED")]

  #Removing TD if remove_TD == T
  if(remove_TD){
    suppressWarnings({
      suppressWarnings(i <- which(apply(array(database$msw[,indices], dim = c(dim(database$msw)[1], length(indices))),2, max, na.rm = T) >= 18 * knt2ms))
      indices <- indices[i]
    })
  }

  return(indices)

}





#' Get the sshs associated with a msw
#'
#' @noRd
#' @param msw numeric maximum sustained wind
#'
#' @return numeric, sshs
computeSSHS <- function(msw){

  if (is.na(msw)) {
    res <- NA

  } else if (msw < sshs[1]) {
    res <- -1

  } else if (msw >= sshs[1] & msw < sshs[2]) {
    res <- 0

  } else if (msw >= sshs[2] & msw < sshs[3]) {
    res <- 1

  } else if (msw >= sshs[3] & msw < sshs[4]) {
    res <- 2

  } else if (msw >= sshs[4] & msw < sshs[5]) {
    res <- 3

  } else if (msw >= sshs[5] & msw < sshs[6]) {
    res <- 4

  } else if (msw >= sshs[6]) {
    res <- 5
  }

  return(res)

}





#' Write data to initialize a Storm object
#'
#' Whether or not to add a storm (with id index in database) in the upcoming Storms object
#'
#' @noRd
#' @param storm_list list of Storm object. To further integrate in a Storms object
#' @param storm_names list of storm names. To further integrate in a Storms object
#' @param storm_seasons list of cyclonic seasons. To further integrate in a Storms object
#' @param storm_sshs list of sshs categories. To further integrate in a Storms object
#' @param nb_storms numeric. number of storm to further integrate in a Storms object
#' @param sds StormsDataset object. sds input from getStorms
#' @param index numeric, index of the storm in the database
#' @param loi_sf_buffer sf object. Location of interest extended with buffer
#'
#' @return a list with 5 slots:
#'   \itemize{
#'     \item list of storm objects
#'     \item list of character (names of storms)
#'     \item list of numeric (seasons of storms)
#'     \item list of numeric (maximum reached categories of storms in sshs)
#'     \item numeric vector (number of storms)
#'   }
writeStorm <- function(storm_list, storm_names, storm_seasons, storm_sshs, nb_storms,
                       sds, index, loi_sf_buffer){

  #Getting lon/lat coordinates
  lon <- sds@database$longitude[, index]
  lat <- sds@database$latitude[, index]
  coords <- data.frame(lon = lon, lat = lat)

  #Keep only non NA data (that are either the first or last observations)
  valid_indices <- which(!is.na(coords$lon))
  coords <- coords[valid_indices,]

  #Removing invalid iso_time
  isotime <- sds@database$isotimes[valid_indices, index]
  list.isotime <- as.numeric(stringr::str_sub(isotime,12,13))
  #Keep only 03H 06H 09H 12H 15H 18h 21H 00h iso times
  ind.isotime <- which(list.isotime %% 3 == 0)
  coords <- coords[ind.isotime,]

  if(dim(coords)[1] == 0){
    #ERROR
    return(list(NULL,NULL,NULL,NULL,NULL))
  }
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
    storm@name <- sds@database$names[index]
    storm@season <- sds@database$seasons[index]
    storm@obs.all <- data.frame(iso.time = isotime,
                                lon = lon[valid_indices],
                                lat = lat[valid_indices],
                                msw = zoo::na.approx(round(sds@database$msw[valid_indices, index] * knt2ms), na.rm = F, rule = 2))


    if("sshs" %in% names(sds@fields)){
      storm@obs.all$sshs <- sds@database$sshs[valid_indices, index]
    }else{
      storm@obs.all$sshs <- unlist(lapply(X = storm@obs.all$msw, FUN = computeSSHS))
    }

    if("rmw" %in% names(sds@fields))
      storm@obs.all$rmw <- zoo::na.approx(round(sds@database$rmw[valid_indices, index] * nm2km), na.rm = F, rule = 2)


    if("pressure" %in% names(sds@fields))
      storm@obs.all$pres <- storm@obs.all$pres <- zoo::na.approx(sds@database$pres[valid_indices, index], na.rm = F, rule = 2)

    if("poci" %in% names(sds@fields))
      storm@obs.all$poci <- zoo::na.approx(sds@database$poci[valid_indices, index], na.rm = F, rule = 2)


    #Wrapping longitudes from -180/180 to 0/360
    lg <- which(storm@obs.all$lon < 0)
    storm@obs.all$lon[lg] <- storm@obs.all$lon[lg] + 360

    #Removing invalid isotime from obs.all
    storm@obs.all <- storm@obs.all[ind.isotime,]
    storm@numobs.all <- dim(storm@obs.all)[1]
    row.names(storm@obs.all) <- seq(1,storm@numobs.all)

    storm@obs <- ind
    storm@numobs <- length(ind)
    storm@sshs <- max(storm@obs.all$sshs,na.rm = T)

    return(list(append(storm_list, storm),
                append(storm_names, storm@name),
                append(storm_seasons, storm@season),
                append(storm_sshs, storm@sshs),
                nb_storms))

  }else{

    return(list(NULL,NULL,NULL,NULL,NULL))

  }

}





#' Initialize a Storms object
#'
#' This function returns a Storms object that
#' gathers all the storms specified by the user
#'
#' @param sds StormsDataset object. Default value is set to IBTRACS_SP which is
#' a database provided by this package and based on the IBTrACS.SP.v04r00.nc
#' file. (See StormsDataset class)
#' @param loi Location of Interest. Must be either:
#' \itemize{
#' \item a SpatialPolygon (shapefile)
#' \item a sf object
#' \item a point of longitude/latitude coordinates provided in a numeric vector
#' \item a character representing a country (See Details section)
#' }
#' @param seasons numeric vector. Should be either one or a range of calendar
#' years. For cyclones that formed in one year and dissipated in the following
#' year, the latter should be used. Default values will allow searching for
#' storms between 1980 and the maximum cyclonic season available in the
#' StormsDataSet object
#' @param names character vector. Names of storms in capital letters. Default
#' value is set to NULL.
#' @param max_dist numeric. Indicates the buffer distance (in km) used to extend
#' the location of interest. Default value is set to 300km
#' @param remove_TD logical. Whether or not to remove tropical depressions
#' (< 18 m/s) and include cyclones only. Default value is set to TRUE
#' @param verbose numeric. Whether or not the function should display
#' informations about the process, outputs, and additional notes. Allowed values
#' are:
#' \itemize{
#' \item 0: Nothing is displayed
#' \item 1: Informations about the process are displayed
#' \item 2: Outputs are also displayed
#' \item 3: Additional notes to manipulate Storms objects are also displayed
#' }
#' Default value is set to 2
#'
#' @returns a Storms object that gathers all storms that match the criteria
#' given in the inputs
#'
#' @details Available countries for the loi input are those provided in the
#' rwolrdxtra package, derived from the newest version of Natural Earth Data.
#' Check the following link for more informations:
#' http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
#'
#'
#' @examples
#' \dontrun{
#' #Get data for a single storm on a given country
#' pam <- getStorms(loi = "Vanuatu", names = "PAM")
#'
#' #Get data for the above storm on a point coordinates (Eastern/Northern degree)
#' pt <- c(169, -19)
#' pam.pt <- getStorms(loi = pt, names = "PAM")
#'
#' #Get data for two storms over New Caledonia
#' sts_nc <- getStorms(loi = "New Caledonia", names = c("ERICA","NIRAN"))
#'
#' #Get data for every storms that occured in the SP basin between 2010 and 2020
#' poly <- cbind(c(135, 290, 290, 135, 135),c(-60, -60, 0, 0, -60))
#' sp <- sf::st_polygon(list(poly))
#' sp <- sf::st_sfc(sp, crs = 4326)
#' sp <- sf::st_as_sf(sp)
#' sts_sp <- getStorms(loi = sp, seasons = c(2010,2020))
#' }
#'
#'
#' @importFrom methods as
#' @export
getStorms <- function(sds = IBTRACS_SP,
                      loi,
                      seasons = c(1980, max(sds@database$seasons, na.rm = T)),
                      names = NULL,
                      max_dist = 300,
                      remove_TD = TRUE,
                      verbose = 2){

  start_time <- Sys.time()

  checkInputsGs(sds, loi, seasons, names, max_dist, verbose, remove_TD)

  if(length(seasons) == 2)
    seasons <- seasons[order(seasons)]


  if (verbose > 0)
    cat("=== getStorms processing ... ===\n\n-> Making buffer: ")

  #Converting loi
  loi.sf <- convertLoi(loi)

  #Handling buffer
  loi.sf.buffer <- makeBuffer(loi.sf, max_dist * km)

  if (verbose){
    cat("Done\n")
    if(is.null(names) & length(seasons) == 2){
      cat("-> Searching storms from",seasons[1],"to",seasons[2],"...\n")
    }else if(is.null(names) & length(seasons) == 1){
      cat("-> Searching for Storms for the ",seasons,"tropical season...\n")
    }else if(!is.null(names)){
      if(length(names) == 1){
        cat("-> Searching for", names,"storm ...\n")
      }else{
        cat("-> Searching for", names,"storms ...\n")
      }
    }
    cat("   -> Identifying Storms: ")
  }


  #Retrieving the matching indices, handling names, seasons and remove TD
  indices <- retrieveStorms(sds@database,
                            filter_names = names,
                            filter_seasons = seasons,
                            remove_TD = remove_TD)

  if (verbose > 0 & length(indices) >= 1) {
    if(is.null(names) & length(seasons) == 2){
      cat(length(indices),"potential candidates...\n")
    }else{
      cat("Done\n")
    }
    cat("-> Gathering storm(s) ... \n")
    if(length(indices) > 1){
      count <- 1 #initializing count for progression bar
      pb <- utils::txtProgressBar(min = count,
                                  max = length(indices),
                                  style = 3)
    }
  }

  if(length(indices) > 0){

    storm.list <- list()
    storm.names <- list()
    storm.seasons <- list()
    storm.sshs <- list()
    nb.storms <- 0
    for (i in indices) {
      sts.output <- writeStorm(storm_list = storm.list,
                               storm_names = storm.names,
                               storm_seasons = storm.seasons,
                               storm_sshs = storm.sshs,
                               nb_storms = nb.storms,
                               sds = sds,
                               index = i,
                               loi_sf_buffer = loi.sf.buffer)

      if(!is.null(sts.output[[1]])){
        storm.list <- sts.output[[1]]
        storm.names <- sts.output[[2]]
        storm.seasons <- sts.output[[3]]
        storm.sshs <- sts.output[[4]]
        nb.storms <- sts.output[[5]]
      }

      if (verbose > 0 & length(indices) > 1){
        utils::setTxtProgressBar(pb, count)
        count <- count + 1
      }
    }

    if (verbose > 0 & length(indices) > 1)
      close(pb)

    stopifnot("No storms found. Please check compatibilities between inputs" = !is.null(unlist(storm.names)))

    #Initializing Storms object
    sts <- new(Class = "Storms",
               nb.storms = nb.storms,
               names = unlist(storm.names),
               seasons = unlist(storm.seasons),
               sshs = unlist(storm.sshs),
               data = storm.list,
               buffer = max_dist,
               spatial.loi = loi.sf,
               spatial.loi.buffer = loi.sf.buffer)

    names(sts@data) <- sts@names
    names(sts@sshs) <- sts@names
    names(sts@seasons) <- sts@names

    end_time <- Sys.time()

    if(verbose > 0){
      cat("\n=== DONE with run time",as.numeric(end_time - start_time),"sec ===\n\n")
      if(verbose > 1){
        cat("SUMMARY:\n")
        cat("(*) LOI: ")
        if(identical(class(loi),"character")){
          cat(loi,"\n")
        }else if(identical(class(loi),"numeric")){
          cat(loi, "lon-lat\n")
        }else{
          cat("sf object (use getLOI function for further informations")
        }
        cat("(*) Buffer size:", getBufferSize(sts),"km\n")
        cat("(*) Remove Tropical Depressions (< 18 m/s in sshs):")
        if(remove_TD){
          cat(" yes\n")
        }else{
          cat(" no\n")
        }
        cat("(*) Number of Storms:", getNbStorms(sts),"\n")
        cat("        Name - Tropical season - SSHS - Number of observation within buffer:\n")
        i = 1
        for(i in 1:length(sts@names)){
          n = sts@names[i]
          s = sts@seasons[[i]]
          sshs = sts@sshs[[i]]
          cat("       ",n,"-", s, "-", sshs, "-",length(getInObs(sts, n, s)),"\n")
        }
        cat("\n")
      }
      if(verbose > 2){
        cat("SHORTCUTS:\n")
        cat("    - Use getNames function to access all storms names\n")
        cat("    - Use getSeasons function to access all or one particular season(s)\n")
        cat("    - Use getLOI function to access LOI\n")
        cat("    - Use getBufferSize function to access buffer size\n")
        cat("    - Use getBuffer function to access buffer\n")
        cat("    - Use getStorm function to access all informations about one particular storm\n")
        cat("    - Use getNbobs function to access all number of all observations for one particular storm\n")
        cat("    - Use getObs function to access all observations about of one particular storm\n")
        cat("    - Use getInObs function to access observations labels within the buffer for one particular storm")
        }
    }

    return(sts)

  }else{
    stop("No storms found. Please check inputs ...")
  }
}
