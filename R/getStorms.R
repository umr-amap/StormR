



#############
#Storm Class#
#############





#' `Storm`  object
#'
#' Gather all the needed informations to model a single storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot  sshs numeric. Maximum category reached in the Saffir Simpson Hurricane
#'   Scale
#' @slot obs numeric vector. Indices of observations within the location of
#'   interest extented with its corresponding buffer (See `StormsList` class)
#' @slot obs.all  data.frame. Contains all of the observations available. An
#'   observation is made up of several fields which are:
#' \itemize{
#'   \item `iso.time`, Date and hours of observations (UTC)
#'   \item `lon`, Longitude coordinate (Eastern degree)
#'   \item `lat`, Latitude coordinate (Northern degree)
#'   \item `msw`, Maximum Sustained Wind (m/s)
#'   \item `sshs`, Category in the Saffir Simpson Hurricane Scale
#'  }
#'   The following field is not mandatory but highly recommended
#'  \itemize{
#'   \item `rmw`, Radius of Maximum Wind (km)
#'  }
#'   Also, the following fields are only mandatory to perform Holland and Boose
#'   models (See `Details`)
#' \itemize{
#'   \item `pres`, Pressure at the center (pa)
#'   \item `poci`, Pressure of the Outermost Closed Isobar (pa)
#' }
#' @importFrom methods new
#' @export
Storm <- methods::setClass("Storm",
                           slots = c(name = "character",
                                     season = "numeric",
                                     sshs = "numeric",
                                     obs = "numeric",
                                     obs.all = "data.frame"))






##############
#StormsList Class#
##############





setOldClass("sf")
#' `StormsList` object
#'
#' Gather all the needed informations to model a set of storms
#'
#' @slot data A list of `Storm` (See `Storm` class)
#' @slot buffer numeric. Buffer used to extent `spatial.loi` (km)
#' @slot spatial.loi sf object. Represents the location of interest. Projection
#'   is EPSG:4326
#' @slot spatial.loi.buffer sf object. Buffer extension of `spatial.loi`
#' @importFrom methods new
#' @import sp
#' @export
StormsList <- methods::setClass("StormsList",
                                slots = c(data = "list",
                                          buffer = "numeric",
                                          spatial.loi = "sf",
                                          spatial.loi.buffer = "sf"))





##############
#Show methods#
##############





#' Show a `Storm`/`StormsList`
#'
#' Display the `Storm`/`StormsList` object
#'
#' @noRd
#' @param object `Storm`/StormList object
#'
#' @return NULL
#' @docType methods
#' @rdname show-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia", names = c("ERICA","NIRAN"))
#' ## Display information about Niran in sts
#' getStorm(sts, name = "NIRAN")
#'
#' ## Display information about sts
#' sts
#' }
setMethod("show",
          signature("Storm"),
          function(object){
            cat("Name:",object@name,"\n")
            cat("Season:",object@season,"\n")
            cat("Maximum category reached (SSHS):", object@sshs,"\n")
            cat("Indices of observations within buffer:", object@obs,"\n")
            cat("Observations:\n")
            print(object@obs.all)
          })
#' @rdname show-methods
setMethod("show",
          signature("StormsList"),
          function(object){
            cat("***** StormList *****\n\n")
            cat("Number of Storms:", getNbStorms(object),"\n")
            cat("Storms availables:\n\n")
            for(i in 1:getNbStorms(object)){
              cat("*",i,"\n")
              show(object@data[[i]])
              cat("\n")
            }
            cat("\n***** End StormList *****\n")
          })





##############################
#Getters for StormsList class#
##############################





#' Extract a `Storm`
#'
#' Extract a `Storm` from a `StormsList`
#'
#' @param sts `StormsList`
#' @param name character. Name of the storm to extract
#' @param season numeric. Cyclonic season of the `Storm` to extract. Used only
#'   if several `Storm` in the `sts` share the same name. Default value is set
#'   de `NULL`
#'
#' @return `Storm`
#' @export
#' @docType methods
#' @rdname getStorm-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#' st <- getStorm(sts, name = "NIRAN")
#' }
setGeneric("getStorm", function(sts, name, season = NULL) standardGeneric("getStorm"))
#' @rdname getStorm-methods
setMethod("getStorm", signature("StormsList"), function(sts, name, season = NULL){
  if(!is.null(season)){
    w = which(getNames(sts) == name)
    for(i in 1:length(w)){
      if(getSeasons(sts@data[[w[i]]]) == season){
        j = w[i]
        break
      }
    }
    sts@data[[j]]
  }else{
    if(length(which(getNames(sts) == name)) > 1)
      stop(paste("More than 1 storm named",name, ".Please specify season\n"))
    sts@data[[which(getNames(sts) == name)]]
  }
})





#' Get number of `Storm` in `StormsList`
#'
#' Get the number of `Storm` available in a `StormsList`
#'
#' @param sts `StormsList`
#'
#' @return numeric. Number of `Storm` available in the given `StormsList`
#' @export
#' @docType methods
#' @rdname getNbStorms-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#' nb <- getNbStorms(sts)
#' }
setGeneric("getNbStorms", function(sts) standardGeneric("getNbStorms"))
#' @rdname getNbStorms-methods
setMethod("getNbStorms", signature("StormsList"), function(sts) length(sts@data))





#' Get LOI from a `StormsList`
#'
#' Get the Location Of Interest from a `StormsList`
#'
#' @param sts `StormsList` object
#'
#' @return sf object. Location Of Interest for the given `StormsList`
#' @export
#' @docType methods
#' @rdname getLOI-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#' loi <- getLOI(sts)
#' }
setGeneric("getLOI", function(sts) standardGeneric("getLOI"))
#' @rdname getLOI-methods
setMethod("getLOI", signature("StormsList"), function(sts) sts@spatial.loi)





#' Get the extended LOI from a `StormsList`
#'
#' Get the extended Location Of Interest from a `StormsList`
#'
#' @param sts `StormsList`
#'
#' @return sf object. Extended Location Of Interest for the given `StormsList`
#'
#' @export
#' @docType methods
#' @rdname getBuffer-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#' buff <- getBuffer(sts)
#' }
setGeneric("getBuffer", function(sts) standardGeneric("getBuffer"))
#' @rdname getBuffer-methods
setMethod("getBuffer", signature("StormsList"), function(sts) sts@spatial.loi.buffer)





#' Get buffer size from a `StormsList`
#'
#' Get the buffer size for a `StormsList`
#'
#' @param sts `StormsList`
#'
#' @return numeric. Buffer size (in km) used to generate the extended Location
#'   Of Interest for the given `StormsList`
#' @export
#' @docType methods
#' @rdname getBufferSize-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#' buffsize <- getBufferSize(sts)
#' }
setGeneric("getBufferSize", function(sts) standardGeneric("getBufferSize"))
#' @rdname getBufferSize-methods
setMethod("getBufferSize", signature("StormsList"), function(sts) sts@buffer)





###############################################
#Getters for both StormsList and Storm classes#
###############################################





#' Get name(s) of storm(s)
#'
#' Get the name(s) of storm(s) available in a `Storm` / `StormsList`
#'
#' @param s `Storm` / `StormsList`
#'
#' @return character vector. Names of each storms provided by the given `Storm`
#'   / `StormsList`
#' @export
#' @docType methods
#' @rdname getNames-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#'
#' ## For storm Niran
#' getNames(getStorm(sts, name = "NIRAN"))
#'
#' ## For sts
#' getNames(sts)
#' }
setGeneric("getNames", function(s) standardGeneric("getNames"))
#' @rdname getNames-methods
setMethod("getNames", signature("Storm"), function(s) s@name)
#' @rdname getNames-methods
setMethod("getNames", signature("StormsList"), function(s){
  l <-unlist(lapply(s@data, getNames))
  names(l) <- NULL
  l
})





#' Get the cyclonic season(s) of storm(s) 
#'
#' Get the cyclonic season(s) of storm(s) available in a `Storm` / `StormsList` 
#'
#' @param s `Storm`/`StormsList` 

#'
#' @return numeric vector. Cyclonic season(s) of each storms provided by the given `Storm`
#'   / `StormsList`
#' @export
#' @docType methods
#' @rdname getSeasons-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#' 
#' ## For storm Niran
#' getSeasons(getStorm(sts, name = "NIRAN"))
#' 
#' ## For sts
#' getSeasons(sts)
#' }
setGeneric("getSeasons", function(s) standardGeneric("getSeasons"))
#' @rdname getSeasons-methods
setMethod("getSeasons", signature("Storm"), function(s) s@season)
#' @rdname getSeasons-methods
setMethod("getSeasons", signature("StormsList"), function(s) unlist(lapply(s@data, getSeasons)))





#' Get maximum SSHS reached of storm(s)
#'
#' Get the maximum Saffir Simpson Hurricane Scale (SSHS) reached of storm(s)
#' available in a `Storm` / `StormsList`
#'
#' @param s `Storm` / `StormsList`
#'
#' @return numeric vector. Maximum SSHS reached for each storm provided by the
#'   given `Storm` / `StormsList`
#' @export
#' @docType methods
#' @rdname getSSHS-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#'
#' ## For storm Niran
#' getSSHS(getStorm(sts, name = "NIRAN"))
#'
#' ## For sts
#' getSSHS(sts)
#' }
setGeneric("getSSHS", function(s) standardGeneric("getSSHS"))
#' @rdname getSSHS-methods
setMethod("getSSHS", signature("Storm"), function(s) s@sshs)
#' @rdname getSSHS-methods
setMethod("getSSHS", signature("StormsList"), function(s) unlist(lapply(s@data, getSSHS)))






#' Get the number of observations available
#'
#' Get the number of observations available for a given `Storm`
#'
#' @param ... extra arguments for `StormsList`
#' @param s  `Storm`/`StormsList`
#' @param name character. Name of the storm to extract in capital letters
#' @param season numeric. Cyclonic season of the `Storm` to extract. Used only
#'   if several `Storm` in `s` share the same name. Default value is set to
#'   `NULL`
#'
#' @return numeric. Number of observations available for the given `Storm`
#' @export
#' @docType methods
#' @rdname getNbObs-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#'
#' ## For storm Niran
#' getNbObs(getStorm(sts, name = "NIRAN"))
#'
#' ## Equivalent to
#' getNbObs(sts, name = "NIRAN")
#' }
setGeneric("getNbObs", function(s, ...) standardGeneric("getNbObs"))
#' @rdname getNbObs-methods
setMethod("getNbObs", signature("Storm"), function(s) dim(s@obs.all)[1])
#' @rdname getNbObs-methods
setMethod("getNbObs", signature("StormsList"), function(s, name, season = NULL) dim(getStorm(s, name, season)@obs.all)[1])






#' Get observations available
#'
#' Get the observations available for a given `Storm`
#'
#' @param ... extra argument for `StormsList`
#' @param s `Storm`/`StormsList`
#' @param name character. Name of the storm to extract in capital letters
#' @param season numeric. Cyclonic season of the `Storm` to extract. Used only
#'   if several `Storm` in `s` object share the same name. Default value is set
#'   to `NULL`
#'
#' @return data.frame. Observations of the given storm
#' @export
#' @docType methods
#' @rdname getObs-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#'
#' ## For storm Niran
#' getObs(getStorm(sts, name = "NIRAN"))
#'
#' ## Equivalent to
#' getObs(sts, name = "NIRAN")
#' }
setGeneric("getObs", function(s, ...) standardGeneric("getObs"))
#' @rdname getObs-methods
setMethod("getObs", signature("StormsList"), function(s, name, season = NULL) getStorm(s, name, season)@obs.all)
#' @rdname getObs-methods
setMethod("getObs", signature("Storm"), function(s) s@obs.all)





#' Get indices of observations within the extended LOI
#'
#' Get the indices of observations within the extended Location Of Interest for
#' a given `Storm`
#'
#' @param ... extra argument for `StormsList`
#' @param s `Storm`/`StormsList`
#' @param name character. Name of the storm to extract in capital letters
#' @param season numeric. Cyclonic season of the `Storm` to extract. Used only
#'   if several `Storm` in `s` object share the same name. Default value is set
#'   to `NULL`
#'
#' @return numeric vector. Indices within the extended LOI for the given `Storm`
#'
#' @export
#' @docType methods
#' @rdname getInObs-methods
#' @examples
#' \dontrun{
#' sts <- Storms(loi = "New Caledonia")
#'
#' ## For storm Niran
#' getInObs(getStorm(sts, name = "NIRAN"))
#'
#' ## Equivalent to
#' getInObs(sts, name = "NIRAN")
#' }
setGeneric("getInObs", function(s, ...) standardGeneric("getInObs"))
#' @rdname getInObs-methods
setMethod("getInObs", signature("StormsList"), function(s, name, season = NULL) getStorm(s, name, season)@obs)
#' @rdname getInObs-methods
setMethod("getInObs", signature("Storm"), function(s) s@obs)





########
#Storms#
########





#' Check inputs for Storms function
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


    if(identical(class(loi), "numeric")){
      stopifnot("loi must have valid lon/lat coordinates " = length(loi) == 2
                & loi[1] >= -180 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90)
    }

    if(identical(class(loi), "character"))
      stopifnot("loi must be length 1 " = length(loi) == 1)

  }else{
    stop("loi is missing")
  }

  
  #Checking seasons input
  stopifnot("seasons must be numeric" = identical(class(as.numeric(seasons)), "numeric"))
  stopifnot("seasons must be as integer" = all(round(seasons) == seasons))
  stopifnot("lower bound of time range is not valid" = seasons >= sds@seasons["min"])
  stopifnot("upper bound of time range is not valid" = seasons <= sds@seasons["max"])

  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be a vector of character" = identical(class(names), "character"))
  } else{
    stopifnot("Incompatible format for seasons (must be either length 1 or 2)" = length(seasons) == 1 ||
                length(seasons) == 2)
  }

  #Checking max_dist input
  stopifnot("max_dist must be numeric " = identical(class(max_dist), "numeric"))
  stopifnot("max_dist must be a length 1 vector " = length(max_dist) == 1)
  stopifnot("max_dist must > 0 " = max_dist > 0)

  #Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1 or 2" = verbose %in% c(0, 1, 2))

  #Checking remove_TD input
  stopifnot("remove_TD must be logical" = identical(class(remove_TD), "logical"))

}





#' Convert loi into a sf object
#'
#' @noRd
#' @param loi loi input from `StormsList`
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

    if(loi[1] < 0){
      loi[1] <- loi[1] + 360
      warning("longitude coordinate for loi set between 0 and 360 degree")
    }
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
#' @param loi sf object. loi input from `StormsList` already converted into sf
#'   object
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
#' @param filter_names character vector. Contains names input from the Storms
#' @param filter_seasons numeric vector.  Contains seasons input from the Storms
#' @param remove_TD logical. Whether or not to remove tropical depressions (< 18
#'   m/s) and include cyclones only. Default value is set to TRUE.
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





#' Write data to initialize a `Storm` object
#'
#' Whether or not to add a storm (with id index in database) in the upcoming
#' Storms object
#'
#' @noRd
#' @param storm_list list of `Storm` object. To further integrate in a
#'   `StormsList` object
#' @param storm_names list of storm names. To further integrate in a
#'   `StormsList` object
#' @param sds StormsDataset object. sds input from Storms
#' @param index numeric, index of the storm in the database
#' @param loi_sf_buffer sf object. Location of interest extended with buffer
#'
#' @return a list with 2 slots:
#'   \itemize{
#'     \item list of storm objects
#'     \item list of character (names of storms)
#'   }
writeStorm <- function(storm_list, storm_names, sds, index, loi_sf_buffer){

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
  ind <- which(sf::st_intersects(pts, loi_sf_buffer, sparse = FALSE) == TRUE)


  #Add TC only if it intersects with loi_sf_buffer
  if (length(ind) > 0) {

    storm <- Storm()
    storm@name <- sds@database$names[index]
    storm@season <- sds@database$seasons[index]
    storm@obs.all <- data.frame(iso.time = isotime,
                                lon = lon[valid_indices],
                                lat = lat[valid_indices],
                                msw = zoo::na.approx(round(sds@database$msw[valid_indices, index]), na.rm = F, rule = 2))


    if("sshs" %in% names(sds@fields)){
      storm@obs.all$sshs <- sds@database$sshs[valid_indices, index]
    }else{
      storm@obs.all$sshs <- unlist(lapply(X = storm@obs.all$msw, FUN = computeSSHS))
    }

    if("rmw" %in% names(sds@fields))
      storm@obs.all$rmw <- zoo::na.approx(round(sds@database$rmw[valid_indices, index]), na.rm = F, rule = 2)


    if("pressure" %in% names(sds@fields))
      storm@obs.all$pres <- storm@obs.all$pres <- zoo::na.approx(sds@database$pres[valid_indices, index], na.rm = F, rule = 2)

    if("poci" %in% names(sds@fields))
      storm@obs.all$poci <- zoo::na.approx(sds@database$poci[valid_indices, index], na.rm = F, rule = 2)


    #Wrapping longitudes from -180/180 to 0/360
    lg <- which(storm@obs.all$lon < 0)
    storm@obs.all$lon[lg] <- storm@obs.all$lon[lg] + 360

    #Removing invalid isotime from obs.all
    storm@obs.all <- storm@obs.all[ind.isotime,]
    row.names(storm@obs.all) <- seq(1,dim(storm@obs.all)[1])

    storm@obs <- ind
    storm@sshs <- max(storm@obs.all$sshs,na.rm = T)

    return(list(append(storm_list, storm),
                append(storm_names, storm@name)))

  }else{

    return(list(NULL,NULL))

  }

}





#' Creating a `StormsList` object 
#'
#' The `Storms()` function extracts storm track data from a `StormsDataset` 
#' and creates a `StormsList` object.
#'
#' @param sds `StormsDataset` object. By default the `test_dataset` object is used
#' @param loi Location of interest. Can be defined using: 
#' \itemize{
#' \item character. A country name (e.g., "Vanuatu").
#' \item numeric vector. A point coordinate (lon, lat in decimal degrees, e.g., c(169.5, -19.2)).
#' \item sp (SpatialPolygon) or a sf (simple features) object (e.g., created from a shapefile).
#' }
#' @param seasons numeric vector. Season(s) of occurrence of the storms. One (e.g., 2020)
#'  or two years (e.g., c(2020,2022)) to extract storms occurring during one specific season or 
#'  during several consecutive seasons, respectively. By default all storms occurring since 1980 are extracted.
#' @param names character vector. Names of specific storms (in capital letters) to extract.
#' @param max_dist numeric. Maximum distance between the location of interest and the storm for which track data are extracted. 
#' Default `max_dist` is set to 300 km.
#' @param remove_TD logical. Whether (TRUE) or not (FALSE) removing tropical depressions (msw < 18
#'   m/s). Default value is set to `TRUE`.
#' @param verbose numeric. Type of information the function displays:
#' \itemize{
#' \item `0`: Nothing is displayed.
#' \item `1`: Only information about the processes are displayed.
#' \item `2`: Information about both the processes and the outputs are displayed (default value).
#' }
#'
#' @returns The `Storms()` function a `StormsList` object containing track data for all storms 
#' meeting the specified criteria (name, season, location, ...).
#'
#' @details The available countries for the `loi` are those provided in the
#'   `rwolrdxtra` package. This package provide high resolution vector country 
#'   boundaries derived from Natural Earth data. More informations on the Natural Earth data
#'   here: http://www.naturalearthdata.com/downloads/10m-cultural-vectors/.
#'
#' @examples
#' \dontrun{
#' #Getting data using country names
#' vanuatu.st <- Storms(loi = "Vanuatu")
#'
#' #Getting data using a specific point location
#' pt <- c(169, -19)
#' pam.pt <- Storms(loi = pt, names = "PAM")
#'
#' #Getting data using country and storm names 
#' niran.nc <- Storms(loi = "New Caledonia", names = c("NIRAN"))
#'
#' #Getting data using a user defined spatial polygon
#' poly <- cbind(c(135, 290, 290, 135, 135),c(-60, -60, 0, 0, -60))
#' sp <- sf::st_polygon(list(poly))
#' sp <- sf::st_sfc(sp, crs = 4326)
#' sp <- sf::st_as_sf(sp)
#' sts_sp <- Storms(loi = sp)
#' }
#'
#'
#' @importFrom methods as
#' @export
Storms <- function(sds = test_dataset,
                   loi,
                   seasons = c(sds@seasons["min"], sds@seasons["max"]),
                   names = NULL,
                   max_dist = 300,
                   remove_TD = TRUE,
                   verbose = 2){
  
  start_time <- Sys.time()

  checkInputsGs(sds, loi, seasons, names, max_dist, verbose, remove_TD)

  if(length(seasons) == 2)
    seasons <- seasons[order(seasons)]


  if (verbose > 0)
    cat("=== Storms processing ... ===\n\n-> Making buffer: ")

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

    for (i in indices) {
      sts.output <- writeStorm(storm_list = storm.list,
                               storm_names = storm.names,
                               sds = sds,
                               index = i,
                               loi_sf_buffer = loi.sf.buffer)

      if(!is.null(sts.output[[1]])){
        storm.list <- sts.output[[1]]
        storm.names <- sts.output[[2]]
      }

      if (verbose > 0 & length(indices) > 1){
        utils::setTxtProgressBar(pb, count)
        count <- count + 1
      }
    }

    if (verbose > 0 & length(indices) > 1)
      close(pb)

    stopifnot("No storms found. Please check compatibilities between inputs" = !is.null(unlist(storm.names)))

    #Initializing StormsList object
    sts <- new(Class = "StormsList",
               data = storm.list,
               buffer = max_dist,
               spatial.loi = loi.sf,
               spatial.loi.buffer = loi.sf.buffer)

    names(sts@data) <- unlist(storm.names)

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
        for(i in 1:getNbStorms(sts)){
          n = getNames(sts@data[[i]])
          s = getSeasons(sts@data[[i]])
          sshs = getSSHS(sts@data[[i]])
          cat("       ",n,"-", s, "-", sshs, "-",length(getInObs(sts, n, s)),"\n")
        }
        cat("\n")
      }
    }

    return(sts)

  }else{
    stop("No storms found. Please check inputs ...")
  }
}
