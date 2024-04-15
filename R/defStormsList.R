





#############
#storm Class#
#############





#' `storm`  object
#'
#' Gather all the needed informations to model a single storm
#'
#' @slot name character. Name of the storm
#' @slot season  numeric. Cyclonic season in which the storm has occured
#' @slot  scale numeric. Maximum scale category reached
#' @slot obs numeric vector. Indices of observations within the location of
#'   interest extented with its corresponding buffer (See `stormsList` class)
#' @slot obs.all  data.frame. Contains all of the observations available. An
#'   observation is made up of several fields which are:
#' \itemize{
#'   \item `iso.time`, Date and hours of observations (UTC)
#'   \item `lon`, Longitude coordinate (Eastern degree)
#'   \item `lat`, Latitude coordinate (Northern degree)
#'   \item `msw`, Maximum Sustained Wind (m/s)
#'   \item `scale`, Level in the chosen scale
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
#' @return A `storm` object.
#' \itemize{
#'  \item `name`, character.
#'  \item `season`, numeric.
#'  \item `scale`, numeric.
#'  \item `obs`, numeric.
#'  \item `obs.all`, data.frame.
#' }
#' @importFrom methods new
#' @export
storm <- methods::setClass("storm",
                           slots = c(name = "character",
                                     season = "numeric",
                                     scale = "numeric",
                                     obs = "numeric",
                                     obs.all = "data.frame"))






##############
#stormsList Class#
##############





setOldClass("sf")
#' `stormsList` object
#'
#' Gather all the needed informations to model a set of storms
#'
#' @slot data A list of `storm` (See `storm` class)
#' @slot buffer numeric. Buffer used to extent `spatialLoi` (km)
#' @slot spatialLoi sf object. Represents the location of interest. Projection
#'   is EPSG:4326
#' @slot spatialLoiBuffer sf object. Buffer extension of `spatialLoi`
#' @slot scale numeric. List of storm scale thresholds to use in all functions of
#' the package. Default value is set to the Saffir Simpson Hurricane Scale
#' @slot scalePalette character. Named vector containing the color hex code
#' corresponding to each category in `scale` slot. Default value is the palette associated with the Saffir Simpson Hurricane Scale
#' @return A `stormsList` object.
#' \itemize{
#'  \item `data`, list.
#'  \item `buffer`, numeric.
#'  \item `spatialLoi`, sf.
#'  \item `spatialLoiBuffer`, sf.
#' }
#' @importFrom methods new
#' @importFrom methods show
#' @export
stormsList <- methods::setClass("stormsList",
                                slots = c(data = "list",
                                          buffer = "numeric",
                                          spatialLoi = "sf",
                                          spatialLoiBuffer = "sf",
                                          scale = "numeric",
                                          scalePalette = "character"))





##############
#Show methods#
##############





#' Show a `storm`/`stormsList`
#'
#' Display the `storm`/`stormsList` object
#'
#' @param object `storm`/`stormsList` object
#'
#' @return NULL, only display information about the object
#' @keywords internal
#' @export
#' @docType methods
#' @rdname show-methods
#' @examples
#' \donttest{
#' sds <- defStormsDataset()
#' sts <- defStormsList(sds = sds, loi = "New Caledonia", names = c("LUCAS","NIRAN"))
#' ## Display information about Niran in sts
#' getStorm(sts, name = "NIRAN")
#'
#' ## Display information about sts
#' sts
#' }
setMethod("show",
          signature("storm"),
          function(object) {
            cat("Name:", object@name, "\n")
            cat("Season:", object@season, "\n")
            cat("Maximum level reached in scale:", object@scale, "\n")
            cat("Indices of observations within buffer:", object@obs, "\n")
            cat("Observations:\n")
            print(object@obs.all)
          })
#' @rdname show-methods
setMethod("show",
          signature("stormsList"),
          function(object) {
            cat("***** stormsList *****\n\n")
            cat("Number of storms:", getNbStorms(object), "\n")
            cat("Storms availables:\n\n")
            for (i in 1:getNbStorms(object)) {
              cat("*", i, "\n")
              show(object@data[[i]])
              cat("\n")
            }
            cat("\n***** End stormsList *****\n")
          })





##############################
#Getters for stormsList class#
##############################





#' Extracting a `storm`
#'
#' The `getStorm()` function extracts a specific `storm` object
#'  from a `stormsList` object.
#'
#' @param sts `stormsList`
#' @param name character. Name of the storm to extract.
#' @param season numeric vector. Seasons of occurrence of the storms
#' (e.g., c(2020,2022)). In the Southern Hemisphere,
#' the cyclone season extends across two consecutive years.
#' Therefore, to capture the 2021 to 2022 cyclone season both
#' years should be specified, with cyclones assigned for the year
#' #'  that originated in. By default all storms occurring since
#' 1980 are extracted.
#' @return A `storm` object.
#' @export
#' @docType methods
#' @rdname getStorm-methods
#' @examples
#' #Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting `storm` for the tropical cyclone Niran
#' st <- getStorm(sts, name = "NIRAN")
#' }
setGeneric("getStorm", function(sts, name, season = NULL) standardGeneric("getStorm"))
#' @rdname getStorm-methods
setMethod("getStorm", signature("stormsList"), function(sts, name, season = NULL) {
  if (!is.null(season)) {
    seasons <- getSeasons(sts)
    ind <- which(names(seasons) == name & seasons == season)
    if (!identical(unname(ind), integer(0))) {
      sts@data[[ind]]
    } else {
      stop(paste("No cyclone named", name, "for season", season))
    }
  }else {
    if (length(which(getNames(sts) == name)) > 1)
      stop(paste("More than 1 storm named", name, ".Please specify season\n"))
    sts@data[[name]]
  }
})





#' Getting the number of `storm`
#'
#' The `getNbStorms()` returns the number of `storm` objects
#'  in the given `stormsList` object.
#'
#' @param sts `stormsList`
#'
#' @return numeric, the number of `storm` objects.
#' @export
#' @docType methods
#' @rdname getNbStorms-methods
#' @examples
#' #Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the number of storms in the sts object
#' getNbStorms(sts)
#' }
setGeneric("getNbStorms", function(sts) standardGeneric("getNbStorms"))
#' @rdname getNbStorms-methods
setMethod("getNbStorms", signature("stormsList"), function(sts) length(sts@data))





#' Getting the location of interest
#'
#' The `getLOI()` functions returns the location of interest for the given `stormsList`.
#'
#' @param sts `stormsList` object
#'
#' @return sf object.
#' @export
#' @docType methods
#' @rdname getLOI-methods
#' @examples
#' #Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the location of interest for the sts object
#' loi <- getLOI(sts)
#' }
setGeneric("getLOI", function(sts) standardGeneric("getLOI"))
#' @rdname getLOI-methods
setMethod("getLOI", signature("stormsList"), function(sts) sts@spatialLoi)





#' Getting the buffered location of interest
#'
#' The `getBuffer()` function returns the buffered location
#'  of interest from a `stormsList` object.
#'
#' @param sts `stormsList`
#'
#' @return A `sf` object.
#'
#' @export
#' @docType methods
#' @rdname getBuffer-methods
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the buffered location of interest from the sts object
#' buff <- getBuffer(sts)
#' }
setGeneric("getBuffer", function(sts) standardGeneric("getBuffer"))
#' @rdname getBuffer-methods
setMethod("getBuffer", signature("stormsList"), function(sts) sts@spatialLoiBuffer)





#' Getting the buffer size
#'
#' The `getBufferSize()` returns the buffer size used to generate
#' the buffered location of interest for a `stormsList` object.
#'
#' @param sts `stormsList`
#'
#' @return numeric.
#' @export
#' @docType methods
#' @rdname getBufferSize-methods
#' @examples
#' #Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the buffer size from the sts object
#' buffsize <- getBufferSize(sts)
#' }
setGeneric("getBufferSize", function(sts) standardGeneric("getBufferSize"))
#' @rdname getBufferSize-methods
setMethod("getBufferSize", signature("stormsList"), function(sts) sts@buffer)





###############################################
#Getters for both stormsList and storm classes#
###############################################





#' Getting the names of the storms
#'
#' The `getNames()` function returns the names of the storms
#'  in a `storm` or a `stormsList` object.
#'
#' @param s `storm` or `stormsList` object.
#'
#' @return character vector.
#' @export
#' @docType methods
#' @rdname getNames-methods
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the names of the storms from the sts object
#' getNames(sts)
#' }
setGeneric("getNames", function(s) standardGeneric("getNames"))
#' @rdname getNames-methods
setMethod("getNames", signature("storm"), function(s) s@name)
#' @rdname getNames-methods
setMethod("getNames", signature("stormsList"), function(s) {
  l <- unlist(lapply(s@data, getNames))
  names(l) <- NULL
  l
})





#' Getting cyclonic seasons of the storms
#'
#' The `getSeasons()` function  returns the cyclonic season
#' of each storm in a `storm` or `stormsList` object.
#'
#' @param s `storm` or `stormsList` object.
#'
#' @return numeric vector.
#' @export
#' @docType methods
#' @rdname getSeasons-methods
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the cyclonic seasons of the storms from the sts object
#' getSeasons(sts)
#' }
setGeneric("getSeasons", function(s) standardGeneric("getSeasons"))
#' @rdname getSeasons-methods
setMethod("getSeasons", signature("storm"), function(s) s@season)
#' @rdname getSeasons-methods
setMethod("getSeasons", signature("stormsList"), function(s) unlist(lapply(s@data, getSeasons)))





#' Getting maximum level in the wind scale
#'
#' The `getScale()` function return the maximum wind scale category reached by
#' each storm in the `storm` or `stormsList` object.
#'
#' @param s `storm` or `stormsList` object.
#'
#' @return numeric vector.
#' @export
#' @docType methods
#' @rdname getScale-methods
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting maximum level in the wind scale
#' #reached by each storm in the sts object
#' getScale(sts)
#' }
setGeneric("getScale", function(s) standardGeneric("getScale"))
#' @rdname getScale-methods
setMethod("getScale", signature("storm"), function(s) s@scale)
#' @rdname getScale-methods
setMethod("getScale", signature("stormsList"), function(s) unlist(lapply(s@data, getScale)))






#' Getting the number of observations
#'
#' The getNbObs() function returns the number of observations
#' for a storm in a `storm` or `stormsList` object.
#'
#' @param ... extra arguments for `stormsList`
#' @param s  `storm` or `stormsList` object.
#' @param name character. Name of the storm in capital letters.
#' @param season numeric. Cyclonic season of the `storm`. Required only
#'   if several `storm` in the `s` have the same name. Default value is set to
#'   `NULL`.
#'
#' @return numeric.
#' @export
#' @docType methods
#' @rdname getNbObs-methods
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' ##Getting the number of observations for the tropical cyclone Niran in the sts object
#' getNbObs(getStorm(sts, name = "NIRAN"))
#' getNbObs(sts, name = "NIRAN")
#' }
setGeneric("getNbObs", function(s, ...) standardGeneric("getNbObs"))
#' @rdname getNbObs-methods
setMethod("getNbObs", signature("storm"),
          function(s) dim(s@obs.all)[1])
#' @rdname getNbObs-methods
setMethod("getNbObs", signature("stormsList"),
          function(s, name, season = NULL) dim(getStorm(s, name, season)@obs.all)[1])






#' Getting observations
#'
#' The `getObs()` function returns observed track data for a storm
#' in a `storm` or `stormsList` object.
#'
#' @param ... extra argument for `stormsList`
#' @param s `storm` or `stormsList` object
#' @param name character. Name of the storm in capital letters.
#' @param season numeric. Cyclonic season of the `storm`. Required only
#'   if several `storm` in the `s` object have the same name.
#' Default value is set to `NULL`.
#'
#' @return A data.frame.
#' @export
#' @docType methods
#' @rdname getObs-methods
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the observed track data for the tropical
#' #cyclone Niran in the sts object
#' getObs(getStorm(sts, name = "NIRAN"))
#' getObs(sts, name = "NIRAN")
#' }
setGeneric("getObs", function(s, ...) standardGeneric("getObs"))
#' @rdname getObs-methods
setMethod("getObs", signature("stormsList"), function(s, name, season = NULL) getStorm(s, name, season)@obs.all)
#' @rdname getObs-methods
setMethod("getObs", signature("storm"), function(s) s@obs.all)





#' Getting the number of the observations
#'
#' The `getInObs()` function returns the number of the
#' observations in a given `storm` or `stormsList` object.
#'
#' @param ... extra argument for `stormsList`
#' @param s `storm` or `stormsList` object.
#' @param name character. Name of the storm in capital letters.
#' @param season numeric. Cyclonic season of the `storm`. Required only
#'   if several `storm` in `s` object have the same name. Default value is set
#'   to `NULL`
#'
#' @return Numeric vector.
#'
#' @export
#' @docType methods
#' @rdname getInObs-methods
#' @examples
#' #Creating a stormsDataset
#' \donttest{
#' sds <- defStormsDataset()
#'
#' #Getting storm track data for all storms near New Caledonia
#' sts <- defStormsList(sds=sds, loi = "New Caledonia")
#'
#' #Getting the number of the observation for the tropical cyclone Niran in the sts object
#' getInObs(getStorm(sts, name = "NIRAN"))
#' getInObs(sts, name = "NIRAN")
#' }
setGeneric("getInObs", function(s, ...) standardGeneric("getInObs"))
#' @rdname getInObs-methods
setMethod("getInObs", signature("stormsList"), function(s, name, season = NULL) getStorm(s, name, season)@obs)
#' @rdname getInObs-methods
setMethod("getInObs", signature("storm"), function(s) s@obs)





########
#storms#
########





#' Check inputs for defStormsList function
#'
#' @noRd
#' @param sds stormsDataset object
#' @param loi Either:
#'   \itemize{
#'     \item a SpatialPolygon (shapefile)
#'     \item a sf object
#'     \item a point of longitude/latitude coordinates
#'     \item a character representing a country,
#'   }
#' @param seasons, numeric vector
#' @param names character vector
#' @param maxDist numeric
#' @param scale numeric vector
#' @param scalePalette character vector
#' @param verbose logical
#' @param removeUnder numeric
#' @return NULL, stops the function if an error is detected
checkInputsDefStormsList <- function(sds, loi, seasons, names, maxDist, scale, scalePalette, verbose, removeUnder) {

  #checking sds input
  stopifnot("sds is missing" = !missing(sds))

  #Checking loi input
  if (!missing(loi)) {

    stopifnot("Invalid class for loi" = identical(class(loi), c("sf", "data.frame")) ||
                identical(class(loi)[1], "SpatialPolygonsDataFrame") ||
                identical(class(loi), "numeric") ||
                identical(class(loi), "character"))


    if (identical(class(loi), "numeric")) {
      stopifnot("loi must have valid lon/lat coordinates " = length(loi) == 2
                & loi[1] >= -180 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90)
    }

    if (identical(class(loi), "character"))
      stopifnot("loi must be length 1 " = length(loi) == 1)

  }else {
    stop("loi is missing")
  }


  #Checking seasons input
  stopifnot("seasons must be numeric" = identical(class(as.numeric(seasons)), "numeric"))
  stopifnot("seasons must be as integer" = all(round(seasons) == seasons))
  stopifnot("lower bound of seasons is lower than the minimum season of the stormsDataset.
            This is not allowed. Please correct your seasons bounds or check your
            stormsDataset if this is not expected" = seasons >= sds@seasons["min"])
  stopifnot("upper bound of seasons is larger than the maximum season of the stormsDataset.
            This is not allowed. Please correct your seasons bounds or check your
            stormsDataset if this is not expected" = seasons <= sds@seasons["max"])

  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be a vector of character" = identical(class(names), "character"))
  } else {
    stopifnot("Incompatible format for seasons (must be either length 1 or 2)" = length(seasons) == 1 ||
                length(seasons) == 2)
  }

  #Checking maxDist input
  stopifnot("maxDist must be numeric " = identical(class(maxDist), "numeric"))
  stopifnot("maxDist must be a length 1 vector " = length(maxDist) == 1)
  stopifnot("maxDist must greater or equal than 0 " = maxDist >= 0)

  # Checking scale input
  stopifnot("scale must be vector of numeric" = identical(class(scale), "numeric"))
  stopifnot("invalid scale input" = all(scale>=0))

  # Checking scalePalette input
  if(!is.null(scalePalette)){
    stopifnot("scalePalette must be a (named) character vector" = identical(class(scalePalette), "character"))
    stopifnot("(lenght(scalePalette) must be equal to lenght(scale) + 1)" =
                length(scalePalette) == length(scale) + 1)
  }


  #Checking verbose input
  stopifnot("verbose must be numeric" = identical(class(verbose), "numeric"))
  stopifnot("verbose must length 1" = length(verbose) == 1)
  stopifnot("verbose must be either 0, 1 or 2" = verbose %in% c(0, 1, 2))

  #Checking removeUnder input
  if(!is.null(removeUnder)){
    stopifnot("removeUnder must be numeric" = identical(class(removeUnder), "numeric"))
    stopifnot("removeUnder must a single integer" = length(removeUnder) == 1)
    stopifnot("Invalid removeUnder input" = removeUnder %in% seq(1, length(scale)))
  }

}





#' Convert loi into a sf object
#'
#' @noRd
#' @param loi loi input from `stormsList`
#'
#' @return loi in a sf format
convertLoi <- function(loi) {

  if (identical(class(loi)[1], c("SpatialPolygonsDataFrame"))) {

    loiSf <- sf::st_as_sf(loi)
    loiSf <- sf::st_geometry(loiSf)
    loiSf <- sf::st_sf(loiSf)
    loiSf <- sf::st_as_sf(loiSf)
    if (sf::st_crs(loiSf) != wgs84) {
      loiSf <- sf::st_transform(loiSf, crs = wgs84)
    }

  } else if (identical(class(loi), c("sf", "data.frame"))) {

    loiSf <- sf::st_geometry(loi)
    loiSf <- sf::st_sf(loiSf)
    loiSf <- sf::st_as_sf(loiSf)
    if (sf::st_crs(loiSf) != wgs84) {
      loiSf <- sf::st_transform(loiSf, crs = wgs84)
    }

  } else if (identical(class(loi), c("numeric"))) {

    if (loi[1] < 0) {
      loi[1] <- loi[1] + 360
      warning("longitude coordinate for loi has been corrected and set between 0 and 360 degree")
    }
    loiDf <- data.frame(lon = loi[1], lat = loi[2])
    loiSf <- sf::st_as_sf(loiDf, coords = c("lon", "lat"))
    sf::st_crs(loiSf) <- wgs84
    loiSf <- sf::st_transform(loiSf, crs = wgs84)

  } else if (identical(class(loi), c("character"))) {

    if (loi %in% c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL")) {
      loiSf <- sf::st_as_sf(basins[basins$Name == loi, ][[2]])

    }else {
      map <- rworldmap::getMap(resolution = "high")
      idCountry <- which(map@data$ADMIN == loi)
      stopifnot("invalid entry for loi" = length(idCountry) > 0)
      loiSf <- map[idCountry, ] |> sf::st_as_sf()
      loiSf <- loiSf$geometry |> sf::st_as_sf()
      sf::st_crs(loiSf) <- wgs84
    }

  }

  #Handling time line for Fiji
  loiSf <- sf::st_shift_longitude(loiSf)

  return(loiSf)
}





#' make loi buffer
#'
#' @noRd
#' @param loi character. loi input from `storms`
#' @param loiSf sf object. loi input from `storms` already converted into sf
#'   object
#' @param buffer numeric. Buffer size to use (in km)
#'
#' @return loi extended with buffer in a sf format
makeBuffer <- function(loi, loiSf, buffer) {

  if (buffer == 0){
    loiBuffer <- loiSf

  }else if ((identical(class(loi), c("character"))) && (loi %in% c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"))) {
    loiBuffer <- loiSf
  }else {
    loiBuffer <- sf::st_buffer(loiSf, dist = buffer)
    loiBuffer <- sf::st_shift_longitude(loiBuffer)
  }

    return(loiBuffer)

}





#' Retrieving the matching indices of storms
#'
#' @noRd
#' @param database storms database
#' @param filterNames character vector. Contains names input from the storms
#' @param filterSeasons numeric vector.  Contains seasons input from the storms
#' @param scale numeric vector. CF defStormsList function
#' @param removeUnder numeric. Whether or not to remove storms under this level.
#'  Default value is set to NULL
#'
#' @return indices of storms in the database, that match the filter inputs
retrieveStorms <- function(database, filterNames, filterSeasons, scale, removeUnder) {

  if (length(filterSeasons) == 1) {
    #We are interested in only one cyclonic season
    indices <- which(database$seasons == filterSeasons)
  } else {
    #We are interested in successive cyclonic seasons
    indices <- seq(from = which(database$seasons == filterSeasons[1])[1],
                   to = max(which(database$seasons == filterSeasons[2])))
  }

  if (!is.null(filterNames)) {
    #We are interested in one or several storms given by their name (and season)
    ind <- c()
    for (n in 1:seq_along(filterNames)) {
      id <- NULL
      id <- which(database$names == filterNames[n])
      stopifnot("Storm not found, invalid name ?" = !is.null(id))
      ind <- c(ind, id)
    }

    indices <- intersect(indices, ind)
    stopifnot("No storm(s) found " = !is.null(indices))



  }

  #Removing NOT_NAMED storms
  indices <- indices[which(database$names[indices] != "NOT_NAMED")]

  # Filter Storms if removeUnder is not NULL
  if (!is.null(removeUnder)) {
    suppressWarnings({
      suppressWarnings(i <- which(apply(array(database$msw[, indices],
                                              dim = c(dim(database$msw)[1], length(indices))),
                                        2, max, na.rm = TRUE) >= scale[removeUnder]))
      indices <- indices[i]
    })
  }

  return(indices)

}





#' Get the level in the scale associated with a msw
#'
#' @noRd
#' @param msw numeric maximum sustained wind
#' @param scale list of values that defines the scale
#' intensity of the storm, e.g. `sshs`
#'
#' @return numeric, scale
computeScaleIndice <- function(msw, scale) {

  if (is.na(msw)) {
    res <- NA

  } else {
    res <- findInterval(msw, scale)
  }

  return(res)

}





#' Write data to initialize a `storm` object
#'
#' Whether or not to add a storm (with id index in database) in the upcoming
#' storms object
#'
#' @noRd
#' @param stormList list of `storm` object. To further integrate in a
#'   `stormsList` object
#' @param stormNames list of storm names. To further integrate in a
#'   `stormsList` object
#' @param sds stormsDataset object. sds input from storms
#' @param index numeric, index of the storm in the database
#' @param loiSfBuffer sf object. Location of interest extended with buffer
#' @param scale numeric vector. Thresholds for the scale used
#' @return a list with 2 slots:
#'   \itemize{
#'     \item list of storm objects
#'     \item list of character (names of storms)
#'   }
writeStorm <- function(stormList, stormNames, sds, index, loiSfBuffer, scale) {

  #Getting lon/lat coordinates
  lon <- sds@database$longitude[, index]
  lat <- sds@database$latitude[, index]
  coords <- data.frame(lon = lon, lat = lat)

  #Keep only non NA data (that are either the first or last observations)
  validIndices <- which(!is.na(coords$lon))
  coords <- coords[validIndices, ]

  #Removing invalid iso_time
  isotime <- sds@database$isotimes[validIndices, index]
  listIsotime <- as.numeric(stringr::str_sub(isotime, 12, 13))

  # database should not contain irregular isotimes
  validTimeStep <- listIsotime[2] - listIsotime[1]
  #Keep only valid iso times
  indIsotime <- which(listIsotime %% validTimeStep == 0)
  coords <- coords[indIsotime, ]


  if (dim(coords)[1] == 0) {
    #ERROR
    return(list(NULL, NULL, NULL, NULL, NULL))
  }
  row.names(coords) <- seq(1, dim(coords)[1])


  #Creating sf point coordinates to intersect with loiSfBuffer
  pts <- sf::st_as_sf(coords, coords = c("lon", "lat"))
  sf::st_crs(pts) <- wgs84

  #Intersect points coordinates with loiSfBuffer
  ind <- which(sf::st_intersects(pts, loiSfBuffer, sparse = FALSE) == TRUE)


  #Add TC only if it intersects with loiSfBuffer
  if (length(ind) > 0) {

    storm <- storm()
    storm@name <- sds@database$names[index]
    storm@season <- sds@database$seasons[index]
    storm@obs.all <- data.frame(iso.time = isotime,
                                lon = lon[validIndices],
                                lat = lat[validIndices],
                                msw = zoo::na.approx(round(sds@database$msw[validIndices, index]),
                                                     na.rm = FALSE, rule = 2))



    # scale is calculated using the scale input and the wind speed data
    storm@obs.all$scale <- unlist(lapply(X = storm@obs.all$msw, FUN = computeScaleIndice, scale = scale))

    if ("rmw" %in% names(sds@fields))
      storm@obs.all$rmw <- zoo::na.approx(round(sds@database$rmw[validIndices, index]), na.rm = FALSE, rule = 2)


    if ("pressure" %in% names(sds@fields))
      storm@obs.all$pres <- zoo::na.approx(sds@database$pres[validIndices, index], na.rm = FALSE, rule = 2)

    if ("poci" %in% names(sds@fields))
      storm@obs.all$poci <- zoo::na.approx(sds@database$poci[validIndices, index], na.rm = FALSE, rule = 2)


    #Wrapping longitudes from -180/180 to 0/360
    lg <- which(storm@obs.all$lon < 0)
    storm@obs.all$lon[lg] <- storm@obs.all$lon[lg] + 360

    #Removing invalid isotime from obs.all
    storm@obs.all <- storm@obs.all[indIsotime, ]
    row.names(storm@obs.all) <- seq(1, dim(storm@obs.all)[1])

    storm@obs <- ind
    storm@scale <- max(storm@obs.all$scale, na.rm = TRUE)

    return(list(append(stormList, storm),
                append(stormNames, storm@name)))

  }else {

    return(list(NULL, NULL))

  }

}





#' Creating a `stormsList` object
#'
#' The `defStormsList()` function extracts storm track data from a `stormsDataset`
#' and creates a `stormsList` object based on specified arguments relating to location of interest,
#' seasons, and names of the storms.
#'
#' @param sds `stormsDataset` object.
#' @param loi Location of interest. Can be defined using,
#' \itemize{
#' \item character, a country name (e.g., "Vanuatu")
#' \item character, a basin name among "NA", "SA", "EP", "WP", "SP", "SI" and "NI"
#' \item numeric vector, a point coordinate (lon, lat in decimal degrees, e.g., c(169.5, -19.2))
#' \item sp (SpatialPolygon) or a sf (simple features) object (e.g., created from a shapefile)
#' }
#' @param seasons numeric vector. Seasons of occurrence of the storms (e.g., c(2020,2022)).
#' In the Southern Hemisphere, the cyclone season extends across two consecutive years.
#' Therefore, to capture the 2021 to 2022 cyclone season both years should be specified,
#' with cyclones assigned for the year that originated in. By default all storms from `sds` are extracted.
#' @param names character vector. Names of specific storms (in capital letters).
#' @param maxDist numeric. Maximum distance between the location of interest and the
#' storm for which track data are extracted. Default `maxDist` is set to 300 km.
#' @param scale numeric. List of storm scale thresholds used for the database.
#'   Default value is set to the Saffir Simpson Hurricane Scale
#' @param scalePalette character. Named vector containing the color hex code
#' corresponding to each category interval of `scale` input
#' @param removeUnder numeric. Storms reaching this maximum level or less in the scale are removed.
#'   Default value is set to NULL.
#' @param verbose numeric. Type of information the function displays. Can be:
#' \itemize{
#' \item `2`, information about both the processes and the outputs are displayed (default value),
#' \item `1`, only information about the processes are displayed, or
#' \item `0`, nothing is displayed.
#' }
#'
#' @returns The `defStormsList()` function returns a `stormsList` object containing track data for all storms
#' meeting the specified criteria (e.g., name, season, location).
#'
#' @details The available countries for the `loi` are those provided in the
#'   `rwolrdxtra` package. This package provide high resolution vector country
#'   boundaries derived from Natural Earth data. More informations on the Natural Earth data
#'   here: [http://www.naturalearthdata.com/downloads/10m-cultural-vectors/](https://www.naturalearthdata.com/downloads/10m-cultural-vectors/).
#'
#'@references
#'Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann, C. J. (2010).
#'The International Best Track Archive for Climate Stewardship (IBTrACS).
#'Bulletin of the American Meteorological Society, 91(3), Article 3.
#'<doi:10.1175/2009bams2755.1>
#'
#' @examples
#' \donttest{
#' #Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' #Getting data using country names
#' vanuatu.st <- defStormsList(sds = sds, loi = "Vanuatu")
#'
#' #Getting data using a specific point location
#' pt <- c(169, -19)
#' pam.pt <- defStormsList(sds = sds, loi = pt, names = "PAM")
#'
#' #Getting data using country and storm names
#' niran.nc <- defStormsList(sds = sds, loi = "New Caledonia", names = c("NIRAN"))
#'
#' #Getting data using a user defined spatial polygon
#' poly <- cbind(c(135, 290, 290, 135, 135),c(-60, -60, 0, 0, -60))
#' sp <- sf::st_polygon(list(poly))
#' sp <- sf::st_sfc(sp, crs = 4326)
#' sp <- sf::st_as_sf(sp)
#' sts_sp <- defStormsList(sds = sds, loi = sp)
#'
#' }
#' @importFrom methods as
#' @export
defStormsList <- function(sds,
                   loi,
                   seasons = c(sds@seasons["min"], sds@seasons["max"]),
                   names = NULL,
                   maxDist = 300,
                   scale = sshs,
                   scalePalette = NULL,
                   removeUnder = NULL,
                   verbose = 2) {

  startTime <- Sys.time()

  checkInputsDefStormsList(sds, loi, seasons, names, maxDist, scale, scalePalette, verbose, removeUnder)

  # order scale
  scale = scale[order(scale)]


  if(identical(scale, sshs) & is.null(scalePalette)){
    # Default palette should be SSHS
    scalePalette <- sshsPalette

  }else if(!identical(scale, sshs) & is.null(scalePalette)){
    # Create a default color Palette based on the number of level in scale
    palette <- grDevices::colorRampPalette(colors = c("red", "green", "blue"))
    scalePalette <- rev(palette(length(scale) + 1))
  }

  if(is.null(names(scalePalette))){
    # If scalePalette has no names, provide default ones
    names(scalePalette) <- paste0("Cat. ",seq(0, length(scale)))

  }



  if (length(seasons) == 2)
    seasons <- seasons[order(seasons)]


  if (verbose > 0)
    cat("=== Storms processing ... ===\n\n-> Making buffer: ")

  #Converting loi
  loiSf <- convertLoi(loi)


   #Handling buffer
   spatialBuffer <- makeBuffer(loi, loiSf, maxDist * km)


  if (verbose) {
    cat("Done\n")
    if (is.null(names) && length(seasons) == 2) {
      cat("-> Searching storms from", seasons[1], "to", seasons[2], "...\n")
    }else if (is.null(names) && length(seasons) == 1) {
      cat("-> Searching for Storms for the ", seasons, "tropical season...\n")
    }else if (!is.null(names)) {
      if (length(names) == 1) {
        cat("-> Searching for", names, "storm ...\n")
      }else {
        cat("-> Searching for", names, "storms ...\n")
      }
    }
    cat("   -> Identifying Storms: ")
  }


  #Retrieving the matching indices, handling names, seasons and remove TD
  indices <- retrieveStorms(sds@database,
                            filterNames = names,
                            filterSeasons = seasons,
                            scale = scale,
                            removeUnder = removeUnder)

  if (verbose > 0 && length(indices) >= 1) {
    if (is.null(names) && length(seasons) == 2) {
      cat(length(indices), "potential candidates...\n")
    }else {
      cat("Done\n")
    }
    cat("-> Gathering storm(s) ... \n")
    if (length(indices) > 1) {
      count <- 1 #initializing count for progression bar
      pb <- utils::txtProgressBar(min = count,
                                  max = length(indices),
                                  style = 3)
    }
  }

  if (length(indices) > 0) {

    stormList <- list()
    stormNames <- list()

    for (i in indices) {
      stsOutput <- writeStorm(stormList = stormList,
                               stormNames = stormNames,
                               sds = sds,
                               index = i,
                               loiSfBuffer = spatialBuffer,
                               scale = scale)

      if (!is.null(stsOutput[[1]])) {
        stormList <- stsOutput[[1]]
        stormNames <- stsOutput[[2]]
      }

      if (verbose > 0 && length(indices) > 1) {
        utils::setTxtProgressBar(pb, count)
        count <- count + 1
      }
    }

    if (verbose > 0 && length(indices) > 1)
      close(pb)

    stopifnot("No storms found. Please check compatibilities between inputs" = !is.null(unlist(stormNames)))

    #Initializing stormsList object
    sts <- new(Class = "stormsList",
               data = stormList,
               buffer = maxDist,
               spatialLoi = loiSf,
               spatialLoiBuffer = spatialBuffer,
               scale = scale,
               scalePalette = scalePalette)

    names(sts@data) <- unlist(stormNames)

    endTime <- Sys.time()

    if (verbose > 0) {
      cat("\n=== DONE with run time", as.numeric(endTime - startTime), "sec ===\n\n")
      if (verbose > 1) {
        cat("SUMMARY:\n")
        cat("(*) LOI: ")
        if (identical(class(loi), "character")) {
          cat(loi, "\n")
        }else if (identical(class(loi), "numeric")) {
          cat(loi, "lon-lat\n")
        }else {
          cat("sf object (use getLOI function for further informations\n")
        }
        cat("(*) Buffer size:", getBufferSize(sts), "km\n")
        if (!is.null(removeUnder)) {
          cat("(*) Remove Storms under level ", removeUnder, " in the scale\n")
        }
        cat("(*) Number of storms:", getNbStorms(sts), "\n")
        cat("        Name - Tropical season - Scale - Number of observation within buffer:\n")
        for (i in 1:getNbStorms(sts)){
          cat("       ",
              getNames(sts@data[[i]]), "-",
              getSeasons(sts@data[[i]]), "-",
              getScale(sts@data[[i]]), "-",
              length(getInObs(sts, getNames(sts@data[[i]]), getSeasons(sts@data[[i]]))),
              "\n")
        }
        cat("\n")
      }
    }

    return(sts)

  }else {
    stop("No storms found. Please check inputs ...")
  }
}
