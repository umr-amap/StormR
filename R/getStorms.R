





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

setGeneric("getStorm", function(sts, name, season = NULL) standardGeneric("getStorm") )
setMethod("getStorm", signature("Storms"), function(sts, name, season = NULL){
  if(!is.null(season)){
    w = which(sts@names == name)
    for(i in 1:length(w)){
      if(getSeason(sts@data[[w[i]]]) == season){
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

setGeneric("getStormNbObs", function(sts, name, season = NULL) standardGeneric("getStormNbObs"))
setMethod("getStormNbObs", signature("Storms"), function(sts, name, season = NULL) getNbObs(getStorm(sts, name, season)))

setGeneric("getStormObs", function(sts, name, season = NULL) standardGeneric("getStormObs"))
setMethod("getStormObs", signature("Storms"), function(sts, name, season = NULL) getObs(getStorm(sts, name, season)))

setGeneric("getStormInObs", function(sts, name, season = NULL) standardGeneric("getStormInObs"))
setMethod("getStormInObs", signature("Storms"), function(sts, name, season = NULL) getInObs(getStorm(sts, name, season)))






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
  if(!sds@loaded){
    err <- paste0("Data have not been collected yet, please run ",deparse(substitute(sds))," <- collectData(",deparse(substitute(sds)),")\n")
    stop(err)
  }

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
#' @param database Storms database
#' @param filter_names character vector. Contains names input from the getStorms
#' @param filter_seasons numeric vector.  Contains seasons input from the getStorms
#' @param remove_TD logical. Whether or not to remove tropical depressions (< 18 m/s)
#' and include cyclones only. Default value is set to TRUE.
#'
#' @return indices of storms in the data base, that match the filter inputs
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
    i <- which(apply(array(database$sshs[,indices], dim = c(dim(database$sshs)[1], length(indices))),2,max, na.rm = T) >= 0)
    indices <- indices[i]
  }

  return(indices)

}





#' Write data to initialize a Storms object
#'
#' Whether or not to add a storm (with id index in database) in the upcoming Storms object
#'
#' @noRd
#' @param storm_list list of Storm object. To further integrate in a Storms object
#' @param storm_names list of storm names. To further integrate in a Storms object
#' @param storm_seasons list of cyclonic seasons. To further integrate in a Storms object
#' @param storm_sshs list of storm sshs. To further integrate in a Storms object
#' @param nb_storms numeric. number of storm to further integrate in a Storms object
#' @param database Storm database
#' @param index numeric, index of the storm in the database
#' @param loi_sf_buffer sf object. Location of interest extended with buffer
#' @param k numeric. linetype
#'
#' @return a list with 4 slots:
#'   \itemize{
#'     \item list of storm objects
#'     \item list of character (names of storms)
#'     \item list of numeric (seasons of storms)
#'     \item numeric vector (number of storms)
#'   }
writeStorm <- function(storm_list, storm_names, storm_seasons, storm_sshs, nb_storms,
                       database, index, loi_sf_buffer, k){


  #Getting lon/lat coordinates
  lon <- database$longitude[, index]
  lat <- database$latitude[, index]
  coords <- data.frame(lon = lon, lat = lat)

  #Keep only non NA data (that are either the first or last observations)
  valid_indices <- which(!is.na(coords$lon))
  coords <- coords[valid_indices,]

  #Removing invalid iso_time
  isotime <- database$isotimes[valid_indices, index]
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
    storm@name <- database$names[index]
    storm@season <- database$seasons[index]
    storm@obs.all <- data.frame(
      iso.time = isotime,
      lon = lon[valid_indices],
      lat = lat[valid_indices],
      msw = zoo::na.approx(round(database$msw[valid_indices, index] * knt2ms), na.rm = F, rule = 2),
      rmw = zoo::na.approx(database$rmw[valid_indices, index], na.rm = F, rule = 2),
      roci = zoo::na.approx(database$roci[valid_indices, index], na.rm = F, rule = 2),
      pres = zoo::na.approx(database$pres[valid_indices, index], na.rm = F, rule = 2),
      poci = zoo::na.approx(database$poci[valid_indices, index], na.rm = F, rule = 2),
      sshs = database$sshs[valid_indices, index]
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
#' a database provided by this package and based on the IBTrACS.SP.v04r00.nc file.
#' The database must be previously loaded (See Details)
#' @param loi Location of Interest. Must be either:
#' \itemize{
#' \item a SpatialPolygon (shapefile)
#' \item a sf object
#' \item a point of longitude/latitude coordinates provided in a numeric vector
#' \item a character representing a country (See Details section)
#' }
#' @param seasons numeric vector. Should be either one or a range of calendar
#' years. For cyclones that formed in one year and dissipated in the following
#' year, the latter should be used
#' @param names character vector. Names of storms. Default value is set to NULL.
#' @param max_dist numeric. Indicates the buffer distance (in km) used to generate
#' spatial.loi.buffer. Default value is set to 300km. This value also represents
#' the maximum distance from the track of the storm where computations can be performed
#' @param remove_TD logical. Whether or not to remove tropical depressions (< 18 m/s)
#' and include cyclones only. Default value is set to TRUE.
#' @param verbose numeric. Either 0, 1, 2 or 3. Whether or not the function must
#' be verbose and display informations about the process, outputs, and additional notes
#' \itemize{
#' \item 0 Nothing is displayed
#' \item 1 Informations about the process are displayed
#' \item 2 Outputs are also displayed
#' \item 3 Additional notes to manipulate Storms objects are also displayed
#' }
#' Default value is set to 2
#'
#' @returns a Storms object that gathers all storms that match the criteria given
#' in the inputs
#'
#' @examples
#' #Focus on a single storm
#' pam <- getStorms(loi = "Vanuatu", names = "PAM")
#'
#' #Focus on several storms over New Caledonia
#' sts_nc <- getStorms(loi = "New Caledonia", names = c("ERICA","NIRAN"))
#'
#' #Focus on every storms that occured in the SP basin between 2010 and 2020
#' poly <- cbind(c(135, 290, 290, 135, 135),c(-60, -60, 0, 0, -60))
#' sp <- sf::st_polygon(list(poly))
#' sp <- sf::st_sfc(sp, crs = 4326)
#' sp <- sf::st_as_sf(sp)
#' sts_sp <- getStorms(loi = sp, seasons = c(2010,2020))
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
    k <- 2 #initializing line type

    for (i in indices) {
      sts.output <- writeStorm(storm_list = storm.list,
                               storm_names = storm.names,
                               storm_seasons = storm.seasons,
                               storm_sshs = storm.sshs,
                               nb_storms = nb.storms,
                               database = sds@database,
                               index = i,
                               loi_sf_buffer = loi.sf.buffer,
                               k = k)
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

      k <- k + 1
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
    names(sts@names) <- sts@names
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
          cat("       ",n,"-", s, "-", sshs, "-",length(getStormInObs(sts, n, s)),"\n")
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
        cat("    - Use getStormNbobs function to access all number of all observations for one particular storm\n")
        cat("    - Use getStormObs function to access all observations about of one particular storm\n")
        cat("    - Use getStormInObs function to access observations labels within the buffer for one particular storm")
        }
    }

    return(sts)

  }else{
    stop("No storms found. Please check inputs ...")
  }
}
