




#' Models a storm using various slots
#'
#' @slot name A character that contains the name of the storm
#' @slot sid  A character that contains a unique id given by ibtracs algorithm
#' @slot season A numeric that contains the cyclonic season in which the storm has occured.
#' @slot numobs.all A numeric that contains the total number of observations available.
#' @slot obs.all A data.frame.that contains of the observations available, where an
#' observation is: Basin, Subbasin, ISO_time, lon, lat, wmo_msw, Nadi_wind, Nadi_cat
#' @slot numobs A numeric that contains the number of observations available within
#' the area of interest/location of interest
#' @slot obs A data.frame.that contains of the observations available within
#' the area of interest/location of interest
#' @slot first.obs A numeric standing for the last observation before entering
#' the area of interest/location of interest
#' @slot last.obs A numeric standing for the first observation after leaving
#' the area of interest/location of interest
#' @slot lty.track A numeric that indicates which line type should be used to
#' plot the storm
#' @return A S4 object gathering all the above informations
#' @importFrom methods new
#' @export
Storm <- methods::setClass("Storm",
                  slots = c(name = "character",
                            sid = "character",
                            season = "numeric",
                            numobs.all = "numeric",
                            obs.all = "data.frame",

                            numobs = "numeric",
                            obs = "data.frame",
                            first.obs = "numeric",
                            last.obs = "numeric",

                            lty.track = "numeric"))



#' Models a set of storms that occured in a location of interest
#'
#' @slot data A list of S4 Storm we are interested in
#' @slot time.period Cyclonic season(s) we are interested in
#' @slot names Storms names of interest
#' @slot nb.storms Number of storms contained in this object
#' @slot spatial.loi A SpatialPolygons that represents the location of interest.
#' Projection is EPSG:4326
#' @slot buffer the buffer used to extent `spatial.loi`
#' @slot spatial.loi.buffer buffer extension of `spatial.loi`
#' @return A S4 object gathering all the above informations
#' @importFrom methods new
#' @import sp
#' @export
Storms <- methods::setClass("Storms",
                   slots = c(data = "list",
                             time.period = "numeric",
                             names = "list",
                             nb.storms = "numeric",
                             spatial.loi = "SpatialPolygons",
                             buffer = "numeric",
                             spatial.loi.buffer = "SpatialPolygons"))









#' Initialize a S4 Storms object
#'
#' @param time_period A numeric vector that contains either the time range of
#' cyclonic season we are interested in, or the cyclonic seasons that matches
#' the occurencies of storms contains in `name`. Default value is set to
#' c(1970,2020)
#' @param name A character vector that contains the name of the storms we are
#' interested in. It must be the same size of `time_period`. If the time_period
#' do not correspond, the function will throw an error
#' @param loi Location of interest. Should be either a `SpatialPolygon`, or a
#' matrix containing the longitude/latitude coordinates. Default value is set
#' to "SP" which will focus on the whole South
#' West Pacific Basin. It can also be a numeric vector that contains one longitude/
#' latitude coordinate. If so, a circle based `SpatialPolygon`
#' centered in loi, with a radius of `max_dist` is used as loi.
#' @param max_dist Numeric buffer that indicates the radius (in degree) of the
#' circle if aoi is used.
#'
#' @return a S4 Storms object that gathers all the above informations
#' @importFrom methods as
#' @export
getStorms <- function(time_period = c(1970,2022),
                      name = NULL,
                      loi = "SP",
                      max_dist = 1){


  #---checking inputs

  #Check time_period
  stopifnot("time_period must be numeric" = identical(class(time_period),"numeric"))
  stopifnot("time_period must be as integers" = ds4psy::is_wholenumber(time_period))

  time_period = as.integer(time_period)
  o = order(time_period)
  time_period = time_period[o]

  stopifnot("lower bound of time range is not valid" = time_period > 1969)
  stopifnot("upper bound of time range is not valid" = time_period < 2023)


  if(!is.null(name)){
    stopifnot("name must be a vector of characters" = identical(class(name),"character"))
    stopifnot("name and time_period must be the same length" = length(time_period) == length(name))
    name = name[o]
  }else{
    stopifnot("time_period must be either length 1 or 2" = length(time_period) == 1 || length(time_period) ==2)
  }


  if(!is.character(loi)){
    if(length(loi) == 2){
      stopifnot("loi must be numeric " = identical(class(loi),"numeric"))
      stopifnot("loi must have valid lon/lat coordinates " = loi[1] >= 0 & loi[1] <= 360 & loi[2] >= -90 & loi[2] <= 90)
      loi.id = "Coordinates"
    }else if(identical(class(loi),c("matrix", "array"))){
      loi.id = "Matrix"
    }else if(identical(class(loi),c("SpatialPolygons"))){
      loi.id = "SpatialPolygons"
    }else{
      stop("invalid class for loi")
    }
  }else{
    stopifnot("loi must be length one " = length(loi) == 1)
    if(loi == "SP"){
      loi = matrix(c(150,200,200,150,150,-5,-5,-30,-30,-5),ncol = 2, nrow = 5)
      colnames(loi) = c("lon","lat")
      loi.id = "Matrix"
    }else{
      map = rworldmap::getMap(resolution = "high")
      id.country = which(map$SOVEREIGNT == loi)
      stopifnot("invalid entry for loi" = length(id.country) > 0)
      loi.id = "Country"
    }
  }

  stopifnot("max_dist must be numeric " = identical(class(max_dist),"numeric"))
  stopifnot("max_dist must be a length 1 vector " = length(max_dist) == 1)


  #---open data_base
  ############################################################
  #-----The following 3lines must be eventually changed-----#
  ############################################################
  file_name = paste0("./inst/extdata/IBTrACS.SP.v04r00.nc")
  TC_data_base = ncdf4::nc_open(file_name)
  cyclonic_seasons = ncdf4::ncvar_get(TC_data_base,"season")


  #---Retrieving the matching indices, handling time_period and name
  if(!is.null(name)){
    #we are interested in one or several storms
    storm.names = ncdf4::ncvar_get(TC_data_base,"name")
    indices = c()
    for(n in 1:length(name)){
      seasons.id = which(cyclonic_seasons == time_period[n])
      storm.id = NULL
      storm.id = which(storm.names == name[n])
      stopifnot("Storm not found" = !is.null(storm.id))
      indices = c(indices,seasons.id[stats::na.omit(match(storm.id, seasons.id))[1]])
      stopifnot("Storm not found" = length(indices) > 0)
    }
  }else{
    if(length(time_period) == 1){
      #we are interested in only one cyclonic season
      indices = which(cyclonic_seasons == time_period[1])
    }else{
      #we are interested in successive cyclonic seasons
      indices = seq(from = which(cyclonic_seasons == time_period[1])[1],
                    to = utils::tail(which(cyclonic_seasons == time_period[2]),1))
    }
  }


  #---Handle loi
  if(loi.id == "Coordinates"){
    #create buffer and then a circle centered on loi with a max_dist radius
    loi.df   <- data.frame(pt = 1, lon = loi[1], lat = loi[2])
    loi.sf   <- sf::st_as_sf(loi.df, coords = c("lon", "lat"))
    loi.sf <- sf::st_buffer(loi.sf, dist = max_dist)
    spatial.poly = as(loi.sf,'Spatial')
  }else if(loi.id == "Matrix"){
    spatial.poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(loi)), 1)))
  }else if(loi.id == "SpatialPolygons"){
    spatial.poly = loi
  }else{
    #loi.id == "Country"
    loi.sp = map@polygons[[id.country]]
    spatial.poly = sp::SpatialPolygons(list(loi.sp))
  }

  sp::proj4string(spatial.poly) = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  if(loi.id == "Matrix" | loi.id == "SpatialPolygons" | loi.id =="Country"){
    spatial.poly.buffer = rgeos::gBuffer(spatial.poly, width = max_dist)
  }

  #get data associated with indices
  sts = Storms()
  sts@time.period = time_period
  sts@names = list()
  sts@nb.storms = 0
  sts@spatial.loi = spatial.poly
  sts@spatial.loi.buffer = spatial.poly.buffer
  sts@buffer = max_dist
  storm.list = list()
  k = 2
  for(i in indices){

    #create spatial points coordinates to intersect with spatial.poly
    numobs = ncdf4::ncvar_get(TC_data_base,"numobs")[i]
    lon = ncdf4::ncvar_get(TC_data_base,"lon")[1:numobs,i]
    lat = ncdf4::ncvar_get(TC_data_base,"lat")[1:numobs,i]
    pts = sp::SpatialPoints(cbind(lon,lat),
                        proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


    #which coordinates are within spatial.poly
    ind = as.numeric(names(which(!is.na(sp::over(pts,spatial.poly.buffer)))))

    if(length(ind) > 0){

      sts@nb.storms = sts@nb.storms + 1

      #offset 1 obs before the loi
      if(ind[1] != 1)
        ind = c(ind[1]-1,ind)
      if(ind[length(ind)] != numobs)
        ind = c(ind,ind[length(ind)]+1)



      storm = Storm()
      storm@name = ncdf4::ncvar_get(TC_data_base,"name")[i]
      storm@sid = ncdf4::ncvar_get(TC_data_base,"sid")[i]
      storm@season = ncdf4::ncvar_get(TC_data_base,"season")[i]
      storm@numobs.all = numobs
      storm@obs.all = data.frame(Basin = ncdf4::ncvar_get(TC_data_base,"basin")[1:numobs,i],
                                 Subbasin = ncdf4::ncvar_get(TC_data_base,"subbasin")[1:numobs,i],
                                 ISO_time = ncdf4::ncvar_get(TC_data_base,"iso_time")[1:numobs,i],
                                 lon = ncdf4::ncvar_get(TC_data_base,"lon")[1:numobs,i],
                                 lat = ncdf4::ncvar_get(TC_data_base,"lat")[1:numobs,i],
                                 wmo_msw = ncdf4::ncvar_get(TC_data_base,"wmo_wind")[1:numobs,i] * 0.514,
                                 Nadi_wind = ncdf4::ncvar_get(TC_data_base,"nadi_wind")[1:numobs,i] * 0.514,
                                 Nadi_cat = ncdf4::ncvar_get(TC_data_base,"nadi_cat")[1:numobs,i])

      storm@obs = storm@obs.all[ind,]
      storm@numobs = dim(storm@obs)[1]
      storm@first.obs = ind[1]
      storm@last.obs = ind[length(ind)]
      storm@lty.track = k
      storm.list = append(storm.list,storm)
      k = k+1
      sts@names = append(sts@names,storm@name)
    }
  }

  ncdf4::nc_close(TC_data_base)
  sts@data = storm.list
  names(sts@data) = sts@names



  return(sts)

}















