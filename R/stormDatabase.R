



#' stormDatabase object
#'
#' Choose the database to use within the package's functions
#'
#' @slot url character. Relative path to download the database
#' @slot name character. Name of the file of the database stored locally
#' @slot format character. Either ".nc" or ".csv"
#' @slot  fields ...
#' @export
StormDataBase <- methods::setClass(
  "StormDatabase",
  slots = c(
    url = "character",
    name = "character",
    format = "character",
    fields = "character"
  )
)




#' Title
#'
#' @param url ...
#' @param name ...
#' @param name ...
#' @param fields ...
#'
#' @return ...
#' @export
initDatabase <- function(url = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/netcdf/IBTrACS.ALL.v04r00.nc",
                         name = "IBTrACS.ALL.v04r00.nc",
                         format = ".nc",
                         fields = c("names" = "name",
                                    "seasons" = "season",
                                    "isoTime" = "iso_time",
                                    "lon" = "usa_lon",
                                    "lat" = "usa_lat",
                                    "msw" = "usa_wind",
                                    "rmw" = "usa_rmw",
                                    "roci" = "usa_roci",
                                    "pressure" = "usa_pres",
                                    "poci" = "usa_poci",
                                    "sshs" = "usa_sshs",
                                    "numobs" = "numobs"
                                    )){

  sdb <- StormDataBase()
  sdb@url <- url
  sdb@name <- name
  sdb@format <- format
  sdb@fields <- fields

  return(sdb)

}





#' Load data from StormDatabase object
#'
#' @noRd
#' @param sdb_info ...
#' @param path ...
#' @return A list of 14 slots
loadData <- function(sdb_info, path){

  if(sdb_info@format == ".nc"){

    filename <- paste0(path,"/",sdb_info@name)
    #Change here
    TC_data_base <- ncdf4::nc_open(filename)
    lon <- ncdf4::ncvar_get(TC_data_base, sdb_info@fields["lon"])
    row <- dim(lon)[1]
    len <- dim(lon)[2]

    sdb <- list(names  = ncdf4::ncvar_get(TC_data_base, sdb_info@fields["names"]),
                seasons = ncdf4::ncvar_get(TC_data_base, sdb_info@fields["seasons"]),
                numobs = ncdf4::ncvar_get(TC_data_base, sdb_info@fields["numobs"]),
                isotimes = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["isoTime"]),
                                 dim = c(row,len)),
                longitude = array(lon, dim = c(row,len)),
                latitude = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["lat"]),
                                 dim = c(row,len)),
                msw = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["msw"]),
                            dim = c(row,len)),
                rmw = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["rmw"]),
                            dim = c(row,len)),
                roci = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["roci"]),
                             dim = c(row,len)),
                pres = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["pressure"]),
                             dim = c(row,len)),
                poci = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["poci"]),
                             dim = c(row,len)),
                sshs = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["sshs"]),
                             dim = c(row,len)))

    ncdf4::nc_close(TC_data_base)

    return(sdb)

  }else if (format == ".csv"){

    filename <-  sdb_info@name
    TC_data_base <- read.csv(system.file("extdata", filename, package = "StormR"))
    lon <- ncdf4::ncvar_get(TC_data_base, sdb_info@fields["lon"])
    row <- dim(lon)[1]
    len <- dim(lon)[2]

    sdb <- list(names  = ncdf4::ncvar_get(TC_data_base, sdb_info@fields["names"]),
                seasons = ncdf4::ncvar_get(TC_data_base, sdb_info@fields["seasons"]),
                numobs = ncdf4::ncvar_get(TC_data_base, sdb_info@fields["numobs"]),
                isotimes = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["isoTime"]),
                                 dim = c(row,len)),
                longitude = array(lon, dim = c(row,len)),
                latitude = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["lat"]),
                                 dim = c(row,len)),
                msw = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["msw"]),
                            dim = c(row,len)),
                rmw = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["rmw"]),
                            dim = c(row,len)),
                roci = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["roci"]),
                             dim = c(row,len)),
                pres = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["pressure"]),
                             dim = c(row,len)),
                poci = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["poci"]),
                             dim = c(row,len)),
                sshs = array(ncdf4::ncvar_get(TC_data_base, sdb_info@fields["sshs"]),
                             dim = c(row,len)))

    ncdf4::nc_close(TC_data_base)

    return(sdb)



  }

}



