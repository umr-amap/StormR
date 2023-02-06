



#' stormDatabase object
#'
#' Choose the database to use within the package's functions
#'
#' @slot url character. Relative path to download the database
#' @slot name character. Name of the file of the database stored locally

#' @slot  fields ...
#' @export
StormDataBase <- methods::setClass(
  "StormDatabase",
  slots = c(
    url = "character",
    name = "character",
    fields = "character"
  )
)




#' Title
#'
#' @param url ...
#' @param name ...
#' @param fields ...
#'
#' @return ...
#' @export
initDatabase <- function(url = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/netcdf/IBTrACS.ALL.v04r00.nc",
                         name = "IBTrACS.ALL.v04r00.nc",
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
  sdb@fields <- fields

  return(sdb)

}



