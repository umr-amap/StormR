



#' stormDatabase object
#'
#' Choose the database to use within the package's functions
#'
#' @slot url character. Relative path to download the database
#' @slot filename character. Name of the file of the database stored locally
#' @slot format character. Either ".nc" or ".csv"
#' @slot fields ...
#' @slot sdb ...
#' @slot loaded ...
#' @export
StormDataBase <- methods::setClass(
  "StormDatabase",
  slots = c(
    url = "character",
    filename = "character",
    format = "character",
    fields = "character",
    sdb = "list",
    loaded = "logical"
  )
)



#' Title
#'
#' @param url ...
#' @param filename ...
#' @param format ...
#' @param fields ...
#'
#' @return ...
#' @export
initDatabase <- function(url = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/netcdf/IBTrACS.ALL.v04r00.nc",
                         filename = "/home/baptiste/Desktop/Travail/StormR/data/IBTrACS.ALL.v04r00.nc",
                         format = ".nc",
                         fields = c("sid" = "sid",
                                    "names" = "name",
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

  sdb <- new(Class = "StormDatabase",
            url = url,
            filename = filename,
            format = format,
            fields = fields,
            sdb = list(),
            loaded = FALSE)

  return(sdb)

}



getDataCSV <- function(indices, sdb_fields, type = "numeric"){

  if(type == "numeric"){
    return(as.numeric(sdb_fields[indices[1]:indices[2]]))
  }else{
    return(sdb_fields[indices[1]:indices[2]])
  }

}


count <- function(sid, SID){

  return(length(which(SID == sid)))
}







setGeneric("collectData", function(sdb, verbose = T) standardGeneric("collectData"))
setMethod("collectData",
          signature("StormDatabase"),
          definition =  function(sdb, verbose = T){

            stopifnot("Data base already loaded" = !sdb@loaded)

            if(verbose)
              cat("=== Loading data  ===\nOpen database... ")


            if(sdb@format == ".nc"){

              #Change here
              TC_data_base <- ncdf4::nc_open(sdb@filename)

              if(verbose)
                cat(sdb@filename,"opened\nCollecting data ...\n")

              lon <- ncdf4::ncvar_get(TC_data_base, sdb@fields["lon"])
              row <- dim(lon)[1]
              len <- dim(lon)[2]

              data <- list(names  = ncdf4::ncvar_get(TC_data_base, sdb@fields["names"]),
                           seasons = ncdf4::ncvar_get(TC_data_base, sdb@fields["seasons"]),
                           numobs = ncdf4::ncvar_get(TC_data_base, sdb@fields["numobs"]),
                           isotimes = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["isoTime"]),
                                            dim = c(row,len)),
                           longitude = array(lon, dim = c(row,len)),
                           latitude = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["lat"]),
                                            dim = c(row,len)),
                           msw = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["msw"]),
                                       dim = c(row,len)),
                           rmw = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["rmw"]),
                                       dim = c(row,len)),
                           roci = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["roci"]),
                                        dim = c(row,len)),
                           pres = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["pressure"]),
                                        dim = c(row,len)),
                           poci = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["poci"]),
                                        dim = c(row,len)),
                           sshs = array(ncdf4::ncvar_get(TC_data_base, sdb@fields["sshs"]),
                                        dim = c(row,len)))

              ncdf4::nc_close(TC_data_base)


            }else if (sdb@format == ".csv"){

              #Change here
              TC_data_base <- utils::read.csv(sdb@filename, header = T)

              if(verbose)
                cat(sdb@filename,"opened\nCollecting data ...\n")

              #Handle length of data base
              r <- 2
              SID <- TC_data_base[,sdb@fields["sid"]]
              lenTot<- length(SID)
              SID <- SID[r:lenTot]
              sid <- unique(SID)


              #Computing number of observations using SID slot
              numobs <- unlist(lapply(X = as.list(sid), FUN =  count, SID =  SID))

              #Get names and seasons
              names <- TC_data_base[r:lenTot,sdb@fields["names"]]
              seasons <- TC_data_base[r:lenTot,sdb@fields["seasons"]]

              indices <- list(c(1,numobs[1]))
              index <- numobs[1]
              n <- names[1]
              s <- seasons[1]
              for(i in 2:(length(numobs)-1)){
                indices <- append(indices, list(c(index + 1, index + numobs[i])))
                n <- c(n,names[index + 1])
                s <- c(s,seasons[index + 1])
                index <- c(index + numobs[i])
              }
              n <- c(n,names[index + 1])
              s <- as.numeric(c(s,seasons[index + 1]))
              indices <- append(indices, list(c(index + 1, index + 1 + numobs[length(numobs)])))

              len <- lenTot - (r-1)
              row <- length(max(numobs, na.rm = T))

              #Collecting data
              data <- list(names = n,
                           seasons = s,
                           numobs = numobs,
                           isotimes = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["isoTime"]], type = "character"), dim = c(row, len)),
                           longitude = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["lon"]]), dim = c(row, len)),
                           latitude = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["lat"]]), dim = c(row, len)),
                           msw = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["msw"]]), dim = c(row, len)),
                           rmw = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["rmw"]]), dim = c(row, len)),
                           roci = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["roci"]]), dim = c(row, len)),
                           poci = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["poci"]]), dim = c(row, len)),
                           pres = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["pressure"]]), dim = c(row, len)),
                           sshs = array(lapply(X = indices, FUN = getDataCSV, sdb_fields = TC_data_base[r:lenTot,sdb@fields["sshs"]]), dim = c(row, len)))

            }

            if(verbose)
              cat("=== DONE ===\n")


            sdb@sdb <- data
            sdb@loaded <- TRUE

            return(sdb)

          })






# fields <- c("sid" = "SID",
#             "names" = "NAME",
#             "seasons" = "SEASON",
#             "isoTime" = "ISO_TIME",
#             "lon" = "USA_LON",
#             "lat" = "USA_LAT",
#             "msw" = "USA_WIND",
#             "rmw" = "USA_RMW",
#             "roci" = "USA_ROCI",
#             "pressure" = "USA_PRES",
#             "poci" = "USA_POCI",
#             "sshs" = "USA_SSHS",
#             "numobs" = "NUMBER")

# sdb_info <- initDatabase(url = "none", filename = "/home/baptiste/Desktop/Travail/StormR/data/ibtracs.SP.list.v04r00.csv", fields = fields, format = ".csv")
# sdb_info_nc <- initDatabase(filename = "/home/baptiste/Desktop/Travail/StormR/data/IBTrACS.SP.v04r00.nc")
# tc <- collectData(sdb_info)
# tc_nc <- collectData(sdb_info_nc)


