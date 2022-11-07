




#' Get SSWS palette colors associated with a wind observation
#'
#' @param ms_wind maximum sustained wind observation
#'
#' @return color palette associated with the observation
getColors = function(ms_wind){

  saffir_simpson_palette = c("#00CCFF",
                             "#00CCCC",
                             "#FFFFB2",
                             "#FECC5C",
                             "#FD8D3C",
                             "#F03B20",
                             "#BD0026")
  if(is.na(ms_wind)){
    color = "black"
  }else{
    if(ms_wind <= 17){
      color = saffir_simpson_palette[1]
    }else{
      if(ms_wind > 17 & ms_wind <= 32){
        color = saffir_simpson_palette[2]
      }else{
        if(ms_wind >=32 & ms_wind <= 42){
          color = saffir_simpson_palette[3]
        }else{
          if(ms_wind > 42 & ms_wind <= 49){
            color = saffir_simpson_palette[4]
          }else{
            if(ms_wind > 49 & ms_wind < 58){
              color = saffir_simpson_palette[5]
            }else{
              if(ms_wind >= 58 & ms_wind < 70){
                color = saffir_simpson_palette[6]
              }else{
                if(ms_wind >= 70){
                  color = saffir_simpson_palette[7]
                }
              }
            }
          }
        }
      }
    }
  }

}





#' Plot the track of a storm on an existing map
#'
#' @param storm Storm object that we want to plot the track. Note that a map
#'  should be previously plotted
#'
#' @return NULL
plot_track = function(storm){


  lon = storm@obs.all$lon
  lat = storm@obs.all$lat
  msw = storm@obs.all$Nadi_wind
  colors = unlist(lapply(msw, getColors))
  graphics::points(lon, lat,
                col = colors, type='o',
                lty = storm@lty.track,
                pch = 19, lwd = 1,
                cex = 0.8)

  }






#' Add ISO_time and name label for the first and the
#' last observation within the loi, on a map for a given storm.
#'
#' @param storm Storm object that we want to plot the track. Note that both a map
#' and track of the storm should be previously plotted
#'
#' @return NULL
plot_labels = function(storm){

  graphics::text(storm@obs.all$lon[storm@first.obs],
       storm@obs.all$lat[storm@first.obs],
       labels = paste(storm@name,
                      storm@obs.all$ISO_time[storm@first.obs],
                      sep="\n"),
       pos = 3,
       cex = 0.8)

  graphics::text(storm@obs.all$lon[storm@last.obs],
       storm@obs.all$lat[storm@last.obs],
       labels = paste(storm@name,
                      storm@obs.all$ISO_time[storm@last.obs],
                      sep="\n"),
       pos = 3,
       cex = 0.8)
}





#' Plot a set of storm
#'
#' @param sts S4 Storms object that gathers all the storms we are interested in
#' @param shapefile a `shapefile` or `SpatialPolygons` object that should replace
#' the default map. Default value is set to NULL.
#' @param ground_color character that stands for the color of the ground area
#' @param ocean_color character that stands for the color of the ocean area
#' @param all_basin Logical, wether or not to plot track onto the whole basin.
#' Default value is set to FALSE. Otherwise, the plot focuses on the extent of
#' `spatial.loi` of object `sts`
#' @param labels Logical, wether or not to plot ISO_time and name labels for the
#' first and the last observation of each storms within the loi. Default value
#' is set to FALSE.
#' @param grtc numeric that controls how many graticules to plot. Default value
#' is set to 1, which plots graticules on multipule of 10 coordinates. Note that
#' it should be a power of 2.
#' @return NULL
#' @importFrom ds4psy is_wholenumber
#' @import rworldxtra
#' @export
plotStorms = function(sts,
                      shapefile =  NULL,
                      ground_color = "grey",
                      ocean_color ="white",
                      all_basin = FALSE,
                      labels = FALSE,
                      grtc = 1){

  stopifnot("no data to plot" = !missing(sts))
  if(!is.null(shapefile))
    stopifnot("shapefile must be class SpatialPolygonsDataFrame" = identical(class(shapefile)[1],"SpatialPolygonsDataFrame"))

  stopifnot("grtc must be class numeric" = identical(class(grtc),"numeric"))
  stopifnot("grtc must contains an integer" = is_wholenumber(grtc))

  l2 = log2(grtc)
  if(!is_wholenumber(l2)){
    grtc = 2**round(l2)
    warning(paste("grtc is not a power of 2, set to",grtc))
  }




  if(all_basin){
    if(is.null(shapefile)){
      xmin = 150
      xmax = 200
      ymin = -30
      ymax = -5
    }else{
      xmin = shapefile@bbox["x","min"]
      xmax = shapefile@bbox["x","max"]
      ymin = shapefile@bbox["y","min"]
      ymax = shapefile@bbox["y","max"]
    }
  }else{
    xmin = sts@spatial.loi.buffer@bbox["x","min"]
    xmax = sts@spatial.loi.buffer@bbox["x","max"]
    ymin = sts@spatial.loi.buffer@bbox["y","min"]
    ymax = sts@spatial.loi.buffer@bbox["y","max"]
  }


  if(is.null(shapefile)){
    world = rworldmap::getMap(resolution = "high")
    maps::map(world,
        fill=TRUE,
        col=ground_color,
        bg=ocean_color,
        xlim = c(xmin,xmax),
        ylim=c(ymin, ymax))
    maps::map.axes(cex.axis = 1)
  }else{
    plot(shapefile,
         xlim = c(xmin,xmax),
         ylim=c(ymin, ymax),
         col= ground_color,
         bg = ocean_color,
         lwd = 1,
         border = 1,
         axes = T)
  }

  #Add graticules
  x.min = round(xmin/10)*10 - 20
  x.max = round(xmax/10)*10 + 20
  y.min = round(ymin/10)*10 - 20
  y.max = round(ymax/10)*10 + 20

  mapproj::map.grid(lim = c(x.min,x.max,y.min,y.max),
           nx = abs(x.max-x.min)/10*grtc,
           ny = abs(y.max-y.min)/10*grtc,
           col = "blue",
           labels = FALSE,
           lty = 3)

  #Plot track
  lapply(sts@data, plot_track)
  if(labels)
    lapply(sts@data, plot_labels)


  plot(sts@spatial.loi.buffer, lwd = 2, add=T)
}



