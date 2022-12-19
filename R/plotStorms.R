





#' Get SSWS palette colors associated with a wind observation
#'
#' @param msw maximum sustained wind observation
#'
#' @return color palette associated with the observation
getColors = function(msw) {
  saffir.simpson.palette = c("#00CCFF",
                             "#00CCCC",
                             "#FFFFB2",
                             "#FECC5C",
                             "#FD8D3C",
                             "#F03B20",
                             "#BD0026")
  if (is.na(msw)) {
    color = NA
  } else{
    if (msw < 18) {
      color = saffir.simpson.palette[1]
    } else{
      if (msw >= 18 & msw < 33) {
        color = saffir.simpson.palette[2]
      } else{
        if (msw >= 33 & msw < 42) {
          color = saffir.simpson.palette[3]
        } else{
          if (msw >= 42 & msw < 49) {
            color = saffir.simpson.palette[4]
          } else{
            if (msw >= 49 & msw < 58) {
              color = saffir.simpson.palette[5]
            } else{
              if (msw >= 58 & msw < 70) {
                color = saffir.simpson.palette[6]
              } else{
                if (msw >= 70) {
                  color = saffir.simpson.palette[7]
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
#' @param all_basin Logical, wether or not to plot track onto the whole basin.
#' Default value is set to FALSE. Otherwise, the plot focuses on the extent of
#' `spatial.loi.buffer` of object `sts`
#'
#' @return NULL
plot_track = function(storm, all_basin) {
  if (all_basin) {
    cex = 0.6
  } else{
    cex = 1
  }

  lon = storm@obs.all$lon
  lat = storm@obs.all$lat
  msw = storm@obs.all$msw
  colors = unlist(lapply(msw, getColors))
  graphics::lines(
    lon,
    lat,
    col = "black",
    lty = storm@lty.track,
    lwd = 1,
    cex = cex
  )

  graphics::points(
    lon,
    lat,
    col = colors,
    pch = 19,
    lwd = 1,
    cex = cex
  )

}






#' Add ISO_time and name label for the first and the
#' last observation within the loi, on a map for a given storm.
#'
#' @param storm Storm object that we want to plot the track. Note that both a map
#' and track of the storm should be previously plotted
#' @param by number, increment of the sequence for the labels to plot
#' @param pos number between 1 and 4, position for the labels to plot: up, left,
#' down, right
#' `spatial.loi.buffer` of object `sts`
#'
#' @return NULL
plot_labels = function(storm, by, pos) {
  cex = 0.6
  ind = round(seq(1, storm@numobs.all, by))

  for (i in ind) {
    lon = storm@obs.all$lon[i]
    lat = storm@obs.all$lat[i]

    graphics::text(
      lon,
      lat,
      labels = paste(storm@name,
                     storm@obs.all$iso.time[i],
                     sep = "\n"),
      pos = pos,
      cex = cex
    )
  }
}





#' Plot a set of storm
#'
#' @param sts S4 Storms object that gathers all the storms we are interested in
#' @param names The storm we would like to plot. Default value is NULL that
#' will plot all the storms contained in `sts`.
#' @param category category of storms we would like to plot
#' @param shapefile a `shapefile` or `SpatialPolygons` object that should replace
#' the default map. Default value is set to NULL.
#' @param ground_color character that stands for the color of the ground area
#' @param ocean_color character that stands for the color of the ocean area
#' @param all_basin Logical, wether or not to plot track onto the whole basin.
#' Default value is set to FALSE. Otherwise, the plot focuses on the extent of
#' `spatial.loi.buffer` of object `sts`
#' @param loi Logical, wether or not to plot `spatial.loi.buffer` on the map
#' Default value is set to TRUE.
#' @param labels Logical, wether or not to plot ISO_time and name labels for the
#' first and the last observation of each storms within the loi. Default value
#' is set to FALSE.
#' @param by number, increment of the sequence for the labels to plot. Default value
#' is set to 8 which represents a 24h time interval
#' @param pos number between 1 and 4, position for the labels to plot: up, left,
#' down, right
#' @param legends Logical, wether or not to plot legends for the
#' plot. Default value is set to FALSE.
#' @param grtc numeric that controls how many graticules to plot. Default value
#' is set to 1, which plots graticules on multipule of 10 coordinates. Note that
#' it should be a power of 2.
#' @param xlim A set of longitude coordinates that controls the longitude extent
#' of the plot. Default value is NULL which will let the plot extends according
#' to the x bounding box of `spatial.loi.buffer`.
#' @param ylim A set of latitude coordinates that controls the latitude extent
#' of the plot. Default value is NULL which will let the plot extends according
#' to the y bounding box of `spatial.loi.buffer`.
#' @return NULL
#' @importFrom ds4psy is_wholenumber
#' @import rworldxtra
#' @export
plotStorms = function(sts,
                      names = NULL,
                      category = NULL,
                      shapefile =  NULL,
                      ground_color = "grey",
                      ocean_color = "white",
                      all_basin = FALSE,
                      labels = FALSE,
                      by = 8,
                      pos = 3,
                      legends = FALSE,
                      loi = TRUE,
                      grtc = 1,
                      xlim = NULL,
                      ylim = NULL) {


  #Check sts input
  stopifnot("no data to plot" = !missing(sts))

  #Check shapefile input
  if (!is.null(shapefile))
    stopifnot(
      "shapefile must be a SpatialPolygonsDataFrame or a SF object" = identical(class(shapefile)[1], "SpatialPolygonsDataFrame") |
        identical(class(shapefile)[1], "sf")
    )

  #Check names input
  if (!is.null(names)) {
    stopifnot("names must be characters" = identical(class(names), "character"))
    stopifnot("Invalid storm names (storm not found)" = names %in% unlist(sts@names))
  }

  #Check category input
  if (!is.null(category)) {
    stopifnot("Invalid category input" = category %in% c(-1,-2,0,1,2,3,4,5))
  }

  #Check grtc input
  stopifnot("grtc must be numeric" = identical(class(grtc), "numeric"))
  stopifnot("grtc must contains an integer" = is_wholenumber(grtc))

  #Check xlim input
  if (!is.null(xlim)) {
    stopifnot("xlim must be numeric" = identical(class(xlim), "numeric"))
    stopifnot("xlim must length 2" = length(xlim) == 2)
    xlim = xlim[order(xlim)]
    stopifnot("xlim must have valid longitude coordinates" = xlim >= 0 &
                xlim <= 360)
  }

  #Check ylim input
  if (!is.null(ylim)) {
    stopifnot("ylim must be numeric" = identical(class(ylim), "numeric"))
    stopifnot("ylim must length 2" = length(ylim) == 2)
    ylim = ylim[order(ylim)]
    stopifnot("ylim must have valid latitude coordinates" = ylim >= -90 &
                ylim <= 90)
  }

  #Check logical inputs
  stopifnot("all_basin must be logical" = identical(class(all_basin), "logical"))
  stopifnot("legends must be logical" = identical(class(legends), "logical"))
  stopifnot("labels must be logical" = identical(class(labels), "logical"))
  stopifnot("loi must be logical" = identical(class(loi), "logical"))

  #Check labels inputs
  stopifnot("by must be as integer" = ds4psy::is_wholenumber(by))
  stopifnot("pos must be as integer" = ds4psy::is_wholenumber(pos))
  stopifnot("pos must be between 1 and 4" = pos >= 1 & pos <= 4)




  #Check on graticules
  l2 = log2(grtc)
  if (!is_wholenumber(l2)) {
    grtc = 2 ** round(l2)
    warning(paste("grtc is not a power of 2, set to", grtc))
  }



  #Handle spatial extent
  if (all_basin) {
    xmin = 150
    xmax = 200
    ymin = -30
    ymax = -5
    if (!is.null(xlim)) {
      warning("xlim ignored")
    }
    if (!is.null(ylim)) {
      warning("ylim ignored")
    }
  } else{
    xmin = sf::st_bbox(sts@spatial.loi.buffer)$xmin
    xmax = sf::st_bbox(sts@spatial.loi.buffer)$xmax
    ymin = sf::st_bbox(sts@spatial.loi.buffer)$ymin
    ymax = sf::st_bbox(sts@spatial.loi.buffer)$ymax
    if (!is.null(xlim)) {
      xmin = xlim[1]
      xmax = xlim[2]
    }
    if (!is.null(ylim)) {
      ymin = ylim[1]
      ymax = ylim[2]
    }
  }


  if (is.null(shapefile)) {
    #Change map here to handle wrapping 0-360 in SWP Basin
    world = rworldmap::getMap(resolution = "high")
    maps::map(
      world,
      fill = TRUE,
      col = ground_color,
      bg = ocean_color,
      wrap = c(0, 360),
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    )
    maps::map.axes(cex.axis = 1)
  } else{
    plot(
      shapefile,
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      col = ground_color,
      bg = ocean_color,
      lwd = 1,
      border = 1,
      axes = T
    )
  }

  #Add graticules
  x.min = round(xmin / 10) * 10 - 20
  x.max = round(xmax / 10) * 10 + 20
  y.min = round(ymin / 10) * 10 - 20
  y.max = round(ymax / 10) * 10 + 20

  #Plot map
  mapproj::map.grid(
    lim = c(x.min, x.max, y.min, y.max),
    nx = abs(x.max - x.min) / 10 * grtc,
    ny = abs(y.max - y.min) / 10 * grtc,
    col = "blue",
    labels = FALSE,
    lty = 3
  )

  #Plot loi
  if (loi)
    plot(sts@spatial.loi.buffer, lwd = 2, add = T)


  #Handle category
  if(!is.null(category) & is.null(names)){
    if(length(category) == 2){
      category = category[order(category)]
      cat.inf = category[1]
      cat.sup = category[2]
      ind = which(unlist(sts@sshs) >= cat.inf & unlist(sts@sshs) <= cat.sup)
    }else{
      #length category == 1
      ind = which(unlist(sts@sshs) == category)
    }
    sts.aux = unlist(sts@data)[ind]
  }else{
    sts.aux = sts@data
  }

  #Plot track
  if (is.null(names)) {
    lapply(sts.aux, plot_track, all_basin)
    if (labels)
      lapply(sts.aux, plot_labels, by, pos)
  } else{
    for(n in names){
      plot_track(sts.aux[[n]], all_basin)
      if (labels)
        plot_labels(sts.aux[[n]], by, pos)
    }
  }

  if (legends) {
    graphics::legend(
      x = "bottomleft",
      legend = c(
        expression(paste("Tropical Depression (below 17 m.s" ^ "-1)")),
        expression(paste("Tropical Storm (18 to 32 m.s" ^ "-1)")),
        expression(paste("Category 1 (33 to 42 m.s" ^ "-1)")),
        expression(paste("Category 2 (43 to 49 m.s" ^ "-1)")),
        expression(paste("Category 3 (50 to 58 m.s" ^ "-1)")),
        expression(paste("Category 4 (58 to 70 m.s" ^ "-1)")),
        expression(paste("Category 5 (70 m.s" ^ "-1","and higher))"))
      ),
      col = c(
        "#00CCFF",
        "#00CCCC",
        "#FFFFB2",
        "#FECC5C",
        "#FD8D3C",
        "#F03B20",
        "#BD0026"
      ),
      pch = 19,
      cex = 0.6
    )
  }

}


