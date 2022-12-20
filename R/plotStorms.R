





#' Get the Saffir Simpson Hurricane scale colors associated with a maximum
#' sustained wind speed
#'
#' @param msw numeric. Maximum Sustained Wind
#'
#' @return color associated with the observation
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

  return(color)
}





#' Plot the track of a Storm on a map that should be previsouly plotted
#'
#' @param st Storm object
#' @param all_basin logical. Whether or not to plot the track onto the whole basin.
#' Default value is set to `FALSE`. Otherwise, the plot focuses on the extent of
#' `spatial.loi.buffer` of the Storms object in which `storm` belongs
#'
#' @return NULL
plotTrack = function(st, all_basin) {

  if (all_basin) {
    cex = 0.6
  } else{
    cex = 1
  }

  lon = st@obs.all$lon
  lat = st@obs.all$lat
  msw = st@obs.all$msw
  colors = unlist(lapply(msw, getColors))

  graphics::lines(
    lon,
    lat,
    col = "black",
    lty = st@lty.track,
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






#' Add ISO Times and name labels on the track of a Storm on a map that should be
#' previsouly plotted
#'
#' @param st Storm object
#' @param by numeric. Increment of the sequence for the labels to plot
#' @param pos numeric. Must be between 1 and 4 and correspond to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right)
#'
#' @return NULL
plotLabels = function(st, by, pos) {
  cex = 0.6
  ind = round(seq(1, st@numobs.all, by))

  for (i in ind) {
    lon = st@obs.all$lon[i]
    lat = st@obs.all$lat[i]

    graphics::text(
      lon,
      lat,
      labels = paste(st@name,
                     st@obs.all$iso.time[i],
                     sep = "\n"),
      pos = pos,
      cex = cex
    )
  }
}





#' Plot a set of Storms contained in a Storms object.
#'
#' @param sts Storms object
#' @param names character(s). Names of the Storms we would like to plot. Default
#' value is set to `NULL` which will consider every Storm in `sts`
#' @param category numeric(s). Should be either a category or a range of category
#' in the Saffir Simpson scale (-2 to 5). Default value is set to `NULL` which
#' will consider every Storm in `sts`. Otherwise it will consider only storm that
#' reached `category`
#' @param ground_color character. Color for the ground
#' @param ocean_color character. Color for the oceans
#' @param all_basin logical. Whether or not to plot the track onto the whole basin.
#' Default value is set to `FALSE`. Otherwise, the plot focuses on the extent of
#' `spatial.loi.buffer` of `sts`
#' @param loi logical. Whether or not to plot `spatial.loi.buffer` on the map
#' Default value is set to TRUE.
#' @param labels logical. Whether or not to plot ISO Times and name labels
#' @param by numeric. Increment of the sequence for the labels to plot. Default value
#' is set to 8 which represents a 24h time interval
#' @param pos numeric. Must be between 1 and 4 and correspond to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right).
#' Default value is set to 3
#' @param legends logical. Whether or not to plot legends. Default value is set
#' to FALSE.
#' @param grtc numeric. Controls the number of graticules to plot. Default value
#' is set to 1, which plots graticules on multiple of 10 coordinates. Note that
#' it should be a power of 2.
#' @param xlim numerics. A set of longitude coordinates that controls the
#' longitude extent of the plot. Default value is set to `NULL` which will let
#' the plot extends according to the x bounding box of `spatial.loi.buffer`.
#' @param ylim numerics. A set of latitude coordinates that controls the
#' latitude extent of the plot. Default value is set to `NULL` which will let
#' the plot extends according to the x bounding box of `spatial.loi.buffer`.
#' @return NULL
#' @importFrom ds4psy is_wholenumber
#' @import rworldxtra
#' @export
plotStorms = function(sts,
                      names = NULL,
                      category = NULL,
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

  #Check inputs
  stopifnot("all_basin must be logical" = identical(class(all_basin), "logical"))
  stopifnot("legends must be logical" = identical(class(legends), "logical"))
  stopifnot("loi must be logical" = identical(class(loi), "logical"))

  #Check labels inputs
  stopifnot("labels must be logical" = identical(class(labels), "logical"))

  #Check by inputs
  stopifnot("by must be as integer" = ds4psy::is_wholenumber(by))

  #Check pos inputs
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
    lapply(sts.aux, plotTrack, all_basin)
    if (labels)
      lapply(sts.aux, plotLabels, by, pos)
  } else{
    for(n in names){
      plotTrack(sts.aux[[n]], all_basin)
      if (labels)
        plotLabels(sts.aux[[n]], by, pos)
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


