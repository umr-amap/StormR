



#' Get the right color associated with a wind observation
#'
#' This function returns the Saffir Simpson Hurricane Scale color associated
#' with a maximum sustained wind speed
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#'
#' @return color associated with the observation
getColors = function(msw) {

  if (is.na(msw)) {
    color = NA

  } else if (msw < sshs[1]) {
    color = sshsPalette[1]

  } else if (msw >= sshs[1] & msw < sshs[2]) {
    color = sshsPalette[2]

  } else if (msw >= sshs[2] & msw < sshs[3]) {
    color = sshsPalette[3]

  } else if (msw >= sshs[3] & msw < sshs[4]) {
    color = sshsPalette[4]

  } else if (msw >= sshs[4] & msw < sshs[5]) {
    color = sshsPalette[5]

  } else if (msw >= sshs[5] & msw < sshs[6]) {
    color = sshsPalette[6]

  } else if (msw >= sshs[6]) {
    color = sshsPalette[7]
  }

  return(color)
}





#' Plot track of a a storm
#'
#' This function plots the track of a storm on a map that should be previously plotted
#'
#' @noRd
#' @param st Storm object
#' @param whole_basin logical. Whether or not to plot the track onto the whole basin.
#' Default value is set to `FALSE`. Otherwise, the plot focuses on the extent of
#' `spatial.loi.buffer` of the Storms object in which st belongs
#'
#' @return NULL
plotTrack = function(st, whole_basin) {

  if (whole_basin) {
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




#' Add labels on plot
#'
#' Add ISO Times and name labels on the track of a storm on a map that should be
#' previsouly plotted
#' @noRd
#' @param st Storm object
#' @param by numeric. Increment of the sequence for the labels to plot
#' @param pos numeric. Must be between 1 and 4. Corresponds to the position of
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





#' Plot several storm tracks
#'
#' This function plots a set of storm tracks contained in a Storms object. Depending
#' on the inputs, the user may choose to plot only a desired set of storm extracted
#' from the Storms object.
#'
#' @param sts Storms object
#' @param names character vector. Name(s) of the storm(s) to plot on map. Default
#' value is set to NULL which will consider every storm in sts
#' @param category numeric vector. Should be either a category or a range of category
#' in the Saffir Simpson scale (-2 to 5). Default value is set to NULL which
#' will consider every storm in sts. Otherwise it will consider only storms that
#' reached category input
#' @param map a shapefile or sf object. It should replace the default map if non NULL.
#'  Default value is set to NULL.
#' @param ground_color character. Color for the ground
#' @param ocean_color character. Color for the oceans
#' @param whole_basin logical. Whether or not to plot the track onto the whole basin.
#' Default value is set to FALSE. Otherwise, the plot focuses on the extent of
#' spatial.loi.buffer of sts
#' @param loi logical. Whether or not to plot spatial.loi.buffer on the map
#' Default value is set to TRUE.
#' @param labels logical. Whether or not to plot ISO Times and name labels
#' @param by numeric. Increment of the sequence for the labels to plot. Default value
#' is set to 8 which represents a 24h time interval
#' @param pos numeric. Must be between 1 and 4. Correspond to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right).
#' Default value is set to 3
#' @param legends logical. Whether or not to plot legends. Default value is set
#' to FALSE.
#' @param grtc numeric. Controls the number of graticules to plot. Default value
#' is set to 1, which plots graticules on multiples of 10 coordinates. Note that
#' it should be a power of 2.
#' @param xlim numeric vector. A set of longitude coordinates that controls the
#' longitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the x bounding box of spatial.loi.buffer from sts input.
#' Ignored if whole_basin is not NULL
#' @param ylim numeric vector. A set of latitude coordinates that controls the
#' latitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the x bounding box of spatial.loi.buffer from sts input.
#' Ignored if whole_basin is not NULL
#' @returns NULL
#' @importFrom ds4psy is_wholenumber
#' @import rworldxtra
#'
#' @examples
#' #Plot category 5 TCs in the WP Basin between 2010 and 2020
#' plotStorms(sts_wp, category = c(3,5))
#'
#' #Plot a single storm (ERICA) with lables every 24h and legends
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE, legend = TRUE)
#'
#' #Plot a single storm (ERICA), with labels every 6h on the right side
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE, by = 2, pos = 4)
#'
#' #Plot a single storm (ERICA), with graticules at each multiples of 2
#' #longitude/latitude coordinates
#' plotStorms(sts_nc, names = "ERICA", grtc = 4)
#'
#'
#' @export
plotStorms = function(sts, names = NULL, category = NULL, map = NULL, ground_color = "grey",
                      ocean_color = "white", whole_basin = FALSE, labels = FALSE, by = 8,
                      pos = 3, legends = FALSE, loi = TRUE, grtc = 1, xlim = NULL, ylim = NULL){


  #Checking sts input
  stopifnot("no data to plot" = !missing(sts))


  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be characters" = identical(class(names), "character"))
    stopifnot("Invalid storm names (storm not found)" = names %in% sts@names)
  }

  #Checking category input
  if (!is.null(category)) {
    stopifnot("category must be numeric(s)" = identical(class(category), "numeric"))
    stopifnot("Invalid category input" = category %in% c(-1,-2,0,1,2,3,4,5))
  }

  #Checking map input
  if (!is.null(map)){
    stopifnot(
      "map must be a SpatialPolygonsDataFrame or a sf object" = identical(class(map)[1], "SpatialPolygonsDataFrame") |
        identical(class(map)[1], "sf"))
    #Converting to sf object
    if(!identical(class(map)[1], "sf"))
      map = sf::st_as_sf(map)
  }



  #Checking grtc input
  stopifnot("grtc must be numeric" = identical(class(grtc), "numeric"))
  stopifnot("grtc must contains an integer" = is_wholenumber(grtc))
  stopifnot("grtc must be length 1" = length(grtc) == 1)

  #Checking xlim input
  if (!is.null(xlim)) {
    stopifnot("xlim must be numeric" = identical(class(xlim), "numeric"))
    stopifnot("xlim must length 2" = length(xlim) == 2)
    xlim = xlim[order(xlim)]
    stopifnot("xlim must have valid longitude coordinates" = xlim >= 0 &
                xlim <= 360)
  }

  #Checking ylim input
  if (!is.null(ylim)) {
    stopifnot("ylim must be numeric" = identical(class(ylim), "numeric"))
    stopifnot("ylim must length 2" = length(ylim) == 2)
    ylim = ylim[order(ylim)]
    stopifnot("ylim must have valid latitude coordinates" = ylim >= -90 &
                ylim <= 90)
  }

  #Checking logical input
  stopifnot("whole_basin must be logical" = identical(class(whole_basin), "logical"))
  stopifnot("legends must be logical" = identical(class(legends), "logical"))
  stopifnot("loi must be logical" = identical(class(loi), "logical"))

  #Checking labels input
  stopifnot("labels must be logical" = identical(class(labels), "logical"))

  #Checking by input
  stopifnot("by must be numeric" = identical(class(by), "numeric"))
  stopifnot("by must be as integer" = ds4psy::is_wholenumber(by))
  stopifnot("by must length 1" = length(by) == 1)

  #Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos), "numeric"))
  stopifnot("pos must be as integer" = ds4psy::is_wholenumber(pos))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between 1 and 4" = pos >= 1 & pos <= 4)




  #Checking grtc input
  l2 = log2(grtc)
  if (!is_wholenumber(l2)) {
    grtc = 2 ** round(l2)
    warning(paste("grtc is not a power of 2, set to", grtc))
  }

  #Handling spatial extent
  if (whole_basin) {
    ext = switch(sts@basin,
                 "SP" = terra::ext(135,290,-60,0),
                 "SI" = terra::ext(10,135,-60,0),
                 "SA" = terra::ext(290,359,-60,0),
                 "NI" = terra::ext(30,100,0,30),
                 "WP" = terra::ext(100,180,0,60),
                 "EP" = terra::ext(180,290,0,60),
                 "NA" = terra::ext(270,359,0,60),
                 "ALL" = terra::ext(0,359,-60,60)
                 )

    if (!is.null(xlim))
      warning("xlim ignored")

    if (!is.null(ylim))
      warning("ylim ignored")

  } else{

    if(is.null(map)){
      ext = terra::ext(sf::st_bbox(sts@spatial.loi.buffer)$xmin,
                       sf::st_bbox(sts@spatial.loi.buffer)$xmax,
                       sf::st_bbox(sts@spatial.loi.buffer)$ymin,
                       sf::st_bbox(sts@spatial.loi.buffer)$ymax)
    }else{
      ext = terra::ext(sf::st_bbox(map)$xmin,
                       sf::st_bbox(map)$xmax,
                       sf::st_bbox(map)$ymin,
                       sf::st_bbox(map)$ymax)
    }


    if (!is.null(xlim)) {
      ext$xmin = xlim[1]
      ext$xmax = xlim[2]
    }

    if (!is.null(ylim)) {
      ext$ymin = ylim[1]
      ext$ymax = ylim[2]
    }

  }

  #Plotting base map
  if(is.null(map)){
    world = rworldmap::getMap(resolution = "high")
    maps::map(
      world,
      fill = TRUE,
      col = ground_color,
      bg = ocean_color,
      wrap = c(0, 360),
      xlim = c(ext$xmin, ext$xmax),
      ylim = c(ext$ymin, ext$ymax)
    )
    maps::map.axes(cex.axis = 1)

  }else{
    plot(map,
         col = ground_color,
         bg = ocean_color,
         border = 1,
         xlim = c(ext$xmin, ext$xmax),
         ylim = c(ext$ymin, ext$ymax),
         axes = T)
  }


  #Adding graticules
  x.min = round(ext$xmin / 10) * 10 - 20
  x.max = round(ext$xmax / 10) * 10 + 20
  y.min = round(ext$ymin / 10) * 10 - 20
  y.max = round(ext$ymax / 10) * 10 + 20

  mapproj::map.grid(
    lim = c(x.min, x.max, y.min, y.max),
    nx = abs(x.max - x.min) / 10 * grtc,
    ny = abs(y.max - y.min) / 10 * grtc,
    col = "blue",
    labels = FALSE,
    lty = 3
  )

  #Plotting loi
  if (loi)
    plot(sts@spatial.loi.buffer, lwd = 2, add = T)


  #Handling categories
  if(!is.null(category) & is.null(names)){
    if(length(category) == 2){
      category = category[order(category)]
      cat.inf = category[1]
      cat.sup = category[2]
      ind = which(sts@sshs >= cat.inf & sts@sshs <= cat.sup)

    }else{
      #length category == 1
      ind = which(sts@sshs == category)
    }
    sts.aux = unlist(sts@data)[ind]

  }else{
    sts.aux = sts@data
  }

  #Plotting track(s) and labels
  if (is.null(names)) {
    lapply(sts.aux, plotTrack, whole_basin)
    if (labels)
      lapply(sts.aux, plotLabels, by, pos)

  } else{
    for(n in names){
      plotTrack(sts.aux[[n]], whole_basin)
      if (labels)
        plotLabels(sts.aux[[n]], by, pos)
    }
  }

  #Adding legends
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


