



#' Get the right color associated with a wind observation
#'
#' This function returns the Saffir Simpson Hurricane Scale color associated
#' with a maximum sustained wind speed
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#'
#' @return color associated with the observation
getColors <- function(msw) {

  if (is.na(msw)) {
    color <- NA

  } else if (msw < sshs[1]) {
    color <- sshsPalette[1]

  } else if (msw >= sshs[1] & msw < sshs[2]) {
    color <- sshsPalette[2]

  } else if (msw >= sshs[2] & msw < sshs[3]) {
    color <- sshsPalette[3]

  } else if (msw >= sshs[3] & msw < sshs[4]) {
    color <- sshsPalette[4]

  } else if (msw >= sshs[4] & msw < sshs[5]) {
    color <- sshsPalette[5]

  } else if (msw >= sshs[5] & msw < sshs[6]) {
    color <- sshsPalette[6]

  } else if (msw >= sshs[6]) {
    color <- sshsPalette[7]
  }

  return(color)
}





#' Plot track of a a storm
#'
#' This function plots the track of a storm on a map that should be previously plotted
#'
#' @noRd
#' @param st Storm object
#'
#' @return NULL
plotTrack <- function(st) {

  cex <- 1
  lon <- st@obs.all$lon
  lat <- st@obs.all$lat
  msw <- st@obs.all$msw
  colors <- unlist(lapply(msw, getColors))

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
plotLabels <- function(st, by, pos) {

  cex <- 0.6
  ind <- round(seq(1, st@numobs.all, by))

  for (i in ind) {
    lon <- st@obs.all$lon[i]
    lat <- st@obs.all$lat[i]

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






#' Check inputs for plotStorms function
#'
#' @noRd
#' @param sts Storms object
#' @param names character vector
#' @param category numeric vector
#' @param map shapefile or sf object
#' @param labels logical
#' @param by numeric
#' @param pos numeric
#' @param legends logical
#' @param loi logical
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @param reset_setting logical
#' @return NULL
checkInputsPs <- function(sts, names, category, map, labels, by,
                          pos, legends, loi, xlim, ylim, reset_setting){

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
  if (!is.null(map))
    stopifnot(
      "map must be a SpatialPolygonsDataFrame or a sf object" = identical(class(map)[1], "SpatialPolygonsDataFrame") |
        identical(class(map)[1], "sf"))

  #Checking xlim input
  if (!is.null(xlim)) {
    stopifnot("xlim must be numeric" = identical(class(xlim), "numeric"))
    stopifnot("xlim must length 2" = length(xlim) == 2)
    stopifnot("xlim must have valid longitude coordinates" = xlim >= 0 &
                xlim <= 360)
  }

  #Checking ylim input
  if (!is.null(ylim)) {
    stopifnot("ylim must be numeric" = identical(class(ylim), "numeric"))
    stopifnot("ylim must length 2" = length(ylim) == 2)
    stopifnot("ylim must have valid latitude coordinates" = ylim >= -90 &
                ylim <= 90)
  }

  #Checking logical inputs
  stopifnot("legends must be logical" = identical(class(legends), "logical"))
  stopifnot("loi must be logical" = identical(class(loi), "logical"))
  stopifnot("labels must be logical" = identical(class(labels), "logical"))
  stopifnot("reset_setting must be logical" = identical(class(reset_setting), "logical"))

  #Checking by input
  stopifnot("by must be numeric" = identical(class(by), "numeric"))
  stopifnot("by must length 1" = length(by) == 1)
  stopifnot("by must be as integer" = round(by) == by)

  #Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos), "numeric"))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between either 1, 2, 3 or 4" = pos %in% c(1, 2, 3, 4))


}





#' Plot several storm tracks
#'
#' This function plots a set of storm tracks contained in a Storms object (see “getStorms” function).
#' Depending on the inputs, the user can choose to plot only a desired set of storms
#' extracted from the Storms object .
#'
#' @param sts Storms object
#' @param names character vector. Name(s) of the storm(s) to plot on map. Default
#' value is set to NULL, which will plot every storm in sts. To see which storms
#' are included in a Storms object, you can use "sts@names"
#' @param category numeric vector. Should be either a category or a range of categories
#' in the Saffir Simpson scale (-2 to 5). Default value is set to NULL which
#' will consider every storm in sts. Otherwise it will consider only storms that
#' reached category input
#' @param map a shapefile or sf object. It will replace the default map if non NULL.
#'  Default value is set to NULL.
#' @param loi logical. Whether or not to plot spatial.loi.buffer on the map
#' Default value is set to TRUE.
#' @param labels logical. Whether or not to plot ISO Times and name labels
#' @param by numeric. Defines the frequency at which labels are plotted for the
#' 3-hourly records. Default value is set to 8 which represents a 24h time interval
#' between each labeled observations. Ignored if labels == FALSE
#' @param pos numeric. Must be between 1 and 4. Correspond to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right).
#' Default value is set to 3. Ignored if labels == FALSE
#' @param legends logical. Whether or not to plot legends. Default value is set
#' to TRUE
#' @param xlim numeric vector. A set of longitude coordinates that controls the
#' longitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the x bounding box of spatial.loi.buffer from sts input.
#' @param ylim numeric vector. A set of latitude coordinates that controls the
#' latitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the x bounding box of spatial.loi.buffer from sts input.
#' @param reset_setting logical. Whether the graphical parameter should be reset on exit. Default
#' value is set to TRUE. It is usefull for the plotBehaviour function. We highly recommand not to change this
#' input.
#' @returns NULL
#' @import rworldxtra
#'
#' @examples
#' #Plot category 5 TCs in the WP Basin between 2010 and 2020
#' plotStorms(sts_wp, category = c(3,5))
#'
#' #Plot a single storm (ERICA) with labels every 24h
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE)
#'
#' #Plot a single storm (ERICA), with labels every 6h on the right side
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE, by = 2, pos = 4)
#'
#' #Plot a single storm (ERICA)
#' plotStorms(sts_nc, names = "ERICA")
#'
#'
#' @export
plotStorms <- function(sts, names = NULL, category = NULL, map = NULL, labels = FALSE,
                       by = 8, pos = 3, legends = TRUE, loi = TRUE, xlim = NULL,
                       ylim = NULL, reset_setting = TRUE){


  checkInputsPs(sts, names, category, map, labels, by, pos, legends,
                loi, xlim, ylim, reset_setting)


  if (!is.null(map)){
    #Converting to sf object
    if(!identical(class(map)[1], "sf"))
      map <- sf::st_as_sf(map)
  }


  #Handling spatial extent
  if(is.null(map)){
    ext <- terra::ext(sf::st_bbox(sts@spatial.loi.buffer)$xmin,
                      sf::st_bbox(sts@spatial.loi.buffer)$xmax,
                      sf::st_bbox(sts@spatial.loi.buffer)$ymin,
                      sf::st_bbox(sts@spatial.loi.buffer)$ymax)
  }else{
    ext <- terra::ext(sf::st_bbox(map)$xmin, sf::st_bbox(map)$xmax,
                      sf::st_bbox(map)$ymin, sf::st_bbox(map)$ymax)
  }


  if (!is.null(xlim)) {
    xlim <- xlim[order(xlim)]
    ext$xmin <- xlim[1]
    ext$xmax <- xlim[2]
  }

  if (!is.null(ylim)) {
    ylim <- ylim[order(ylim)]
    ext$ymin <- ylim[1]
    ext$ymax <- ylim[2]
  }



  #Plotting base map
  #grDevices::dev.new(width = 10, height = 9)
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(5, 4, 4, 6))

  if(is.null(map)){
    world <- rworldmap::getMap(resolution <- "high")
    maps::map(
      world,
      fill = TRUE,
      col = groundColor,
      bg = oceanColor,
      wrap = c(0, 360),
      xlim = c(ext$xmin, ext$xmax),
      ylim = c(ext$ymin, ext$ymax)
    )
    maps::map.axes(cex.axis = 1)

  }else{
    plot(map,
         col = groundColor,
         bg = oceanColor,
         border = 1,
         xlim = c(ext$xmin, ext$xmax),
         ylim = c(ext$ymin, ext$ymax),
         axes = T)
  }

  #Plotting loi
  if (loi)
    plot(sts@spatial.loi.buffer, lwd = 2, add = T)

  if(as.character(sf::st_geometry_type(sts@spatial.loi)) == "POINT")
    plot(sts@spatial.loi, lwd = 2, pch = 4, add = T)



  #Handling categories
  if(!is.null(category) & is.null(names)){
    if(length(category) == 2){
      category <- category[order(category)]
      cat.inf <- category[1]
      cat.sup <- category[2]
      ind <- which(sts@sshs >= cat.inf & sts@sshs <= cat.sup)

    }else{
      #length category == 1
      ind <- which(sts@sshs == category)
    }
    sts.aux <- unlist(sts@data)[ind]

  }else{
    sts.aux <- sts@data
  }

  #Plotting track(s) and labels
  if (is.null(names)) {
    lapply(sts.aux, plotTrack)
    if (labels)
      lapply(sts.aux, plotLabels, by, pos)

  } else{
    for(n in names){
      plotTrack(sts.aux[[n]])
      if (labels)
        plotLabels(sts.aux[[n]], by, pos)
    }
  }

  #Adding legends
  if (legends) {

    graphics::legend(
      x = "topright",
      inset = c(-0.7, 0),
      xpd = TRUE,
      legend = c(
        expression(paste("Tropical Depression (below 17 m.s" ^ "-1",")")),
        expression(paste("Tropical Storm (18 to 32 m.s" ^ "-1",")")),
        expression(paste("Category 1 (33 to 42 m.s" ^ "-1",")")),
        expression(paste("Category 2 (43 to 49 m.s" ^ "-1",")")),
        expression(paste("Category 3 (50 to 58 m.s" ^ "-1",")")),
        expression(paste("Category 4 (58 to 70 m.s" ^ "-1",")")),
        expression(paste("Category 5 (70 m.s" ^ "-1","and higher)"))),
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
      cex = 0.8
    )
  }

  if(reset_setting)
    on.exit(graphics::par(opar))

}
