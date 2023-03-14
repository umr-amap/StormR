




#' Get the right color associated with a wind observation
#'
#' This function returns the Saffir Simpson Hurricane Scale color associated
#' with a maximum sustained wind speed
#'
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





#' Plot track of a storm
#'
#' This function plots the track of a storm on a map that should be previously
#' plotted
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
    lty = 2,
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
#' Add names labels, indices of observations and ISO Times for a given storm on
#' a map that should be previsouly plotted
#'
#' @noRd
#' @param st Storm object
#' @param by numeric. Increment of the sequence for the labels to plot
#' @param pos numeric. Must be between 1 and 4. Corresponds to the position of
#'   labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right)
#'
#' @return NULL
plotLabels <- function(st, by, pos) {

  cex <- 0.6
  ind <- round(seq(1, getNbObs(st), by))

  for (i in ind) {
    lon <- st@obs.all$lon[i]
    lat <- st@obs.all$lat[i]

    graphics::text(lon,
                   lat,
                   labels = paste0(st@name," (",i,")\n", st@obs.all$iso.time[i]),
                   pos = pos,
                   cex = cex)
  }

}





#' Check inputs for plotStorms function
#'
#' @noRd
#' @param sts StormsList object
#' @param names character vector
#' @param category numeric vector
#' @param labels logical
#' @param by numeric
#' @param pos numeric
#' @param legends logical
#' @param loi logical
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @return NULL
checkInputsPs <- function(sts, names, category, labels, by,
                          pos, legends, loi, xlim, ylim){

  #Checking sts input
  stopifnot("no data to plot" = !missing(sts))

  #Checking names input
  if (!is.null(names)) {
    stopifnot("names must be characters" = identical(class(names), "character"))
    stopifnot("Invalid storm names (storm not found)" = names %in% getNames(sts))
  }

  #Checking category input
  if (!is.null(category)) {
    stopifnot("category must be numeric(s)" = identical(class(category), "numeric"))
    stopifnot("Invalid category input" = category %in% c(-1,-2,0,1,2,3,4,5))
  }

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

  #Checking by input
  stopifnot("by must be numeric" = identical(class(by), "numeric"))
  stopifnot("by must length 1" = length(by) == 1)
  stopifnot("by must be as integer" = round(by) == by)

  #Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos), "numeric"))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between either 1, 2, 3 or 4" = pos %in% c(1, 2, 3, 4))


}





#' Plot storm track(s)
#'
#' This function plots a set of storm tracks contained in a `StormsList` object
#' (See `Storms` function). Depending on the inputs, the user can choose to plot
#' only a desired set of storms extracted from the `StormsList` object
#'
#' @param sts `StormsList` object
#' @param names character vector. Name(s) of the storm(s) in capital letters to
#'   plot on the map. Default value is set to `NULL`, which will plot every
#'   storm provided in `sts`. To see which storms are included in a `StormsList`
#'   object, you can use the `getNames` function
#' @param category numeric vector. Should be either a category or a range of
#'   categories in the Saffir Simpson scale (-1 to 5). Default value is set to
#'   `NULL` which will consider every storm in `sts`. Otherwise it will consider
#'   only storms that reached `category` input
#' @param xlim numeric vector. A set of longitude coordinates that controls the
#'   longitude extent of the plot. Default value is set to `NULL` which will let
#'   the plot extends according to the longitude range of the extended LOI
#' @param ylim numeric vector. A set of latitude coordinates that controls the
#'   latitude extent of the plot. Default value is set to `NULL` which will let
#'   the plot extends according to the the latitude range of the extended LOI
#' @param labels logical. Whether or not to plot names labels with the
#'   corresponding indices of observations and ISO Times along the track(s).
#'   Default value is set to `NULL`
#' @param by numeric. Defines the frequency at which labels are plotted for the
#'   3 (or 6) hourly records. Default value is set to `8` which represents a 24h
#'   (or 48h) time interval between each labeled observations. Ignored if
#'   `labels == FALSE`
#' @param pos numeric. Must be between `1` and `4`. Correspond to the position
#'   of labels according to the observation: `1` (up), `2` (left), `3` (down),
#'   `4` (right). Default value is set to 3. Ignored if labels == FALSE
#' @param legends logical. Whether or not to plot legends. Default value is set
#'   to `TRUE`
#' @param loi logical. Whether or not to plot the extended LOI on the map.
#'   Default value is set to `TRUE`
#' @import rworldxtra
#'
#' @examples
#' \dontrun{
#' #Plot Erica over New Caledonia with labels every 24h
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE)
#'
#' #Plot Erica, with labels every 6h on the right side of observations
#' plotStorms(sts_nc, names = "ERICA", labels = TRUE, by = 2, pos = 4)
#'
#' }
#' 
#'
#' @export
plotStorms <- function(sts,
                       names = NULL,
                       category = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       labels = FALSE,
                       by = 8,
                       pos = 3,
                       legends = TRUE,
                       loi = TRUE){


  checkInputsPs(sts, names, category, labels, by, pos, legends,
                loi, xlim, ylim)


  #Handling spatial extent
  ext <- terra::ext(sf::st_bbox(sts@spatial.loi.buffer)$xmin,
                    sf::st_bbox(sts@spatial.loi.buffer)$xmax,
                    sf::st_bbox(sts@spatial.loi.buffer)$ymin,
                    sf::st_bbox(sts@spatial.loi.buffer)$ymax)

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

  #Handling categories and names
  if(!is.null(category) & is.null(names)){

    if(length(category) == 2){
      category <- category[order(category)]
      cat.inf <- category[1]
      cat.sup <- category[2]
      ind <- which(getSSHS(sts) >= cat.inf & getSSHS(sts) <= cat.sup)

    }else{
      #length category == 1
      ind <- which(getSSHS(sts) == category)
    }

    sts.aux <- unlist(sts@data)[ind]

  }else{

    if(!is.null(names)){
      if(!is.null(category))
        warning("category input ignored\n")

      ind <- which(getNames(sts) %in% names)
      sts.aux <- unlist(sts@data)[ind]

    }else{
      sts.aux <- sts@data
    }
  }

  #Plotting base map
  world <- rworldmap::getMap(resolution = "high")
  maps::map(world,
            fill = TRUE,
            col = groundColor,
            bg = oceanColor,
            wrap = c(0, 360),
            xlim = c(ext$xmin-1, ext$xmax+1),
            ylim = c(ext$ymin-1, ext$ymax+1))
  maps::map.axes(cex.axis = 1)

  #Plotting loi
  if (loi)
    plot(sts@spatial.loi.buffer, lwd = 1, border = "blue", add = T)

  if(loi & as.character(sf::st_geometry_type(sts@spatial.loi)) == "POINT")
    plot(sts@spatial.loi, lwd = 2, col = "blue", pch = 4, add = T)

  #Plotting track(s) and labels
  lapply(sts.aux, plotTrack)
  if(labels)
    lapply(sts.aux, plotLabels, by, pos)

  #Adding legends
  if(legends) {

    leg <- c(expression(paste("TD (< 18 m.s" ^ "-1",")")),
             expression(paste("TS (18 to 32 m.s" ^ "-1",")")),
             expression(paste("Cat. 1 (33 to 42 m.s" ^ "-1",")")),
             expression(paste("Cat. 2 (43 to 49 m.s" ^ "-1",")")),
             expression(paste("Cat. 3 (50 to 58 m.s" ^ "-1",")")),
             expression(paste("Cat. 4 (58 to 70 m.s" ^ "-1",")")),
             expression(paste("Cat. 5 ( >= 70 m.s" ^ "-1",")")))

    col <- c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")

    lty <- rep(0,7)
    pch <- rep(19,7)
    lwd <- rep(1,7)

    if (loi){
      if(loi & as.character(sf::st_geometry_type(sts@spatial.loi)) == "POINT"){
        leg <- c(leg, "LOI")
        col <- c(col, "blue")
        lty <- c(lty, 0)
        pch <- c(pch,4)
        lwd <- c(lwd, 2)
      }
      leg <- c(leg, "LOI buffer")
      col <- c(col, "blue")
      lty <- c(lty, 1)
      pch <- c(pch, NA)
      lwd <- c(lwd, 1)
    }

    #Handling inset
    coord <- graphics::par("usr")
    inset <- (coord[2] - coord[1]) * 0.05


    graphics::legend(x = coord[2] + inset,
                     y = coord[4],
                     xpd = T,
                     legend = leg,
                     col = col,
                     lty = lty,
                     pch = pch,
                     lwd = lwd,
                     cex = 0.8)

  }
  
}
