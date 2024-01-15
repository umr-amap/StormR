




#' Get the right color associated with a wind observation
#'
#' This function returns the Saffir Simpson Hurricane Scale color associated
#' with a maximum sustained wind speed
#'
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param scale list of values that defines the scale intensity of the storm, e.g. `sshs`
#' @param palette list of colors that defines the scale intensity of the storm
#' @return color associated with the observation
getColors <- function(msw, scale, palette) {
  if (is.na(msw)) {
    color <- NA
  } else {
    i <- findInterval(msw, scale)
    color <- palette[i + 1]
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
#' @param scale numeric vector. Thresholds used for the scale
#' @param scale named character vector. colors used for the scale
#'
#' @return A plot with the track of the storm
plotTrack <- function(st, scale, scalePalette) {
  cexL <- 1
  cexP <- 0.6
  lon <- st@obs.all$lon
  lat <- st@obs.all$lat
  msw <- st@obs.all$msw
  colors <- unlist(lapply(msw, getColors, scale = scale, palette = scalePalette))

  graphics::lines(
    lon,
    lat,
    col = "black",
    lty = 2,
    lwd = 1,
    cex = cexL
  )

  graphics::points(
    lon,
    lat,
    col = colors,
    pch = 19,
    lwd = 1,
    cex = cexP
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
#' @return A plot with added labels
plotLabels <- function(st, by, pos) {
  cex <- 0.5
  ind <- round(seq(1, getNbObs(st), by))

  for (i in ind) {
    lon <- st@obs.all$lon[i]
    lat <- st@obs.all$lat[i]

    graphics::text(lon,
      lat,
      labels = paste0(st@name, " (", i, ")\n", st@obs.all$iso.time[i]),
      pos = pos,
      cex = cex
    )
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
#' @param dynamicPlot logical
#' @return NULL, just stops the function if an error is found
checkInputsPlotStorms <- function(sts, names, category, labels, by,
                                  pos, legends, loi, xlim, ylim, dynamicPlot) {
  # Checking sts input
  stopifnot("no data to plot" = !missing(sts))
  
  # Checking names input
  if (!is.null(names)) {
    stopifnot("names must be characters" = identical(class(names), "character"))
    stopifnot("Invalid storm names (storm not found)" = names %in% getNames(sts))
  }

  # Checking category input
  if (!is.null(category)) {
    stopifnot("category must be numeric(s)" = identical(class(category), "numeric"))
    stopifnot("Invalid category input" = category %in% seq(0, length(sts@scale)))
  }

  # Checking xlim input
  if (!is.null(xlim)) {
    stopifnot("xlim must be numeric" = identical(class(xlim), "numeric"))
    stopifnot("xlim must length 2" = length(xlim) == 2)
    stopifnot("xlim must have valid longitude coordinates" = xlim >= 0 &
                xlim <= 360)
  }

  # Checking ylim input
  if (!is.null(ylim)) {
    stopifnot("ylim must be numeric" = identical(class(ylim), "numeric"))
    stopifnot("ylim must length 2" = length(ylim) == 2)
    stopifnot("ylim must have valid latitude coordinates" = ylim >= -90 &
                ylim <= 90)
  }

  # Checking logical inputs
  stopifnot("loi must be logical" = identical(class(loi), "logical"))
  stopifnot("labels must be logical" = identical(class(labels), "logical"))

  # Checking by input
  stopifnot("by must be numeric" = identical(class(by), "numeric"))
  stopifnot("by must length 1" = length(by) == 1)
  stopifnot("by must be as integer" = round(by) == by)

  # Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos), "numeric"))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between either 1, 2, 3 or 4" = pos %in% c(1, 2, 3, 4))

  # Checking legend
  stopifnot("legends must be character" = identical(class(legends), "character"))
  stopifnot("legends must be length 1" = length(legends) == 1)
  stopifnot(
    "legends must be either topright, topleft, bottomleft, bottomright, or none" =
      legends %in% c("topright", "topleft", "bottomleft", "bottomright", "none")
  )

  #Checking mode input
  stopifnot("dynamicPlot must be logical" = identical(class(dynamicPlot), "logical"))
  stopifnot("dynamicPlot must length 1" = length(dynamicPlot) == 1)
}





#' Plotting storm track data
#'
#' This `plotStorms()` function allows plotting storm track data stored in a `StormsList` object.
#'
#' @param sts `StormsList` object
#' @param names character vector. Name(s) of the storm(s) in capital letters.
#'  If `names = NULL` (default setting), all storms are plotted.
#' @param category numeric vector. Category of storms to be plotted among the level in
#'  the windscale provided in `sts` input. If `category=NULL` (default setting),
#'  all storms are plotted.
#' @param xlim numeric vector. The x limits of the plot.
#' @param ylim numeric vector. The y limits of the plot.
#' @param labels logical. Whether (TRUE) or not (FALSE, default setting) to add labels with the name
#' of the storm and the indices and ISO times of the observation.
#' @param by numeric. If `labels=TRUE`, defines the frequency at which labels are plotted.
#' Default value is set to `8` which corresponds to a 24h (or 48h) time interval between the labelled observations
#' when observations are made every 3 (or 6) hours.
#' @param pos numeric. If `labels=TRUE`, defines the position of the labels, `1` (above the observation),
#'  `2` (on the left), `3` (below, default setting), and `4` (on the right).
#' @param legends character. Indicates where to plot the legend, `"topright"`(default setting), `"topleft"`,
#' `"bottomleft"`, `"bottomright"`, or `"none"` (legend not plotted).
#' @param loi logical. Whether (TRUE, default setting) or not (FALSE) to plot the
#' extent of the buffered location of interest on the map.
#' @param dynamicPlot logical. Whether (FALSE, default setting) or (TRUE) to plot the 
#' data dynamicaly using leaflet library
#'
#' @return A plot of the storm track data.
#' 
#' @examples
#' #' #Creating a stormsDataset
#' \donttest{
#' dev.off()
#' sds <- defStormsDataset()
#'
#' # Getting storm track data for tropical cyclone Pam (2015)
#' pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#'
#' # Plotting Pam over Vanuatu with labels every 24h
#' plotStorms(sts =pam, labels = TRUE)
#'
#' # Plotting Pam over Vanuatu with labels every 6h on the right side of the observations
#' plotStorms(pam, labels = TRUE, by = 2, pos = 4)
#'
#' # dynamicPlot mode
#  # plotStorms(pam, dynamicPlot=TRUE)
#'
#' }
#' @export
plotStorms <- function(sts,
                       names = NULL,
                       category = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       labels = FALSE,
                       by = 8,
                       pos = 3,
                       legends = "topright",
                       loi = TRUE,
                       dynamicPlot = FALSE) {

  checkInputsPlotStorms(sts, names, category, labels, by, pos, legends,loi, xlim, ylim, dynamicPlot)


  # Handling spatial extent
  ext <- terra::ext(
    sf::st_bbox(sts@spatialLoiBuffer)$xmin,
    sf::st_bbox(sts@spatialLoiBuffer)$xmax,
    sf::st_bbox(sts@spatialLoiBuffer)$ymin,
    sf::st_bbox(sts@spatialLoiBuffer)$ymax
  )

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

  # Handling categories and names
  if (!is.null(category) && is.null(names)) {
    if (length(category) == 2) {
      category <- category[order(category)]
      catInf <- category[1]
      catSup <- category[2]
      ind <- which(getScale(sts) >= catInf & getScale(sts) <= catSup)
    } else {
      # length category == 1
      ind <- which(getScale(sts) == category)
    }

    stsAux <- unlist(sts@data)[ind]
  } else {
    if (!is.null(names)) {
      if (!is.null(category)) {
        warning("category input ignored\n")
      }

      ind <- which(getNames(sts) %in% names)
      stsAux <- unlist(sts@data)[ind]
    } else {
      stsAux <- sts@data
    }
  }

  if (!dynamicPlot) {

    # Plotting base map
    world <- rworldmap::getMap(resolution = "high")

    maps::map(world,
      fill = TRUE,
      col = groundColor,
      bg = oceanColor,
      wrap = c(0, 360),
      xlim = c(ext$xmin - 1, ext$xmax + 1), # we extend W & E by 1°
      ylim = c(ext$ymin - 1, ext$ymax + 1)
    ) # we extend S & N by 1°
    maps::map.axes(cex.axis = 1)


    # Plotting loi
    if (loi) {
      plot(sts@spatialLoiBuffer, lwd = 1, border = "blue", add = TRUE)
    }

    if (loi && as.character(sf::st_geometry_type(sts@spatialLoi)) == "POINT") {
      plot(sts@spatialLoi, lwd = 2, col = "blue", pch = 4, add = TRUE)
    }

    # Plotting track(s) and labels
    lapply(stsAux, plotTrack, sts@scale, sts@scalePalette)
    if (labels) {
      lapply(stsAux, plotLabels, by, pos)
    }
    
    # Adding legends
    if (legends != "none") {
      
      
      leg <- names(sts@scalePalette)
      col <- sts@scalePalette
      
      lty <- rep(0, length(col))
      pch <- rep(19, length(col))
      lwd <- rep(1, length(col))
      
      graphics::legend(legends,
                       title = "Scale",
                       legend = leg,
                       col = col,
                       lty = lty,
                       pch = pch,
                       lwd = lwd,
                       cex = 0.6,
                       bty = "n"
      )
    }

  }else {
    #Init map

    map <- leaflet::fitBounds(
      leaflet::addProviderTiles(
        leaflet::leaflet(options =
                           leaflet::leafletOptions(worldCopyJump = F,
                                                   minZoom = 2,
                                                   maxZoom = 12),
                         width = 650,
                         height = 700),
        leaflet::providers$Esri.NatGeoWorldMap,
        group = "Satellite",
        options = leaflet::providerTileOptions(errorTileUrl = "Tile not found")),
      lng1 = as.numeric(ext$xmin),
      lng2 = as.numeric(ext$xmax),
      lat1 = as.numeric(ext$ymin),
      lat2 = as.numeric(ext$ymax)
      )

    #Plotting loi
    if(loi)
      map <- leaflet::addPolylines(map,
                                   data = sts@spatialLoiBuffer,
                                   fillColor = "transparent",
                                   label = "Buffer limit")

    if(loi & as.character(sf::st_geometry_type(sts@spatialLoi)) == "POINT")
      map <- leaflet::addCircleMarkers(map,
                                       data = sts@spatialLoi,
                                       fillColor = "transparent",
                                       label = "LOI")


    #Plotting track(s) and labels
    for (st in stsAux) {

      data <- st@obs.all
      data$type <- unlist(lapply(data$msw,getColors, sts@scale, sts@scalePalette))

      labels <- paste0("<b>",st@name, "</b><br>",
                       "<i>observation ",row.names(data), "</i><br>",
                       data$iso.time,
                       "<ul>",
                       "<li>", "scale:  ", data$scale, "</li>",
                       "<li>", "msw:  ", data$msw, "m/s</li>",
                       "<li>", "rmw:  ", data$rmw, "km</li>",
                       "<li>", "pressure:  ", data$pressure, "pa</li>",
                       "<li>", "poci:  ", data$poci, "pa</li>",
                       "</ul>")

      #Plot track
      map <- leaflet::addPolylines(map,
                                   data = data,
                                   lng = ~lon,
                                   lat = ~lat,
                                   color = "black",
                                   weight = 2)


      map <- leaflet::addCircleMarkers(map,
        data = data,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = ~type,
        stroke = FALSE,
        fillOpacity = 1,
        popup = labels
      )
    }

    #Adding legends
    map <- leaflet::addLegend(map,
                              legends,
                              colors = sts@scalePalette,
                              labels = names(sts@scalePalette),
                              title = "Scale",
                              opacity = 1)

    map

    return(map)
  }


}
