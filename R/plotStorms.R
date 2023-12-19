




#' Get the right color associated with a wind observation
#'
#' This function returns the Saffir Simpson Hurricane Scale color associated
#' with a maximum sustained wind speed
#'
#' @noRd
#' @param msw numeric. Maximum Sustained Wind (m/s)
#' @param scale list of values that defines the scale intensity of the storm, e.g. `sshs`
#'
#' @return color associated with the observation
getColors <- function(msw, scale) {
  if (is.na(msw)) {
    color <- NA
  } else {
    i <- findInterval(msw, scale)
    color <- sshsPalette[i + 1]
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
#' @return A plot with the track of the storm
plotTrack <- function(st) {
  cexL <- 1
  cexP <- 0.6
  lon <- st@obs.all$lon
  lat <- st@obs.all$lat
  msw <- st@obs.all$msw
  colors <- unlist(lapply(msw, getColors, scale = sshs))

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
#' @return NULL, just stops the function if an error is found
checkInputsPlotStorms <- function(sts, names, category, labels, by,
                                  pos, legends, loi, xlim, ylim) {
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
    stopifnot("Invalid category input" = category %in% c(-1, -2, 0, 1, 2, 3, 4, 5))
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
}





#' Plotting storm track data
#'
#' This `plotStorms()` function allows plotting storm track data stored in a `StormsList` object.
#'
#' @param sts `StormsList` object
#' @param names character vector. Name(s) of the storm(s) in capital letters.
#'  If `names = NULL` (default setting), all storms are plotted.
#' @param category numeric vector. Category of storms to be plotted in the Saffir-Simpson hurricane wind scale.
#'  Can be a single category or a range of categories among:
#' \itemize{
#'    \item -1, for tropical depression,
#'    \item 0, for tropical storms,
#'    \item 1, for category 1 tropical cyclone,
#'    \item 2, for category 2 tropical cyclone,
#'    \item 3, for category 3 tropical cyclone,
#'    \item 4, for category 4 tropical cyclone, or
#'    \item 5, for category 5 tropical cyclone.
#' }
#'  If `category=NULL` (default setting), all storms are plotted.
#' @param xlim numeric vector. The x limits of the plot.
#' @param ylim numeric vector. The y limits of the plot.
#' @param labels logical. Whether (TRUE) or not (FALSE, default setting) to add labels with the name
#' of the storm and the indices and ISO times of the observation.
#' @param by numeric. If `labels=TRUE`, defines the frequency at which labels are plotted.
#' Default value is set to `8` which corresponds to a 24h (or 48h) time interval between the labelled observations
#' when observations are made every 3 (or 6) hours.
#' @param pos numeric. If `labels=TRUE`, defines the position of the labels, `1` (above the observation),
#'  `2` (on the left), `3` (below, default setting), and `4` (on the right).
#' @param legends character. Indicates where to plot the legend, `"topright"`, `"topleft"` (default setting),
#' `"bottomleft"`, `"bottomright"`, or `"none"` (legend not plotted).
#' @param loi logical. Whether (TRUE, default setting) or not (FALSE) to plot the
#' extent of the buffered location of interest on the map.
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
#' plotStorms(pam, labels = TRUE)
#'
#' # Plotting Pam over Vanuatu with labels every 6h on the right side of the observations
#' plotStorms(pam, labels = TRUE, by = 2, pos = 4)
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
                       legends = "topleft",
                       loi = TRUE) {
  checkInputsPlotStorms(
    sts, names, category, labels, by, pos, legends,
    loi, xlim, ylim
  )


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
      ind <- which(getSSHS(sts) >= catInf & getSSHS(sts) <= catSup)
    } else {
      # length category == 1
      ind <- which(getSSHS(sts) == category)
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
  lapply(stsAux, plotTrack)
  if (labels) {
    lapply(stsAux, plotLabels, by, pos)
  }

  # Adding legends
  if (legends != "none") {
    leg <- c("TD", "TS", "Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5")
    col <- c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")

    lty <- rep(0, 7)
    pch <- rep(19, 7)
    lwd <- rep(1, 7)

    graphics::legend(legends,
      title = "SSHS",
      legend = leg,
      col = col,
      lty = lty,
      pch = pch,
      lwd = lwd,
      cex = 0.6,
      bty = "n"
    )
  }
}
