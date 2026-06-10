#' Check inputs for plotTEW function
#'
#' @noRd
#' @param sts StormsList object
#' @param rasterProduct Spatraster
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @param labels logical
#' @param by numeric
#' @param pos numeric
#' @param colorPalette character vector
#' @param main character
#' @param legends character
#' @param dynamicPlot logical
#' @return NULL, just stops the function if an error is found
#' @noRd

checkInputsplotTEW <- function(sts,
                               rasterProduct,
                               xlim,
                               ylim,
                               labels,
                               by,
                               pos,
                               colorPalette,
                               main,
                               legends,
                               fan,
                               dynamicPlot,
                               dtm) {
  # Checking sts input
  stopifnot("no data to plot" = !missing(sts))

  # Checking rasterProduct
  stopifnot("no data to plot" = !missing(rasterProduct))
  stopifnot("Raster stack are not allowed. Please subset your desired layer" = terra::nlyr(rasterProduct) == 1)


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

  # Checking labels input
  stopifnot("labels must be logical" = identical(class(labels), "logical"))

  # Checking by input
  stopifnot("by must be numeric" = identical(class(by), "numeric"))
  stopifnot("by must length 1" = length(by) == 1)
  stopifnot("by must be as integer" = round(by) == by)

  # Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos), "numeric"))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between either 1, 2, 3 or 4" = pos %in% c(1, 2, 3, 4))

  # Checking colorPalette input
  if (!is.null(colorPalette)) {
    stopifnot("colorPalette must be character" = identical(class(colorPalette), "character"))
  }

  # Checking main input
  if (!is.null(main)) {
    stopifnot("main must be character" = identical(class(main), "character"))
    stopifnot("main must be length 1" = length(main) == 1)
  }

  # Checking legend
  stopifnot("legends must be character" = identical(class(legends), "character"))
  stopifnot("legends must be length 1" = length(legends) == 1)
  stopifnot(
    "legends must be either topright, topleft, bottomleft, bottomright, or none" =
      legends %in% c("topright", "topleft", "bottomleft", "bottomright", "none")
  )

  # Checking fan input
  stopifnot("fan must be logical" = identical(class(fan), "logical"))
  stopifnot("fan must length 1" = length(fan) == 1)

  # Checking mode input
  stopifnot("dynamicPlot must be logical" = identical(class(dynamicPlot), "logical"))
  stopifnot("dynamicPlot must length 1" = length(dynamicPlot) == 1)

  # Checking dtm input
  if (!is.null(dtm)) {
    stopifnot("dtm must be a SpatRaster" = inherits(dtm, "SpatRaster"))
  }
}

##' Check inputs for plotTemporalTEW function
#'
#' @noRd
#' @param data list of data frames
#' @param storm numeric or character
#' @return NULL
checkInputsPlotTemporalTEW <- function(data, storm) {

  # Checking data input
  stopifnot("no data found" = !missing(data))
  stopifnot("data must be temporal series. Be sure to use data generated from temporalBehaviour()
  function first, and then computeTEW() function" = identical(class(data[[1]]), "list"))

  # Checking storm input
  stopifnot("You must select one single storm" = !missing(storm))
  if (identical(class(storm), "character")) {
    stopifnot("storm not find in data" = (storm %in% names(data)))
  }else if (identical(class(storm), "numeric")) {
    stopifnot("storm index out of bound in data" = (storm %in% c(1, length(names(data)))))
  }else {
    stop("Invalid storm input")
  }

}

#' compute wind fan for plotTEW
#' @param sts StormsList object
#' @param rasterProduct raster object
#'
#' @return the minimum and maximum direction of each profile
#' @noRd

computeWindFan <- function(sts, rasterProduct) {
  pf <- spatialBehaviour(sts, product = "Profiles", verbose = 0)

  pf_aligned <- terra::project(pf, rasterProduct, method = "bilinear")
  pf_masked <- terra::mask(pf_aligned, rasterProduct)

  layersDir <- names(pf_masked)[grep("_Direction_", names(pf_masked))]
  if (length(layersDir) == 0L) {
    warning("No direction layers found in pf. Wind fan will not be drawn.")
    return(NULL)
  }

  dir_range <- terra::global(pf_masked[[layersDir]], fun = "range", na.rm = TRUE)
  d_min <- min(dir_range$min, na.rm = TRUE)
  d_max <- max(dir_range$max, na.rm = TRUE)

  list(d_min = d_min, d_max = d_max)
}



#' Plotting topographic exposure to wind
#'
#' The `plotTEW()` function allows plotting topographic exposure to wind products generated using
#' the `computeTEW()` function and stored in `SpatRaster` objects.
#'
#' @param sts `StormsList` object.
#' @param rasterProduct layer name in a `SpatRaster` object. These layers are obtained from the `computeTEW` function.
#' The names of the layers follow the terminology:
#' \itemize{
#'    \item for "Integrated", "TEW1wdMean" or "TEW1wdMax", the name of the storm in capital letters,
#'      "TEW1wd" or "TEW" and the name of the
#'      statistic separated by underscores (e.g.,"PAM_TEW1wd_Mean", "PAM_TEW1wd_Max", "PAM_TEW_Mean"),
#'    \item for multiple profiles, the name of the storm in capital letters, "TEW" or "TEW1wd",
#'      and the indices of the observation separated by underscores (e.g., "PAM_TEW_18", "PAM_TEW1wd_33", ...).
#' }
#' @param colorPalette character vector. The color palette used to plot the raster layer.
#' If `colorPalette=NULL` (default setting), the default color palette is used.
#' @param main character. Title of the plot. If `main=NULL` (default setting),
#' a default title is generated based on the name of the layer.
#' @param xlim numeric vector. The x limits of the plot.
#' @param ylim numeric vector. The y limits of the plot.
#' @param labels logical. Whether (TRUE) or not (FALSE, default setting) to add labels with the name
#' of the storm and the indices and ISO times of the observation.
#' @param by numeric. If `labels=TRUE`, defines the frequency at which labels are plotted.
#' Default value is set to `8` which corresponds to a 24h (or 48h) time interval between the labelled observations
#' when observations are made every 3 (or 6) hours.
#' @param pos numeric. If `labels=TRUE`, defines the position of the labels, `1` (above the observation),
#' `2` (on the left), `3` (below, default setting), and `4` (on the right).
#' @param legends character. Indicates where to plot the legend, `"topright"`(default setting), `"topleft"`,
#' `"bottomleft"`, `"bottomright"`, or `"none"` (legend not plotted).
#' @param fan logical. Whether (FALSE, default setting) or (TRUE) to add the range of wind direction along the storm.
#' @param dtm `SpatRaster`. Digital terrain model used to compute the TEW.
#' If provided, the land/ocean mask is derived from the DTM at its native resolution,
#' giving a much finer coastline than world map polygons.
#' @param dynamicPlot logical. Whether (FALSE, default setting) or (TRUE) to plot the
#' data dynamicaly using leaflet library
#' @returns A plot of the storm track data with the raster layer.
#'
#' @examples
#' \donttest{
#' # Creating a stormsDataset
#' sds <- defStormsDataset()
#'
#' # Getting storm track data for tropical cyclone Pam (2015)
#' pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#'
#' # Getting digital terrain model for PortVilla island
#' mnt <- terra::rast(system.file("extdata", "test_datadtm.tif", package = "StormR"))
#'
#' # Plotting topographic exposure to wind (tew) for Pam (2015) near Vanuatu
#' pam.prof <- spatialBehaviour(pam, product = "Profiles", verbose = 0)
#' pam.tew <- computeTEW(pam.prof, pam, mnt, verbose = 0)
#'
#' # Basic plot
#' plotTEW(pam, pam.tew)
#'
#' # Plot with DTM-derived land mask for finer coastline
#' plotTEW(pam, pam.tew, dtm = mnt)
#'
#' # Plotting with the range of wind direction
#' plotTEW(pam, pam.tew, fan = TRUE)
#'
#' # dynamicPlot mode
#' plotTEW(pam, pam.tew, dynamicPlot = TRUE)
#' }
#' @export

plotTEW <- function(sts,
                    rasterProduct,
                    dtm = NULL,
                    colorPalette = NULL,
                    main = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    labels = FALSE,
                    by = 8,
                    pos = 3,
                    legends = "topright",
                    fan = FALSE,
                    dynamicPlot = FALSE) {
  checkInputsplotTEW(
    sts, rasterProduct, xlim, ylim, labels, by, pos, colorPalette,
    main, legends, fan, dynamicPlot, dtm
  )

  product <- strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]]
  name <- product[1]
  productSubType <- product[2]
  productId <- product[3]

  if (!(name %in% getNames(sts))) {
    stop("Incompatibility between rasterProduct and sts (name not found in sts)")
  }

  # spatial extent
  extent <- terra::ext(rasterProduct)
  xmin <- extent$xmin
  xmax <- extent$xmax
  ymin <- extent$ymin
  ymax <- extent$ymax

  if (!is.null(xlim)) {
    xlim <- xlim[order(xlim)]
    xmin <- xlim[1]
    xmax <- xlim[2]
  }
  if (!is.null(ylim)) {
    ylim <- ylim[order(ylim)]
    ymin <- ylim[1]
    ymax <- ylim[2]
  }

  # legend and color
  col <- grDevices::hcl.colors(100, palette = "RdBu", rev = TRUE)
  range <- c(-1, 1)
  leg <- name

  if (productId == "Max") {
    leg <- ifelse(dynamicPlot, "Max",
      expression(paste("One wind direction Maximum Topographic Exposure to Wind"))
    )
  } else if (productId == "Mean") {
    leg <- ifelse(dynamicPlot, "Mean",
      expression(paste("One wind direction Mean Topographic Exposure to Wind"))
    )
  } else if (productId == "Integrated") {
    leg <- ifelse(dynamicPlot, "Integrated Topographic Exposure to Wind",
      expression(paste("Integrated Topographic Exposure to Wind"))
    )
  } else {
    if (productSubType == "TEW1wd") {
      leg <- ifelse(dynamicPlot, paste("TEW 1 wind direction - Index", productId),
        paste("One wind direction Topographic Exposure to Wind - Index", productId)
      )
    } else if (productSubType == "TEW") {
      leg <- ifelse(dynamicPlot, paste("TEW - Index", productId),
        paste("Topographic Exposure to Wind - Index", productId)
      )
    }
  }

  if (!is.null(colorPalette)) col <- colorPalette
  if (!is.null(main)) leg <- main

  # Build land mask from DTM (preferred) or world map polygons
  if (!is.null(dtm)) {
    land_mask <- terra::ifel(dtm > 0, 1, NA)
    land_contour <- terra::ifel(dtm > 0, 1, 0)
  } else {
    world <- terra::project(terra::vect(rworldmap::getMap(resolution = "high")),
      terra::crs(rasterProduct)
    )
    land_mask <- terra::rasterize(world, rasterProduct, field = 1)
    land_contour <- terra::subst(land_mask, NA, 0)
  }
  raster_land <- terra::mask(rasterProduct, land_mask)

  # static plot
  if (!dynamicPlot) {
    # Plot land/ocean background
    terra::plot(land_mask,
      col = groundColor,
      colNA = oceanColor,
      axes = TRUE,
      legend = FALSE,
      main = leg
    )

    # Overlay TEW raster
    terra::plot(raster_land,
      col = col,
      type = "continuous",
      alpha = 0.7,
      axes = FALSE,
      range = range,
      legend = TRUE,
      add = TRUE
    )

    # Draw coastline at raster resolution
    terra::contour(land_contour,
      levels = 0.5, col = "grey30", lwd = 0.6,
      drawlabels = FALSE, add = TRUE
    )

    # track storm
    plotTrack(sts@data[[name]], sts@scale, sts@scalePalette)

    # fan of wind direction along the cyclone
    if (fan) {
      fan_data <- tryCatch(computeWindFan(sts, rasterProduct), error = function(e) NULL)

      if (!is.null(fan_data)) {
        x_span <- xmax - xmin
        y_span <- ymax - ymin

        x0 <- xmin + x_span * 0.85
        y0 <- ymin + y_span * 0.88
        r <- x_span * 0.06

        d_min_rad <- (90 - fan_data$d_max) * pi / 180
        d_max_rad <- (90 - fan_data$d_min) * pi / 180

        t_seq <- seq(d_min_rad, d_max_rad, length.out = 40)
        x_arc <- x0 + r * cos(t_seq)
        y_arc <- y0 + r * sin(t_seq)

        graphics::polygon(c(x0, x_arc, x0), c(y0, y_arc, y0),
          col    = grDevices::rgb(1, 1, 1, 0.45),
          border = "grey30",
          lwd    = 1.5
        )
      }
    }


    # labels
    if (labels && productId %in% c("Mean", "Max", "Integrated")) {
      plotLabels(sts@data[[name]], by, pos)
    }

    if (labels && !productId %in% c("Mean", "Max", "Integrated")) {
      ind <- as.numeric(productId)

      if (round(ind) == ind) {
        graphics::text(sts@data[[name]]@obs.all$lon[ind],
          sts@data[[name]]@obs.all$lat[ind],
          labels = paste0(
            name, "\n",
            sts@data[[name]]@obs.all$iso.time[ind],
            "\n(", ind, ")"
          ),
          pos = pos, cex = 0.6
        )
      } else {
        indf <- floor(ind)
        indc <- ceiling(ind)
        pos2 <- switch(as.character(pos),
          "1" = 3,
          "2" = 4,
          "3" = 1,
          "4" = 2
        )

        graphics::text(sts@data[[name]]@obs.all$lon[indf],
          sts@data[[name]]@obs.all$lat[indf],
          labels = paste0(
            name, "\n",
            sts@data[[name]]@obs.all$iso.time[indf],
            "\n(", indf, ")"
          ),
          pos = pos, cex = 0.6
        )

        graphics::text(sts@data[[name]]@obs.all$lon[indc],
          sts@data[[name]]@obs.all$lat[indc],
          labels = paste0(
            name, "\n",
            sts@data[[name]]@obs.all$iso.time[indc],
            "\n(", indc, ")"
          ),
          pos = pos2, cex = 0.6
        )
      }
    }

    # dynamic plot
  } else {
    ext <- unname(as.vector(terra::ext(rasterProduct)))

    track <- sts@data[[name]]@obs.all

    pal <- leaflet::colorNumeric(
      palette  = grDevices::hcl.colors(100, palette = "RdBu", rev = TRUE),
      domain   = c(-1, 1),
      na.color = "transparent"
    )

    map <- leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap,
        options = leaflet::providerTileOptions(errorTileUrl = "Tile not found")
      ) |>
      leaflet::fitBounds(
        lng1 = ext[1], lat1 = ext[3],
        lng2 = ext[2], lat2 = ext[4]
      ) |>
      leaflet::addRasterImage(raster_land,
        colors  = pal,
        opacity = 0.8
      ) |>
      leaflet::addPolylines(
        lng = unname(track$lon),
        lat = unname(track$lat),
        color = "black",
        weight = 1.5,
        opacity = 0.8
      ) |>
      leaflet::addCircleMarkers(
        lng = unname(track$lon),
        lat = unname(track$lat),
        radius = 4,
        color = "black",
        fill = TRUE,
        fillOpacity = 0.8
      ) |>
      leaflet::addLegend(legends,
        pal     = pal,
        values  = terra::values(raster_land, na.rm = TRUE),
        title   = leg,
        opacity = 0.8
      )
    map
  }
}

#' Plotting TEW time series at given point locations
#'
#' @param data time series generated with `temporalBehaviour` with `product=TS`
#'   input first and then `computeTEW`
#' @param storm list of characters. Storm names. The storm must be available in
#'   `data` input. It can also be a vector of integer corresponding to the
#'   indices of storms stored in `data`input.
#'
#' @return null
#' @export
plotTemporalTEW <- function(data, storm) {

  checkInputsPlotTemporalTEW(data, storm)

  # Filter by storm
  subData <- data[storm]

  nbOfPositions <- length(subData[[1]])

  # Generate a sequence of colors

  # TODO pb of dependencies here...
  palette <- grDevices::colorRampPalette(colors = c("red", "green", "blue"))
  cols <- palette(nbOfPositions)

  # Generate a sequence of symbols
  symbols <- seq(1, nbOfPositions, 1)

  notNaIndices <- which(!is.na(subData[[1]][[1]]$tew))

  # Define x-axis range
  for (location in subData[[1]][c(2:nbOfPositions)]) {

    notNaIndices <- union(notNaIndices,
                          which(!is.na(location$tew)))
  }

  notNaIndices <- seq(min(notNaIndices), max(notNaIndices))
  dat <- subData[[1]][[1]]$tew[notNaIndices]
  ylim <- c(-1., 1.)
  ylab <- "Topographic Exposure to Wind"
  dy <- .25 # dashed lines every 0.25

  plot(dat,
       type = "l",
       ylim = ylim,
       xlab = "",
       ylab = ylab,
       axes = FALSE,
       col = cols[1])

  graphics::points(dat, col = cols[1], pch = symbols[1])

  i <- 2
  for (location in subData[[1]][c(2:nbOfPositions)]) {
    dat <- location$tew[notNaIndices]

    graphics::lines(dat, col = cols[i])
    graphics::points(dat, col = cols[i], pch = symbols[i])
    i <- i + 1
  }

  graphics::legend("topleft",
                   pch = symbols,
                   col = cols,
                   legend = paste(names(subData), names(subData[[1]])),
                   bty = "n")


  # Handle x axis labels
  labels <- subData[[1]][[1]]$isoTimes[notNaIndices]
  t1 <- labels[1]
  t2 <- labels[2]
  diffTime <- as.numeric(difftime(t2, t1, units = "hours"))

  if (diffTime == 30) {
    notNullLabels <- seq(1, length(labels)) %% 2 == 0
    labels[notNullLabels] <- ""
  }else if (diffTime == 15) {
    notNullLabels <- seq(1, length(labels)) %% 4 == 0
    labels[notNullLabels] <- ""
  }

  graphics::axis(1,
                 at = seq(1, length(subData[[1]][[1]]$tew[notNaIndices])),
                 labels = labels,
                 las = 2,
                 cex.axis = 0.5)


  graphics::axis(2,
                 at = seq(ylim[1], ylim[2], dy),
                 labels = seq(ylim[1], ylim[2], dy),
                 las = 2)

  #abline(v=seq(1, length(subData[[1]][[1]]$speed)),
  #       lty = 2)
  graphics::abline(h = seq(ylim[1], ylim[2], dy),
                   lty = 2)

}
