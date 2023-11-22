




#' Check inputs for plotBehaviour function
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
#' @return NULL, just stops the function if an error is found
checkInputsPlotBehaviour <- function(sts, rasterProduct, xlim, ylim, labels, by, pos, colorPalette, main, legends) {
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
}






#' Plotting spatial wind behaviour
#'
#' The `plotBehaviour()` function allows plotting spatial statistics generated using
#' the `spatialBehaviour()` function and stored in `SpatRaster` objects.
#'
#' @param sts `StormsList` object.
#' @param rasterProduct layer name in a `SpatRaster` object. The names of the layers follow
#' the following terminology:
#' \itemize{
#'    \item for "MSW" or "PDI", the name of the storm in capital letters and the name of the
#' statistic separated by underscores (e.g., "PAM_MSW", "PAM_PDI"),
#'    \item for duration of exposure, the name of the storm in capital letters, "Exposure",
#' and the threshold value separated by underscores (e.g., "PAM_Exposure_18", "PAM_Exposure_33", ...).
#'    \item for wind profiles, the name of the storm in capital letters, "Speed" or "Direction",
#' and the indices of the observation separated by underscores (e.g., "PAM_Speed_41", "PAM_Direction_41",...).
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
#' @param legends character. Indicates where to plot the legend, `"topright"`, `"topleft"` (default setting),
#' `"bottomleft"`, `"bottomright"`, or `"none"` (legend not plotted).
#'
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
#' # Plotting maximum sustained wind speed for Pam (2015) near Vanuatu
#' pam.msw <- spatialBehaviour(pam, verbose = 0)
#' plotBehaviour(pam, pam.msw)
#'
#' # Plotting 2D wind speed profile for Pam (2015) near Vanuatu
#' pam.prof <- spatialBehaviour(pam, product = "Profiles", verbose = 0)
#' plotBehaviour(pam, pam.prof$PAM_Speed_37, labels = TRUE, pos = 4)
#' }
#' @export
plotBehaviour <- function(sts,
                          rasterProduct,
                          colorPalette = NULL,
                          main = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          labels = FALSE,
                          by = 8,
                          pos = 3,
                          legends = "topleft") {
  checkInputsPlotBehaviour(
    sts, rasterProduct, xlim, ylim, labels, by, pos, colorPalette,
    main, legends
  )

  name <- strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]][1]
  product <- strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]][2]


  if (!(name %in% getNames(sts))) {
    stop("Imcompatibility between rasterProduct and sts (name not found in sts)")
  }


  # Handling spatial extent
  xmin <- terra::ext(rasterProduct)$xmin
  xmax <- terra::ext(rasterProduct)$xmax
  ymin <- terra::ext(rasterProduct)$ymin
  ymax <- terra::ext(rasterProduct)$ymax

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


  # Plotting track
  plotStorms(
    sts = sts, names = name, xlim = c(xmin, xmax), ylim = c(ymin, ymax),
    legends = legends
  )

  # Adding rasterProduct on map
  if (product == "MSW") {
    col <- mswSSHSPalette
    range <- c(17, 95)
    leg <- expression(paste("MSW (m.s"^"-1", ")"))
  } else if (product == "PDI") {
    col <- pdiPalette
    range <- c(0, max(terra::values(rasterProduct), na.rm = TRUE))
    leg <- expression(paste("PDI (J.m"^"2", ")"))
  } else if (product == "Exposure") {
    col <- exposurePalette
    range <- c(0, max(terra::values(rasterProduct), na.rm = TRUE))
    leg <- expression(paste("Duration of exposure (h)"))
  } else if (product == "Speed") {
    col <- mswSSHSPalette
    range <- c(17, 95)
    leg <- expression(paste("Radial wind speed (m.s"^"-1", ")"))
  } else if (product == "Direction") {
    col <- exposurePalette
    range <- c(0, 360)
    leg <- expression(paste("Wind direction (degree)"))
  }

  if (!is.null(colorPalette)) {
    col <- colorPalette
  }

  if (!is.null(main)) {
    leg <- main
  }

  # Adding title
  graphics::title(leg)


  terra::plot(rasterProduct,
    col = col,
    type = "continuous",
    xlim = c(xmin - 1, xmax + 1), # we extend W & E by 1°. Needs to be in agreement with plotStorm
    ylim = c(ymin - 1, ymax + 1), # we extend S & N by 1°. Needs to be in agreement with plotStorm
    alpha = 0.7,
    axes = FALSE,
    range = range,
    legend = TRUE,
    add = TRUE
  )


  # Adding track again (to emphazise)
  plotTrack(sts@data[[name]])

  # Adding labels
  if (labels && product != "Profiles" && product != "WindDirection") {
    plotLabels(sts@data[[name]], by, pos)
  }

  if (labels && (product == "Profiles" || product == "WindDirection")) {
    ind <- as.numeric(strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]][3])

    if (round(ind) == ind) {
      # It is a real observation
      graphics::text(sts@data[[name]]@obs.all$lon[ind],
        sts@data[[name]]@obs.all$lat[ind],
        labels = paste0(
          name, "\n", sts@data[[name]]@obs.all$iso.time[ind],
          "\n(", ind, ")"
        ),
        pos = pos,
        cex = 0.6
      )
    } else {
      # It is an interpolated observation
      indf <- floor(ind)
      indc <- ceiling(ind)
      pos2 <- switch(pos,
        "1" = 3,
        "2" = 4,
        "3" = 1,
        "4" = 2
      )
      graphics::text(sts@data[[name]]@obs.all$lon[indf],
        sts@data[[name]]@obs.all$lat[indf],
        labels = paste0(
          name, "\n", sts@data[[name]]@obs.all$iso.time[indf],
          "\n(", indf, ")"
        ),
        pos = pos,
        cex = 0.6
      )

      graphics::text(sts@data[[name]]@obs.all$lon[indc],
        sts@data[[name]]@obs.all$lat[indc],
        labels = paste0(
          name, "\n", sts@data[[name]]@obs.all$iso.time[indc],
          "\n(", indc, ")"
        ),
        pos = pos2,
        cex = 0.6
      )
    }
  }
}
