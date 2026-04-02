#' Check inputs for plotExposure function
#'
#' @noRd
#' @param sts StormsList object
#' @param dtm Digital Terrain Model
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

checkInputsPlotExposure <- function(sts, dtm, rasterProduct, xlim, ylim, labels, by, pos, colorPalette, main, legends, fan, dynamicPlot) {
  # Checking sts input
  stopifnot("no data to plot" = !missing(sts))
  
  # Checking dtm input
  stopifnot("no data to plot" = !missing(dtm))
  
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
}


#' compute wind fan for plotExposure
#' @param sts StormsList object
#' @param rasterProduct raster object
#' 
#' @return the minimum and maximum direction of each profile
#' @noRd

computeWindFan <- function(sts, rasterProduct) {
  
  pf <- spatialBehaviour(sts, product = "Profiles", verbose = 0)
  
  pf_aligned <- terra::project(pf, rasterProduct, method = "bilinear")
  pf_masked  <- terra::mask(pf_aligned, rasterProduct)
  
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
#' The `plotExposure()` function allows plotting topographic exposure to wind products generated using
#' the `computeExposure()` function and stored in `SpatRaster` objects.
#'
#' @param sts `StormsList` object.
#' @param dtm  SpatRaster of Digital Terrain Model object.
#' @param rasterProduct layer name in a `SpatRaster` object. The names of the layers follow
#' the following terminology:
#' \itemize{
#'    \item for "Summary","Mean" or "Max", the name of the storm in capital letters,"Exposure" and the name of the
#' statistic separated by underscores (e.g.,"PAM_Exposure_Summary" "PAM_Exposure_Mean", "PAM_Exposure_Mean"),
#'    \item for wind profiles, the name of the storm in capital letters, "Exposure",
#' and the indices of the observation separated by underscores (e.g., "PAM_Exposure_18", "PAM_Exposure_33", ...).
#'    \item for wind profiles computed by pixel, the name of the storm in capital letters, "Exposure",
#' and the indices of the observation separated by underscores followed by "_Pix" (e.g., "PAM_Exposure_41_Pix",,...).
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
#' mnt <- 
#'
#' # Plotting topographic exposure to wind (tew) for Pam (2015) near Vanuatu
#' pam.tew <- computeExposure(pam, mnt, verbose = 0)
#' plotExposure(pam, mnt, pam.tew)
#'
#' # Plotting with the range of wind direction
#' plotExposure(pam, mnt, pam.tew, fan = TRUE)
#' 
#' # dynamicPlot mode
#' plotExposure(pam, mnt, pam.msw, dynamicPlot = TRUE)
#'
#' }
#' @export

plotExposure <- function(sts,
                         dtm,
                         rasterProduct,
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
  
  checkInputsPlotExposure(
    sts, dtm, rasterProduct, xlim, ylim, labels, by, pos, colorPalette,
    main, legends, fan, dynamicPlot
  )
  
  name    <- strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]][1]
  product <- strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]][3]
  
  if (!(name %in% getNames(sts))) {
    stop("Incompatibility between rasterProduct and sts (name not found in sts)")
  }
  
  # spatial extent
  xmin <- terra::ext(rasterProduct)$xmin
  xmax <- terra::ext(rasterProduct)$xmax
  ymin <- terra::ext(rasterProduct)$ymin
  ymax <- terra::ext(rasterProduct)$ymax
  
  if (!is.null(xlim)) { xlim <- xlim[order(xlim)]; xmin <- xlim[1]; xmax <- xlim[2] }
  if (!is.null(ylim)) { ylim <- ylim[order(ylim)]; ymin <- ylim[1]; ymax <- ylim[2] }
  
  # legend and color
  col   <- grDevices::hcl.colors(100, palette = "RdBu")
  range <- c(-1,1)
  leg   <- name
  
  if (product == "Max") {
    leg   <- ifelse(dynamicPlot, "Max",
                    expression(paste("Maximum Topographic Exposure")))
  } else if (product == "Mean") {
    leg <- ifelse(dynamicPlot, "Mean",
                  expression(paste("Mean Topographic Exposure")))
  } else if (product == "Summary") {
    leg <- ifelse(dynamicPlot, "Summary of Topographic Exposure",
                  expression(paste("Summary of Topographic Exposure")))
  }
  
  if (!is.null(colorPalette)) col <- colorPalette
  if (!is.null(main))         leg <- main
  
  # static plot
  if (!dynamicPlot) {
    
    # plot on rasterProduct extent
    terra::plot(rasterProduct,
                col    = col,
                type   = "continuous",
                xlim   = c(xmin, xmax),
                ylim   = c(ymin, ymax),
                alpha  = 0.7,
                axes   = TRUE,
                range  = range,
                legend = TRUE,
                main = leg)
    
    # contour of land
    land <- terra::ifel(dtm > 0, 1, NA)
    land_poly <- terra::as.polygons(land, dissolve = TRUE)
    terra::plot(land_poly,
                add    = TRUE,
                col    = NA,
                border = "grey30",
                lwd    = 0.8)
    
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
        r  <- x_span * 0.06
        
        d_min_rad <- (90 - fan_data$d_max) * pi / 180
        d_max_rad <- (90 - fan_data$d_min) * pi / 180
        
        t_seq <- seq(d_min_rad, d_max_rad, length.out = 40)
        x_arc <- x0 + r * cos(t_seq)
        y_arc <- y0 + r * sin(t_seq)
        
        graphics::polygon(c(x0, x_arc, x0), c(y0, y_arc, y0),
                          col    = grDevices::rgb(1, 1, 1, 0.45),
                          border = "grey30",
                          lwd    = 1.5)

      }
    }
      
    
    # labels
    if (labels && !product %in% c("Profiles", "PixProfiles")) {
      plotLabels(sts@data[[name]], by, pos)
    }
    
    if (labels && product %in% c("Profiles", "PixProfiles")) {
      ind <- as.numeric(strsplit(names(rasterProduct), split = "_", fixed = TRUE)[[1]][3])
      
      if (round(ind) == ind) {
        graphics::text(sts@data[[name]]@obs.all$lon[ind],
                       sts@data[[name]]@obs.all$lat[ind],
                       labels = paste0(name, "\n",
                                       sts@data[[name]]@obs.all$iso.time[ind],
                                       "\n(", ind, ")"),
                       pos = pos, cex = 0.6)
      } else {
        indf <- floor(ind); indc <- ceiling(ind)
        pos2 <- switch(as.character(pos), "1"=3, "2"=4, "3"=1, "4"=2)
        
        graphics::text(sts@data[[name]]@obs.all$lon[indf],
                       sts@data[[name]]@obs.all$lat[indf],
                       labels = paste0(name, "\n",
                                       sts@data[[name]]@obs.all$iso.time[indf],
                                       "\n(", indf, ")"),
                       pos = pos, cex = 0.6)
        
        graphics::text(sts@data[[name]]@obs.all$lon[indc],
                       sts@data[[name]]@obs.all$lat[indc],
                       labels = paste0(name, "\n",
                                       sts@data[[name]]@obs.all$iso.time[indc],
                                       "\n(", indc, ")"),
                       pos = pos2, cex = 0.6)
      }
    }
    
    # dynamic plot
  } else {
    
    ext <- unname(as.vector(terra::ext(rasterProduct)))
    
    track <- sts@data[[name]]@obs.all
    
    pal <- leaflet::colorNumeric(
      palette  = "RdBu",
      domain   = c(-1,1),
      na.color = "transparent"
    )
    
    map <- leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::fitBounds(lng1 = ext[1], lat1 = ext[3],
                         lng2 = ext[2], lat2 = ext[4]) |>
      leaflet::addRasterImage(rasterProduct,
                              colors  = pal,
                              opacity = 0.8) |>
      leaflet::addPolylines(lng     = unname(track$lon),
                            lat     = unname(track$lat),
                            color   = "black",
                            weight  = 1.5,
                            opacity = 0.8) |>
      leaflet::addCircleMarkers(lng         = unname(track$lon),
                                lat         = unname(track$lat),
                                radius      = 4,
                                color       = "black",
                                fill        = TRUE,
                                fillOpacity = 0.8) |>
      leaflet::addLegend(legends,
                         pal     = pal,
                         values  = terra::values(rasterProduct, na.rm = TRUE),
                         title   = leg,
                         opacity = 0.8)
    map
    
  }
}
