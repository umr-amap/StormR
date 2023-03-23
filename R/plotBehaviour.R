




#' Check inputs for plotBehaviour function
#'
#' @noRd
#' @param sts StormsList object
#' @param raster_product Spatraster
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @param labels logical
#' @param by numeric
#' @param pos numeric
#' @param color_palette character vector
#' @param main character
#' @param legends character
#' @return NULL
checkInputsPb <- function(sts, raster_product, xlim, ylim, labels, by, pos, color_palette, main, legends){

  #Checking sts input
  stopifnot("no data to plot" = !missing(sts))

  #Checking raster_product
  stopifnot("no data to plot" = !missing(raster_product))
  stopifnot("Raster stack are not allowed. Please subset your desired layer" = terra::nlyr(raster_product) == 1)

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

  #Checking labels input
  stopifnot("labels must be logical" = identical(class(labels), "logical"))

  #Checking by input
  stopifnot("by must be numeric" = identical(class(by),"numeric"))
  stopifnot("by must length 1" = length(by) == 1)
  stopifnot("by must be as integer" = round(by) == by)

  #Checking pos input
  stopifnot("pos must be numeric" = identical(class(pos),"numeric"))
  stopifnot("pos must length 1" = length(pos) == 1)
  stopifnot("pos must be between either 1, 2, 3 or 4" = pos %in% c(1, 2, 3, 4))

  #Checking color_palette input
  if(!is.null(color_palette))
    stopifnot("color_palette must be character" = identical(class(color_palette),"character"))
  
  #Checking main input
  if(!is.null(main)){
    stopifnot("main must be character" = identical(class(main),"character"))
    stopifnot("main must be length 1" = length(main) == 1)
  }
  
  #Checking legend
  stopifnot("legends must be character" = identical(class(legends), "character"))
  stopifnot("legends must be length 1" = length(legends) == 1)
  stopifnot("legends must be either topright, topleft, bottomleft, bottomright, or none" = legends %in% c("topright", "topleft", "bottomleft", "bottomright", "none"))
  
  

}






#'Plotting spatial wind behaviour 
#'
#'The `plotBehaviour()` function allows plotting spatial statistics generated using 
#'the `spatialBehaviour()` function and stored in `SpatRaster` objects.
#'
#'@param sts `StormsList` object.
#'@param raster_product layer name in a `SpatRaster` object. The names of the layers follow 
#'the following terminology, the name of the storm in capital letters and the name of the statistic 
#'separated by underscores (e.g., "PAM_MSW", "PAM_PDI"). For the duration of exposure, the names of the 
#'layer follow the following terminology, the name of the storm in capital letters, "Exposure", and the threshold 
#'value separated by underscores (e.g., "PAM_Exposure_18", "PAM_Exposure_33", ...). For the wind profiles, 
#'the names of the layer follow the following terminology, the name of the storm in capital letters, "Speed" or "Direction", 
#'and the indices of the observation separated by underscores (e.g., "PAM_Speed_41", "PAM_Direction_41",...).
#'@param color_palette character vector. The color palette used to plot the raster layer. If `color_palette=NULL` (default setting), 
#'default color palette are used.
#'@param main character. Title of the plot.  If `main=NULL` (default setting),
#'a default title is generated.
#'@param xlim numeric vector. The x limits of the plot.
#'@param ylim numeric vector. The y limits of the plot.
#'@param labels logical. Whether (TRUE) or not (FALSE, default setting) add labels with the name 
#'of the storm and the indices and ISO times of the observation.
#'@param by numeric. If `labels=TRUE`, defines the frequency at which labels are plotted. 
#'Default value is set to `8` which corresponds to a 24h (or 48h) time interval between the labelled observations
#'when observations are made every 3 (or 6) hours.
#'@param pos numeric. If `labels=TRUE`, defines the position of the labels, `1` (above the observation), 
#'`2` (on the left), `3` (below, default setting), and `4` (on the right).
#'@param legends character. Indicates where to plot the legend, `"topright"`, `"topleft"` (default setting), 
#'`"bottomleft"`, `"bottomright"`, or `"none"` (legend not plotted).
#'
#'@returns NULL
#'
#' @examples
#' \dontrun{
#' #Creating a StormsDataset
#' sds <- defDatabase()
#' 
#'#Getting storm track data for tropical cyclone Pam (2015)
#' pam <- Storms(sds = sds, loi = "Vanuatu", names = "PAM")
#' 
#' #Plotting maximum sustained wind speed for Pam (2015) near Vanuatu
#' pam.msw <- spatialBehaviour(pam, verbose = 0)
#' plotBehaviour(pam, pam.msw)
#'
#' #Plotting 2D wind speed profile for Pam (2015) near Vanuatu
#' pam.prof <- spatialBehaviour(pam, product = "Profiles", verbose = 0)
#' plotBehaviour(pam, pam.prof$PAM_Speed_37, labels = TRUE, pos = 4)
#' }
#'
#'
#'@export
plotBehaviour <- function(sts,
                          raster_product,
                          color_palette = NULL,
                          main = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          labels = FALSE,
                          by = 8,
                          pos = 3,
                          legends = "topleft"){


  checkInputsPb(sts, raster_product, xlim, ylim, labels, by, pos, color_palette,
                main, legends)

  name <- strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][1]
  product <- strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][2]


  if (!(name %in% getNames(sts)))
    stop("Imcompatibility between raster_product and sts (name not found in sts)")


  #Handling spatial extent
  xmin <- terra::ext(raster_product)$xmin
  xmax <- terra::ext(raster_product)$xmax
  ymin <- terra::ext(raster_product)$ymin
  ymax <- terra::ext(raster_product)$ymax

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


  #Plotting track
  plotStorms(sts = sts, names = name, xlim = c(xmin, xmax), ylim = c(ymin, ymax),
             legends = legends)

  #Adding raster_product on map
  if(product == "MSW"){

    col <- mswSSHSPalette
    range <- c(17, 95)
    leg <- expression(paste("MSW (m.s" ^ "-1",")"))

  }else if(product == "PDI"){

    col <- pdiPalette
    range <- c(0, max(terra::values(raster_product), na.rm = T))
    leg <- expression(paste("PDI (J.m" ^ "2",")"))

  }else if(product == "Exposure"){

    col <- exposurePalette
    range <- c(0, max(terra::values(raster_product), na.rm = T))
    leg <- expression(paste("Duration of exposure (h)"))

  }else if(product == "Speed"){

    col <- mswSSHSPalette
    range <- c(17, 95)
    leg <- expression(paste("Radial wind speed (m.s" ^ "-1",")"))

  }else if(product == "Direction"){

    col <- exposurePalette
    range <- c(0, 360)
    leg <- expression(paste("Wind direction (degree)"))

  }

  if(!is.null(color_palette))
    col <- color_palette
  
  if(!is.null(main))
    leg <- main

  #Adding title
  graphics::title(leg)

  
  
  plot(raster_product,
       col = col,
       type = "continuous",
       xlim = c(xmin, xmax),
       ylim = c(ymin, ymax),
       alpha = 0.7,
       axes = FALSE,
       range = range,
       legend = TRUE,
       add = T)


  #Adding track again (to emphazise)
  plotTrack(sts@data[[name]])

  #Adding labels
  if(labels & product != "Profiles" & product != "WindDirection")
    plotLabels(sts@data[[name]],by,pos)

  if(labels & (product == "Profiles" | product == "WindDirection")){

    ind <- as.numeric(strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][3])

    if(round(ind) == ind){
      #It is a real observation
      graphics::text(sts@data[[name]]@obs.all$lon[ind],
                     sts@data[[name]]@obs.all$lat[ind],
                     labels = paste0(name,"\n", sts@data[[name]]@obs.all$iso.time[ind],
                                     "\n(",ind,")"),
                     pos = pos,
                     cex = 0.6)
    }else{
      #It is an interpolated observation
      indf <- floor(ind)
      indc <- ceiling(ind)
      pos2 <- switch(pos, "1" = 3, "2" = 4, "3" = 1, "4" = 2)
      graphics::text(sts@data[[name]]@obs.all$lon[indf],
                     sts@data[[name]]@obs.all$lat[indf],
                     labels = paste0(name,"\n", sts@data[[name]]@obs.all$iso.time[indf],
                                    "\n(",indf,")"),
                     pos = pos,
                     cex = 0.6)

      graphics::text(sts@data[[name]]@obs.all$lon[indc],
                     sts@data[[name]]@obs.all$lat[indc],
                     labels = paste0(name,"\n", sts@data[[name]]@obs.all$iso.time[indc],
                                     "\n(",indc,")"),
                     pos = pos2,
                     cex = 0.6)
    }
  }
  
}
