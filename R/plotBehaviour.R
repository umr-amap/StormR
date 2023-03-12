




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
#' @return NULL
checkInputsPb <- function(sts, raster_product, xlim, ylim, labels, by, pos, color_palette, main){

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

}






#'Plot rasterized information storm behaviour
#'
#'This function plots a rasterized product (among maximum sustained wind, power
#'dissipation index, category exposure, or 2D wind speed structure/wind direction
#'at a given observation) associated with a storm provided in a StormsList object
#' alongside with its track
#'
#'@param sts StormsList object
#'@param raster_product SpatRaster object. Name of the layer must be
#'  "STORMNAME_product" where product is either:
#'  \itemize{
#'    \item "MSW"
#'    \item "PDI"
#'    \item "Exposure_threshold" where "threshold" represents the wind threshold
#'          used to compute Exposure raster
#'    \item "Speed_index" where index stands for the index of observation
#'    \item "Direction_index" where index stands for the index of
#'          observation
#'
#'  }
#' @param color_palette character vector. Represents the color palette used for
#' the plot. Default value is set to NULL, which will automatically choose a
#' color palette provided by this package and depending on the product
#' @param main character. Title of the plot. Default value is set to NULL wich
#' will set a title automatically depending on the product
#' @param xlim numeric vector. A set of longitude coordinates that controls the
#' longitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the longitude range of the extended LOI
#' @param ylim numeric vector. A set of latitude coordinates that controls the
#' latitude extent of the plot. Default value is set to NULL which will let
#' the plot extends according to the longitude range of the extended LOI
#' @param labels logical. Whether or not to plot name labels with the
#' corresponding indices of observations and ISO Times along the track
#' @param by numeric. Defines the frequency at which labels are plotted for the
#' 3 (or 6) hourly records. Default value is set to 8 which represents a 24h
#' (or 48h) time interval between each labeled observations. Ignored if
#' labels == FALSE
#' @param pos numeric. Must be between 1 and 4. Correspond to the position of
#' labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right).
#' Default value is set to 3. Ignored if labels == FALSE
#'
#' @returns NULL
#'
#' @examples
#' \dontrun{
#' #Plot MSW raster for Pam (2015) in Vanuatu
#' pam <- Storms(loi = "Vanuatu", names = "PAM")
#' pam_msw <- spatialBehaviour(pam, verbose = 0)
#' plotBehaviour(pam, pam_msw)
#'
#' #Plot a 2D windspeed structure  for Pam (2015) in Vanuatu
#' pam <- Storms(loi = "Vanuatu", names = "PAM")
#' pam_prof <- spatialBehaviour(pam, product = "Profiles", verbose = 0)
#' plotBehaviour(pam, pam_prof[["PAM_Profiles_37"]], labels = TRUE, pos = 2)
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
                          pos = 3){


  checkInputsPb(sts, raster_product, xlim, ylim, labels, by, pos, color_palette,
                main)

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

  #Handling gap with legend
  size.map = ymax - ymin
  y.leg = ymin - size.map * 0.2

  #Plotting track
  plotStorms(sts = sts, names = name, xlim = c(xmin, xmax), ylim = c(ymin, ymax))

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
       plg = list(loc = "bottom",
                  ext = c(xmin, xmax, y.leg - size.map* 0.05, y.leg),
                  cex = 0.7,
                  shrink = 0),
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
