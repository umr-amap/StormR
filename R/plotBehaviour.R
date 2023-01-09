



#' Check inputs for plotBehaviour function
#'
#' @noRd
#' @param sts Storms object
#' @param raster_product Spatraster
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @param labels logical
#' @param by numeric
#' @param pos numeric
#' @return NULL
checkInputsPb = function(sts, raster_product, xlim, ylim, labels, by, pos){

  #Checking sts input
  stopifnot("no data to plot" = !missing(sts))

  #Checking raster_product
  stopifnot("no data to plot" = !missing(raster_product))

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

  #Checking labels inputs
  stopifnot("labels must be logical" = identical(class(labels), "logical"))

  #Checking by inputs
  stopifnot("by must be numeric" = identical(class(by),"numeric"))
  stopifnot("by must be as integer" = ds4psy::is_wholenumber(by))
  stopifnot("by must length 1" = length(by) == 1)

  #Checking pos inputs
  stopifnot("pos must be numeric" = identical(class(pos),"numeric"))
  stopifnot("pos must be as integer" = ds4psy::is_wholenumber(pos))
  stopifnot("pos must be between 1 and 4" = pos >= 1 & pos <= 4)
  stopifnot("pos must length 1" = length(pos) == 1)

}






#'Plot rasterize informations below the associated track of a storm
#'
#'This function plots a rasterize product (Maximum Sustained Wind, Power
#'Dissipation Index, Category Exposure, 2D wind speed structure at a given
#'observation ...) associated with a storm contained in a Storms object
#'alongside with its track
#'
#'@param sts Storms object
#'@param raster_product Spatraster object. Name of the layer must be
#'  "stormName_product" where product is either MSW, PDI, or Exposure(1,2,3,4,5,All).
#'  It can also be "stormName_profileInd" where Ind stand for the observations if raster_product
#'  is a 2D wind speed structure
#'@param xlim numeric vector. A set of longitude coordinates that controls the
#'  longitude extent of the plot. Default value is set to NULL which will let
#'  the plot extends according to the x bounding box of spatial.loi.buffer
#'@param ylim numeric vector. A set of latitude coordinates that controls the
#'  latitude extent of the plot. Default value is set to NULL which will let
#'  the plot extends according to the y bounding box of spatial.loi.buffer
#'@param labels logical. Whether or not to plot ISO Times and name labels
#'@param by numeric. Increment of the sequence for the labels to plot. Default
#'  value is set to 8 which represents a 24h time interval
#'@param pos numeric. Must be between 1 and 4. Correspond to the position of
#'  labels according to the observation: 1 (up), 2 (left), 3 (down), 4 (right).
#'  Default value is set to 3
#'@returns NULL
#'
#' @examples
#' #Plot MSW analytic raster for PAM 2015 in Vanuatu
#' pam_msw = terra::rast(system.file("extdata", "PAM_MSW.tiff",package = "StormR"))
#' plotBehaviour(pam, pam_msw)
#'
#' #Plot PDI analytic raster for ERICA 2003 in New Caledonia
#' erica_pdi = terra::rast(system.file("extdata", "ERICA_PDI.tiff",package = "StormR"))
#' plotBehaviour(sts_nc, erica_pdi)
#'
#' #Plot PDI analytic raster for NIRAN 2021 in New Caledonia
#' niran_pdi = terra::rast(system.file("extdata", "NIRAN_PDI.tiff",
#'                                     package = "StormR"))
#' plotBehaviour(sts_nc, niran_pdi)
#'
#' #Plot 2D wind speed structure for ERICA 2003 at observation 93
#' erica_profile93 = terra::rast(system.file("extdata", "ERICA_profile93.tiff", package = "StormR"))
#' plotBehaviour(sts_nc, erica_profile93, labels = TRUE)
#'
#'@export
plotBehaviour = function(sts, raster_product, xlim = NULL, ylim = NULL, labels = FALSE,
                         by = 8, pos = 3){


  checkInputsPb(sts, raster_product, xlim, ylim, labels, by, pos)

  name = strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][1]
  product = strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][2]

  if (!(name %in% sts@names))
    stop("Imcompatibility between raster_product and sts (name not found in sts)")



  #Handling spatial extent
  xmin = terra::ext(raster_product)$xmin
  xmax = terra::ext(raster_product)$xmax
  ymin = terra::ext(raster_product)$ymin
  ymax = terra::ext(raster_product)$ymax

  if (!is.null(xlim)) {
    xlim = xlim[order(xlim)]
    xmin = xlim[1]
    xmax = xlim[2]
  }
  if (!is.null(ylim)) {
    ylim = ylim[order(ylim)]
    ymin = ylim[1]
    ymax = ylim[2]
  }


  #Plotting track
  plotStorms(
    sts = sts,
    names = name,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  )

  #Adding title
  graphics::title(paste0(name," ",sts@data[[name]]@season))


  #Adding raster_product on map
  if (product == "MSW" | stringr::str_detect(product,"profile")) {
    if(product == "MSW"){
      leg = expression(paste("MSW (m.s" ^ "-1)"))

    }else{
      leg = expression(paste("radial wind speed (m.s" ^ "-1)"))
    }

    plot(
      raster_product,
      col = rev(grDevices::heat.colors(50)),
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      alpha = 0.7,
      axes = FALSE,
      range = c(17, 80),
      legend = T,
      plg = list(
        title = leg,
        title.cex = 0.9,
        cex = 0.7,
        shrink = 0
      ),
      add = T,
    )


  } else if (product == "PDI") {
    plot(
      raster_product,
      col = rev(viridis::inferno(50)),
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      alpha = 0.7,
      axes = FALSE,
      range = c(0, 30),
      plg = list(
        title = expression(paste("PDI")),
        title.cex = 0.9,
        cex = 0.7,
        shrink = 0
      ),
      add = T
    )
  } else if (product %in% c("Exposure1",
                            "Exposure2",
                            "Exposure3",
                            "Exposure4",
                            "Exposure5",
                            "ExposureAll")) {
    plot(
      raster_product,
      col = rev(viridis::viridis(50)),
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      alpha = 0.7,
      axes = FALSE,
      plg = list(
        title = expression(paste("Time spent (h)")),
        title.cex = 0.9,
        cex = 0.7,
        shrink = 0
      ),
      add = T
    )
  }

  #Adding track again (to emphazise)
  plotTrack(sts@data[[name]], FALSE)

  #Adding labels
  if(labels)
    plotLabels(sts@data[[name]],by,pos)

  if(labels & stringr::str_detect(product,"profile")){
    ind = as.numeric(stringr::str_sub(product,8,nchar(product)))
    graphics::text(
      sts@data[[name]]@obs.all$lon[ind],
      sts@data[[name]]@obs.all$lat[ind],
      labels = paste(name,
                     sts@data[[name]]@obs.all$iso.time[ind],
                     sep = "\n"),
      pos = pos,
      cex = 0.6
    )
  }

}
