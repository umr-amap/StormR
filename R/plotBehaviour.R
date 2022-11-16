



#' Plot a selected product (MSW, PDI ...) alongside with the track of a selected
#' storm contained in object `sts`.
#'
#' @param sts Storms object that contains the storm we are interested in
#' @param raster_product Spatraster object which spatializes the product we are interested
#' in. The name of the layer must be nameOfTheStorm_product (which is the case)
#' if the product is generated with the function `stormBehaviour`
#' @param xlim A set of longitude coordinates that controls the longitude extent
#' of the plot. Default value is NULL which will let the plot extends according
#' to the x bounding box of `spatial.loi.buffer`.
#' @param ylim A set of latitude coordinates that controls the latitude extent
#' of the plot. Default value is NULL which will let the plot extends according
#' to the y bounding box of `spatial.loi.buffer`.
#' @param mask Logical, wether or not to mask the data according to `spatial.loi`
#'
#' @return NULL
#' @export
plotBehaviour = function(sts, raster_product, xlim = NULL, ylim = NULL, mask = FALSE){


  #Check sts input
  stopifnot("no data to plot" = !missing(sts))

  #Check raster_product
  stopifnot("no data to plot" = !missing(raster_product))


  name = strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][1]
  product = strsplit(names(raster_product), split = "_", fixed = TRUE)[[1]][2]

  if(!(name %in% unlist(sts@names)))
    stop("Imcompatibility between raster_product and sts (name not found in sts)")


  #Check xlim input
  if(!is.null(xlim)){
    stopifnot("xlim must be numeric" = identical(class(xlim),"numeric"))
    stopifnot("xlim must length 2" = length(xlim) == 2)
    xlim = xlim[order(xlim)]
    stopifnot("xlim must have valid longitude coordinates" = xlim >= 0 & xlim <= 360)
  }

  #Check ylim input
  if(!is.null(ylim)){
    stopifnot("ylim must be numeric" = identical(class(ylim),"numeric"))
    stopifnot("ylim must length 2" = length(ylim) == 2)
    ylim = ylim[order(ylim)]
    stopifnot("ylim must have valid latitude coordinates" = ylim >= -90 & ylim <= 90)
  }

  #Check mask input
  stopifnot("mask must be logical" = identical(class(mask),"logical"))


  xmin = sf::st_bbox(sts@spatial.loi.buffer)$xmin
  xmax = sf::st_bbox(sts@spatial.loi.buffer)$xmax
  ymin = sf::st_bbox(sts@spatial.loi.buffer)$ymin
  ymax = sf::st_bbox(sts@spatial.loi.buffer)$ymax

  if(!is.null(xlim)){
    xmin = xlim[1]
    xmax = xlim[2]
  }
  if(!is.null(ylim)){
    ymin = ylim[1]
    ymax = ylim[2]
  }

  if(mask){
    v = terra::vect(sts@spatial.loi)
    m = terra::rasterize(v,raster_product)
    raster_product = terra::mask(raster_product,m)
  }

  plotStorms(sts, name = name, xlim = c(xmin,xmax), ylim = c(ymin,ymax))


  if(product == "MSW")
    plot(raster_product,
         col = rev(grDevices::heat.colors(50)),
         xlim = c(xmin,xmax),
         ylim = c(ymin,ymax),
         alpha = 0.7,
         axes = FALSE,
         range = c(17,max(raster_product[])),
         add = T)


  plot_track(sts@data[[name]],FALSE)


}
