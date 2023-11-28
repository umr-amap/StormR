
# TODO non ASCII character somewhere ...?

##' Check inputs for plotTemporal function
#'
#' @noRd
#' @param data list of data frames
#' @param storm numeric or character
#' @param var character
#' @return NULL
checkInputsPlotTemporal <- function(data, storm, var) {
  
  # Checking data input
  stopifnot("no data found" = !missing(data))
  stopifnot("data must be temporal series. Be sure to use data generated from temporalBehaviour() function run with product input set to 'TS'" = identical(class(data[[1]]), "list"))
  
  # Checking storm input
  stopifnot("You must select one single storm" = !missing(storm))
  if(identical(class(storm), "character")){
    stopifnot("storm not find in data" = (storm %in% names(data)))
  }else if(identical(class(storm), "numeric")){
    stopifnot("storm index out of bound in data" = (storm %in% c(1, length(names(data)))))
  }else{
    stop("Invalid storm input")
  }
  
  # Checking var input
  stopifnot("invalid var input, must be either 'speed' or 'direction'" = 
              var == 'speed' | var == 'direction')

}




#' Plotting wind behaviour time series and summary statistics at given point
#' locations
#'
#' @param data time series generated with `temporalBehaviour` with `product=TS`
#'   input
#' @param storm list of characters. Storm names. The storm must be available in
#'   `data` input. It can also be a vector of integer corresponding to the
#'   indices of storms stored in `data`input.
#' @param var character. Represent the type of variable to plot. Must be either
#'   `speed` or `direction`. Default value is set to `speed`.
#'
#' @return null
#' @export
#'
#' @examples
#' \donttest{
#' #Add example here
#' }
plotTemporal <- function(data, storm, var = 'speed'){
  
  checkInputsPlotTemporal(data, storm, var)
  
  # Filter by storm
  sub_data <- data[storm]
  
  nbOfPositions <- length(sub_data[[1]])
  
  # Generate a sequence of colors
  
  # TODO pb of dependencies here...
  palette <- colorRampPalette(colors=c("red", "green","blue"))
  cols <- palette(nbOfPositions)
  
  # Generate a sequence of symbols 
  symbols <- seq(1, nbOfPositions, 1)
  
  # Generate range of wind speed
  if(var=="speed"){
    minValue <- c()
    maxValue <- c()
    for(location in sub_data[[1]]){
      minValue <- c(minValue, min(location$speed, na.rm=T))
      maxValue <- c(maxValue, max(location$speed, na.rm=T))
    }
    range <- c(floor(min(minValue))-10, ceiling(max(maxValue) + 10))
    dy = 5
  }else{
    range = c(0, 360)
    dy = 15
  }
  
  # Plot setting depending on the var input
  if(var=="speed"){
    dat <- sub_data[[1]][[1]]$speed
    ylim <- c(0, 60) # Changer ici avec le range dynamique
    ylab <- "Maximum Sustained Wind speed (m/s)"
  }else{
    dat = sub_data[[1]][[1]]$direction
    ylab <- "Wind direction (°)"
    ylim <- c(0, 360)
  }
  
  plot(dat,
       type = "l",
       ylim = ylim,
       xlab = "",
       ylab = ylab, 
       axes = FALSE,
       col = cols[1])
  
  graphics::points(dat, col = cols[1], pch = symbols[1])
  
  i = 2
  for(location in sub_data[[1]][c(2:nbOfPositions)]){
    
    if(var=="speed"){
      dat <- location$speed
    }else{
      dat = location$direction
    }
    
    graphics::lines(dat, col = cols[i])
    graphics::points(dat, col = cols[i], pch = symbols[i])
    i <- i + 1
  }
  
  graphics::legend("topleft",
         pch = symbols,
         col = cols,
         legend = paste(names(sub_data), names(sub_data[[1]])),
         bty = "n")
  graphics::axis(1,
                 at = seq(1, length(sub_data[[1]][[1]]$speed)),
                 labels = sub_data[[1]][[1]]$isoTimes,
                 las = 2, 
                 cex.axis = 0.5)
  
  
  graphics::axis(2,
                 at = seq(0, range[2], dy), 
                 labels = seq(0, range[2], dy),
                 las = 2) 
  
  #abline(v=seq(1, length(sub_data[[1]][[1]]$speed)),
  #       lty = 2)
  graphics::abline(h=seq(0, range[2], dy),
                   lty = 2)
  
}

