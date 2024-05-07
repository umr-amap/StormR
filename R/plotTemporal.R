

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
  stopifnot("data must be temporal series. Be sure to use data generated from temporalBehaviour()
  function run with product input set to 'TS'" = identical(class(data[[1]]), "list"))

  # Checking storm input
  stopifnot("You must select one single storm" = !missing(storm))
  if (identical(class(storm), "character")) {
    stopifnot("storm not find in data" = (storm %in% names(data)))
  }else if (identical(class(storm), "numeric")) {
    stopifnot("storm index out of bound in data" = (storm %in% c(1, length(names(data)))))
  }else {
    stop("Invalid storm input")
  }

  # Checking var input
  stopifnot("invalid var input, must be either 'speed' or 'direction'" =
              var == "speed" | var == "direction")

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
#' sds <- defStormsDataset()
#' st <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)
#'
#' df <- data.frame(x = c(168.33, 167.17), y = c(-17.73, -15.53))
#' rownames(df) <- c("Port_Vila", "Luganville")
#'
#' # Generate temporal series of wind on the points
#' TS <- temporalBehaviour(st, points = df, product = "TS", tempRes = 30, verbose = 0)
#'
#' # Plot temporal series of wind speed
#' plotTemporal(data=TS, storm="PAM")
#'
#' # Plot temporal series of wind direction
#' plotTemporal(data=TS, storm="PAM", var='direction')
#' }
plotTemporal <- function(data, storm, var = "speed") {

  checkInputsPlotTemporal(data, storm, var)

  # Filter by storm
  subData <- data[storm]

  nbOfPositions <- length(subData[[1]])

  # Generate a sequence of colors

  # TODO pb of dependencies here...
  palette <- grDevices::colorRampPalette(colors = c("red", "green", "blue"))
  cols <- palette(nbOfPositions)

  # Generate a sequence of symbols
  symbols <- seq(1, nbOfPositions, 1)



  notNaIndices <- which(!is.na(subData[[1]][[1]]$speed))

  # Define x-axis range
  for (location in subData[[1]][c(2:nbOfPositions)]) {

    notNaIndices <- union(notNaIndices,
                          which(!is.na(location$speed)))
  }

  notNaIndices <- seq(min(notNaIndices), max(notNaIndices))



  # Plot setting depending on the var input
  if (var == "speed") {

    # Set dynamic range of wind speed
    minValue <- c()
    maxValue <- c()
    for (location in subData[[1]]) {
      minValue <- c(minValue, min(location$speed, na.rm = TRUE))
      maxValue <- c(maxValue, max(location$speed, na.rm = TRUE))
    }

    dat <- subData[[1]][[1]]$speed[notNaIndices]
    ylim <- c(floor(min(minValue)) - 10, ceiling(max(maxValue) + 10))
    ylab <- "Maximum Sustained Wind speed (m/s)"
    dy <- 5 # dashed lines every 5m/s on the plot
  }else {
    dat <- subData[[1]][[1]]$direction[notNaIndices]
    ylim <- c(0, 360)
    ylab <- "Wind direction (degree)"
    dy <- 15 # dashed lines every 5m/s on the plot
  }

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

    if (var == "speed") {
      dat <- location$speed[notNaIndices]
    }else {
      dat <- location$direction[notNaIndices]
    }

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

  if(diffTime == 30){
    notNullLabels <- seq(1,length(labels)) %% 2 == 0
    labels[notNullLabels] <- ""
  }else if(diffTime == 15){
    notNullLabels <- seq(1,length(labels)) %% 4 == 0
    labels[notNullLabels] <- ""
  }

  graphics::axis(1,
                 at = seq(1, length(subData[[1]][[1]]$speed[notNaIndices])),
                 labels = labels,
                 las = 2,
                 cex.axis = 0.5)


  graphics::axis(2,
                 at = seq(0, ylim[2], dy),
                 labels = seq(0, ylim[2], dy),
                 las = 2)

  #abline(v=seq(1, length(subData[[1]][[1]]$speed)),
  #       lty = 2)
  graphics::abline(h = seq(0, ylim[2], dy),
                   lty = 2)

}
