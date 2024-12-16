devtools::load_all()
points <- data.frame(lon = c(168.33, 167.17), lat = c(-17.73, -15.53))
rownames(points) <- c("Port_Vila", "Luganville")
sds <- defStormsDataset()
st <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)

### spatialBehaviour
pf <- spatialBehaviour(st, verbose = 1)

verbose <- 1
product <- "Profiles"
windThreshold <- st@scale
method <- "Willoughby"
asymmetry <- "Chen"
empiricalRMW <- FALSE
spaceRes <- "2.5min"
tempRes <- 60
spatialResolution <- resolutions[spaceRes]
rasterTemplate <- makeTemplateRaster(st@spatialLoiBuffer, spatialResolution)
extent <- terra::ext(rasterTemplate)
nbg <- switch(spaceRes,
  "30sec" = 59,
  "2.5min" = 11,
  "5min" = 5,
  "10min" = 3
)
buffer_size <- 2.5
countriesGeometryInLoi <- if (method == "Boose") getCountriesInLoi(sts@spatialLoiBuffer) else NULL
rasters <- initRasters(rasterTemplate, getNbStorms(sts), product, windThreshold)
s <- 1 # Initializing count of storms
storm <- st@data[[1]]
indicesInLoi <- getIndices(storm, 2, product)
dataTC <- getDataInterpolate(storm, indicesInLoi, tempRes, empiricalRMW, method)
nbSteps <- dim(dataTC)[1] - 1
stormsSpeed <- c()
stormsDirection <- c()
j <- 1
local_extent <- data.frame(
  dataTC$lon[j] - buffer_size,
  dataTC$lon[j] + buffer_size,
  dataTC$lat[j] - buffer_size,
  dataTC$lat[j] + buffer_size
)
names(local_extent) <- c("xmin", "xmax", "ymin", "ymax")
rasterTemplateTimeStep <- makeTemplateRaster(local_extent, spatialResolution, dataTC$isoTimes[j])
eye <- cbind(dataTC$lon[j], dataTC$lat[j])
distEyeKm <- computeDistanceEyeKm(rasterTemplateTimeStep, eye)
distEyeDeg <- computeDistanceEyeDeg(rasterTemplateTimeStep, eye)
output <- computeWindProfile(storm@name,
  dataTC[j, ], method, asymmetry, distEyeKm, distEyeDeg,
  st@spatialLoiBuffer, countriesGeometryInLoi, rasterTemplateTimeStep
)
# in case this does not work
data <- dataTC[j, ]
loi <- st@spatialLoiBuffer
countries <- countriesGeometryInLoi
points <- rasterTemplateTimeStep
landIntersect <- NULL
speed <- willoughby(
  r = distEyeKm, rmw = data$rmw, msw = data$msw, lat = data$lat
)
names(speed) <- paste0(name, "_Speed_", data["indices"])
direction <- terra::rast(
  x = points,
  vals = terra::values(
    computeDirection(method,
                     distEyeDeg$lon,
                     distEyeDeg$lat,
                     data$lat,
                     landIntersect),
  ),
  names = paste0(name, "_Direction_", data["indices"])
)
output <- computeAsymmetry(
  asymmetry, speed, direction,
  data$vxDeg, data$vyDeg, data$stormSpeed, distEyeKm,
  data$rmw, data$lat
)



stormSpeed[[j]] <- moveOnLoi(output$speed, rasterTemplate, extent)
stormDirection[[j]] <- moveOnLoi(output$direction, rasterTemplate, extent)
names(stormSpeed[[j]]) <- paste0(storm@name, "_Speed_", dataTC$indices[j])
names(stormDirection[[j]]) <- paste0(storm@name, "_Direction_", dataTC$indices[j])
terra::time(stormSpeed[[j]]) <- terra::time(output$speed)
terra::time(stormDirection[[j]]) <- terra::time(output$direction)

# Does not work
#TS <- temporalBehaviour(st, points = df, product = "TS", tempRes = 30, verbose = 0)

# Manual editting of the function "temporalBehaviour"
product <- "TS"
windThreshold <- NULL
method <- "Willoughby"
asymmetry <- "Chen"
empiricalRMW <- FALSE
tempRes <- 30
if (is.null(windThreshold)) {
  windThreshold <- st@scale
}
countriesGeometryInLoi <- if (method == "Boose") getCountriesInLoi(sts@spatialLoiBuffer) else NULL
df$x[df$x < 0] <- df$x[df$x < 0] + 360
finalResult <- list()

storm <- st@data[[1]]
ind <- getIndices(storm, 2, "none")
dataTC <- getDataInterpolate(storm, ind, tempRes, empiricalRMW, method)
nbSteps <- dim(dataTC)[1] - 1
eye <- cbind(dataTC$lon, dataTC$lat)
distEyeKm <- computeDistanceEyeKm(df, eye)
distEyeDeg <- computeDistanceEyeDeg(df, eye)

i <- 1
j <- 1

# Debug computeWindProfile
output <- computeWindProfile(
  dataTC[j, ], method, asymmetry, terra::values(distEyeKm[j, i]),
  distEyeDeg[j, c(i * 2 - 1, i * 2)],
  st@spatialLoiBuffer, countriesGeometryInLoi, df[j, ]
)
output <- computeWindProfile(
  dataTC, method, asymmetry, terra::values(distEyeKm),
  terra::values(distEyeDeg),
  st@spatialLoiBuffer, countriesGeometryInLoi, df
)
landIntersect <- NULL
speed <- willoughby(
  r = terra::values(distEyeKm), rmw = dataTC$rmw,
  msw = dataTC$msw, lat = dataTC$lat
)

direction <- speed
for (i in 1:seq_along(rownames(df))) {
  direction[, i] <- computeDirection(method,
    terra::values(distEyeDeg)[, c(i * 2 - 1, i * 2)][[1]],
    terra::values(distEyeDeg)[, c(i * 2 - 1, i * 2)][[2]],
    data$lat,
    landIntersect
  )
}

landInteraction <- function(lat, direction, landIntersect) {
  if (lat >= 0) {
    direction[landIntersect == 1] <- direction[landIntersect == 1] - 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] - 20
  } else {
    direction[landIntersect == 1] <- direction[landIntersect == 1] + 40
    direction[landIntersect == 0] <- direction[landIntersect == 0] + 20
  }
}
mapply(landIntersect, landInteraction, data$lat, direction)

output <- computeAsymmetry(
  asymmetry, speed, direction,
  data$vxDeg, data$vyDeg, data$stormSpeed, distEyeKm,
  data$rmw, data$lat
)
# computeAsymmetry details
windX <- speed * cos(dir)
windY <- speed * sin(dir)
stormDir <- -(atan2(vy, vx) - pi / 2)
stormDir[stormDir < 0] <- stormDir[stormDir < 0] + 2 * pi
mWindX <- vh * cos(stormDir)
mWindY <- vh * sin(stormDir)
formula <- 3 * rmw**(3 / 2) * r**(3 / 2) / (rmw**3 + r**3 + rmw**(3 / 2) * r**(3 / 2))
tWindX <- windX + formula * mWindX
tWindY <- windY + formula * mWindY
speed <- sqrt(tWindX**2 + tWindY**2)

