devtools::load_all()
library(Rcpp)
library(rbenchmark)

sd <- defStormsDataset()
sts <- defStormsList(sd, loi = "Vanuatu", names = "PAM")
st <- sts@data$PAM
storm <- st
wgs84 <- 4326

product <- "MSW"
windThreshold <- c(18, 33, 42, 49, 58, 70)
method <- "Boose"
asymmetry <- "Chen"
empiricalRMW <- FALSE
spaceRes <- "2.5min"
tempRes <- 1
loi <- sts@spatialLoiBuffer

rasterTemplate <- makeTemplateRaster(sts@spatialLoiBuffer, resolutions[spaceRes])

world <- rworldmap::getMap(resolution = "high")
world <- sf::st_as_sf(world)
world <- sf::st_transform(world, crs = wgs84)
indCountries <- which(sf::st_intersects(sts@spatialLoiBuffer, world$geometry, sparse = FALSE) == TRUE)

buffer <- 2.5

# Handling indices inside loi.buffer or not
ind <- getIndices(storm, 2, product)

it1 <- storm@obs.all$iso.time[1]
it2 <- storm@obs.all$iso.time[2]
timeDiff <- as.numeric(as.POSIXct(it2) - as.POSIXct(it1))
# Interpolated time step dt, default value dt <- 4 --> 1h
dt <- 1 + (1 / tempRes * timeDiff) # + 1 for the limit values
# Getting data associated with storm st
dataTC <- getDataInterpolate(storm, ind, dt, timeDiff, empiricalRMW, method)
nbStep <- dim(dataTC)[1] - 1

j <- 1

rasterTemplateModel <- makeTemplateModel(rasterTemplate, buffer, dataTC, j)
rasterWind <- rasterTemplateModel
rasterDirection <- rasterTemplateModel
# Computing coordinates of raster
crds <- terra::crds(rasterWind, na.rm = FALSE)
# Computing distances in degree to the eye of the storm for x and y axes
x <- crds[, 1] - dataTC$lon[j]
y <- crds[, 2] - dataTC$lat[j]

distEye <- terra::distance(x = crds, y = cbind(dataTC$lon[j], dataTC$lat[j]), lonlat = TRUE)
data <- dataTC
index <- j

msw <- dataTC$msw[index]
lat <- dataTC$lat[index]
r <- distEye * 0.001
rmw <- dataTC$rmw[index]
pc <- dataTC$pc[index]
poci <- dataTC$poci[index]
vx <- dataTC$vx[index]
vy <- dataTC$vy[index]
vh <- dataTC$stormSpeed[index]

benchmark(willoughby(r, rmw, msw, lat), willoughby_cpp(r, rmw, msw, lat))[,1:4]
all(willoughby(r, rmw, msw, lat) == willoughby_cpp(r, rmw, msw, lat))
benchmark(holland(r, rmw, msw, pc, poci, lat), holland_cpp(r, rmw, msw, pc, poci, lat))[,1:4]
all(holland(r, rmw, msw, pc, poci, lat) == holland_cpp(r, rmw, msw, pc, poci, lat))

pts <- sf::st_as_sf(as.data.frame(crds), coords = c("x", "y"))
sf::st_crs(pts) <- wgs84
landIntersect <- rep(0, length(x))
for (i in indCountries) {
  ind <- which(sf::st_intersects(pts, world$geometry[i], sparse = FALSE) == TRUE)
  landIntersect[ind] <- 1
}

benchmark(boose(r, rmw, msw, pc, poci, x, y, vx, vy, vh, landIntersect, lat), boose_cpp(r, rmw, msw, pc, poci, x, y, vx, vy, vh, landIntersect, lat))[,1:4]
all(boose(r, rmw, msw, pc, poci, x, y, vx, vy, vh, landIntersect, lat) == boose_cpp(r, rmw, msw, pc, poci, x, y, vx, vy, vh, landIntersect, lat))

wind <- boose(r, rmw, msw, pc, poci, x, y, vx, vy, vh, landIntersect, lat)

benchmark(computeDirection(x, y, lat), computeDirection_cpp(x, y, lat))[,1:4]
all(computeDirection(x, y, lat) == computeDirection_cpp(x, y, lat))

benchmark(computeDirectionBoose(x, y, lat, landIntersect), computeDirectionBoose_cpp(x, y, lat, landIntersect), replications = 1000, columns = c("test", "elapsed", "relative"))
all(computeDirectionBoose(x, y, lat, landIntersect) == computeDirectionBoose_cpp(x, y, lat, landIntersect))

benchmark(computeAsymmetry("Chen", wind, direction, x, y, vx, vy, vh, r, rmw, lat), computeAsymmetry_cpp("Chen", wind, x, y, vx, vy, vh, r, rmw, lat))[,1:4]
all(computeAsymmetry("Chen", wind, direction, x, y, vx, vy, vh, r, rmw, lat) == computeAsymmetry_cpp("Chen", wind, x, y, vx, vy, vh, r, rmw, lat))

benchmark(computePDI(wind, tempRes), computePDI_cpp(wind, tempRes))[,1:4]
all(computePDI(wind, tempRes) == computePDI_cpp(wind, tempRes))
benchmark(computeExposure(wind, tempRes, windThreshold), computeExposure_cpp(wind, tempRes, windThreshold))[,1:4]
all(computeExposure(wind, tempRes, windThreshold) == computeExposure_cpp(wind, tempRes, windThreshold))
