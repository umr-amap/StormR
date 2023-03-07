## code to prepare `basins` dataset goes here



#Data for routines
resolutions = c("30sec" = 0.00833333, "2.5min" = 0.04166667, "5min" = 0.08333333, "10min" = 0.1666667)
mph2ms <- 0.44704
knt2ms <- 0.514
kmh2ms <- 1/3.6
nm2km <- 1.852
b2pa <- 100000
mb2pa <- 100
psi2pa <- 6895
atm2pa <- 101300
km <- 1000
wgs84 <- 4326
sshs <- c(18, 33, 42, 49, 58, 70, 100)

Basins <- data.frame(row.names = c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"),
                    xmin = c(270, 290, 180, 100, 135, 10, 30, 0),
                    xmax = c(359, 359, 290, 180, 290, 135, 100, 359),
                    ymin = c(0, -60, 0, 0, -60, -60, 0, -60),
                    ymax = c(60, 0, 60, 60, 0, 0, 30, 60))

#Color Palettes
oceanColor <- "white"
groundColor <- "grey"

sshsPalette <- c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")


palette <- c("#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
x <- seq(18,80)
y <- x
color_range <- colorRampPalette(sshsPalette[2:7], bias = 0.8)
mswSSHSPalette <- color_range(63)

plot(x, y, col = mswSSHSPalette, lwd = 3)
abline(v = sshs)

mswPalette <- rev(grDevices::heat.colors(50))
pdiPalette <- rev(viridis::inferno(50))
exposurePalette <- rev(viridis::viridis(50))

#Data for test functions
df_getDataInterpolate <-getDataInterpolate(pam@data[["PAM"]], seq(26,49), 4, 3, FALSE, "Willoughby")

#Map for intersection
world <- rworldmap::getMap(resolution = "high")
world <- sf::st_as_sf(world)
world <- sf::st_transform(world, crs = wgs84)


usethis::use_data(resolutions,
                  mph2ms, knt2ms, kmh2ms, nm2km, b2pa, mb2pa, psi2pa, atm2pa,
                  km,wgs84, Basins, sshs,
                  oceanColor, groundColor, sshsPalette, mswSSHSPalette, mswPalette, pdiPalette, exposurePalette,
                  df_getDataInterpolate,
                  world,
                  internal = TRUE, overwrite = T)
