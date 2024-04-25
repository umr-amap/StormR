## Various data needed by StormR

# Data for routines
resolutions <- c("30sec" = 0.00833333, "2.5min" = 0.04166667, "5min" = 0.08333333, "10min" = 0.1666667)
# Unit conversion constants
mph2msC <- 0.44704
knt2msC <- 0.514
kmh2msC <- 1 / 3.6
nm2kmC <- 1.852
b2paC <- 100000
mb2paC <- 100
psi2paC <- 6895
atm2paC <- 101300

km <- 1000

wgs84 <- 4326

# SSHS constants

sshs <- c(18, 33, 42, 49, 58, 70)
sshsPalette <- c("#00CCFF",
                 "#00CCCC",
                 "#FFFFB2",
                 "#FECC5C",
                 "#FD8D3C",
                 "#F03B20",
                 "#BD0026")

names(sshsPalette) <- c("TD",
                        "TS",
                        "Cat. 1",
                        "Cat. 2",
                        "Cat. 3",
                        "Cat. 4",
                        "Cat. 5")

northAtlantic <- sf::st_polygon(list(cbind(c(359, 359, 298, 300, 289, 282, 279, 263, 266, 281, 304, 308, 320, 359),
                                           c(0, 60, 60, 50, 42, 34, 30, 28, 18, 10, 7, 0, 0, 0))))
southAtlantic <- sf::st_polygon(list(cbind(c(290, 359, 359, 290, 290),
                                           c(-60, -60, 0, 0, -60))))
eastPacific <- sf::st_polygon(list(cbind(c(180, 180, 210, 217, 236, 236, 240, 250, 265, 280, 180),
                                         c(0, 60, 60, 60, 49, 40, 34, 25, 15, 0, 0))))
westPacific <- sf::st_polygon(list(cbind(c(85, 180, 180, 85, 85),
                                         c(0, 0, 60, 60, 0))))
southPacific <- sf::st_polygon(list(cbind(c(135, 290, 290, 135, 135),
                                          c(-60, -60, 0, 0, -60))))
southIndian <- sf::st_polygon(list(cbind(c(10, 135, 135, 10, 10),
                                         c(-60, -60, 0, 0, -60))))
northIndian <- sf::st_polygon(list(cbind(c(30, 100, 100, 30, 30),
                                         c(0, 0, 60, 60, 0))))
global <- sf::st_polygon(list(cbind(c(0, 359, 359, 0, 0),
                                    c(-60, -60, 60, 60, -60))))

all_basins <- sf::st_sfc(northAtlantic,
                         southAtlantic,
                         eastPacific,
                         westPacific,
                         southPacific,
                         southIndian,
                         northIndian,
                         global,
                         crs = wgs84)
all_basins_names <- data.frame(Name = c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"))
basins <- sf::st_sf(all_basins_names, geometry = all_basins)

# Margin
margin <- c(4, 12, 4, 8)

# Color Palettes
oceanColor <- "white"
groundColor <- "grey"

palette <- c("#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
xsup <- 95
xinf <- 18
nbC <- xsup - xinf
x <- seq(xinf, xsup)
y <- x
colorRange <- colorRampPalette(sshsPalette[2:7], bias = 1)
mswSSHSPalette <- colorRange(nbC)
plot(x, y, col = mswSSHSPalette, lwd = 3)
abline(v = sshs)
mswPalette <- rev(grDevices::heat.colors(50))
pdiPalette <- rev(viridis::inferno(50))
exposurePalette <- rev(viridis::viridis(50))

################################################################################
########### Data for the tests of functions ####################################
################################################################################

# spatialBehaviour functions
suppressWarnings(sds <- defStormsDataset())
pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)
dfGetDataInterpolate <- getDataInterpolate(pam@data[["PAM"]], seq(26, 49), 60, FALSE, "Willoughby")
mswPam <- spatialBehaviour(pam)
mapPam <- plotStorms(pam, dynamicPlot = TRUE)
mapPamMsw <- plotBehaviour(pam, mswPam, dynamicPlot = TRUE)


# defStormsDataset functions
sdsFromNc <- defStormsDataset(seasons = c(2015, 2020))
sdsFromCsv <- defStormsDataset(filename = system.file("extdata", "test_dataset.csv", package = "StormR"),
                               seasons = c(2015, 2020))



usethis::use_data(resolutions,
  mph2msC, knt2msC, kmh2msC, nm2kmC, b2paC, mb2paC, psi2paC, atm2paC,
  km, wgs84, basins, sshs,
  margin,
  oceanColor, groundColor, sshsPalette, mswSSHSPalette, mswPalette, pdiPalette, exposurePalette,
  dfGetDataInterpolate,
  sdsFromNc, sdsFromCsv,
  mapPam, mapPamMsw,
  internal = TRUE, overwrite = TRUE
)

