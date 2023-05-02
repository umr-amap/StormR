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
sshs <- c(18, 33, 42, 49, 58, 70)

basins <- data.frame(
    row.names = c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"),
    xmin = c(270, 290, 180, 100, 135, 10, 30, 0),
    xmax = c(359, 359, 290, 180, 290, 135, 100, 359),
    ymin = c(0, -60, 0, 0, -60, -60, 0, -60),
    ymax = c(60, 0, 60, 60, 0, 0, 30, 60)
)

# Margin
margin <- c(4, 12, 4, 8)

# Color Palettes
oceanColor <- "white"
groundColor <- "grey"

sshsPalette <- c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")


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

# Data for test functions
suppressWarnings(sds <- defDatabase())
pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)
dfGetDataInterpolate <- getDataInterpolate(pam@data[["PAM"]], seq(26, 49), 4, 3, FALSE, "Willoughby")

usethis::use_data(resolutions,
    mph2msC, knt2msC, kmh2msC, nm2kmC, b2paC, mb2paC, psi2paC, atm2paC,
    km, wgs84, Basins, sshs,
    margin,
    oceanColor, groundColor, sshsPalette, mswSSHSPalette, mswPalette, pdiPalette, exposurePalette,
    dfGetDataInterpolate,
    internal = TRUE, overwrite = TRUE
)
