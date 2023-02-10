## code to prepare `basins` dataset goes here



IBTRACS_sdb = initDatabase()
IBTRACS = loadData(IBTRACS_sdb, "/home/baptiste/Desktop/Travail/StormR/data")


resolutions = c("30sec" = 0.00833333, "2.5min" = 0.04166667, "5min" = 0.08333333, "10min" = 0.1666667)
knt2ms <- 0.514
km <- 1000
wgs84 <- 4326
oceanColor <- "white"
groundColor <- "grey"

Basins <- data.frame(row.names = c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"),
                    xmin = c(270, 290, 180, 100, 135, 10, 30, 0),
                    xmax = c(359, 359, 290, 180, 290, 135, 100, 359),
                    ymin = c(0, -60, 0, 0, -60, -60, 0, -60),
                    ymax = c(60, 0, 60, 60, 0, 0, 30, 60))

sshs <- c(18, 33, 42, 49, 58, 70, 100)

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

usethis::use_data(IBTRACS_sdb, IBTRACS, resolutions, knt2ms, km, wgs84, oceanColor, groundColor, Basins, sshs, sshsPalette, mswSSHSPalette, mswPalette, pdiPalette, exposurePalette, internal = TRUE, overwrite = T)