## code to prepare `basins` dataset goes here


Basins = data.frame(row.names = c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"),
                    xmin = c(270, 290, 180, 100, 135, 10, 30, 0),
                    xmax = c(359, 359, 290, 180, 290, 135, 100, 359),
                    ymin = c(0, -60, 0, 0, -60, -60, 0, -60),
                    ymax = c(60, 0, 60, 60, 0, 0, 30, 60))

sshs = c(18, 33, 42, 49, 58, 70, 100)

sshsPalette = c("#00CCFF", "#00CCCC", "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")

usethis::use_data(Basins, sshs, sshsPalette, internal = TRUE, overwrite = T)
