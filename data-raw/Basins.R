## code to prepare `basins` dataset goes here


Basins = data.frame(row.names = c("NA", "SA", "EP", "WP", "SP", "SI", "NI", "ALL"),
                    xmin = c(270, 290, 180, 100, 135, 10, 30, 0),
                    xmax = c(359, 359, 290, 180, 290, 135, 100, 359),
                    ymin = c(0, -60, 0, 0, -60, -60, 0, -60),
                    ymax = c(60, 0, 60, 60, 0, 0, 30, 60))


usethis::use_data(Basins, overwrite = TRUE)
