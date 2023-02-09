## code to prepare `zee-nc` dataset goes here

eezNC = sf::st_read("/home/baptiste/Desktop/Travail/StormR/data/zeeNC/zee-nc.shp")

usethis::use_data(eezNC, overwrite = TRUE)
