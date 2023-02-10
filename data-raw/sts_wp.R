## code to prepare `sts_wp` dataset goes here

sts_wp = getStorms(seasons = c(2010,2020))
usethis::use_data(sts_wp, overwrite = TRUE)
