## code to prepare `sts_wp` dataset goes here

sts_wp = getStorms(basin = "WP", time_period = c(2010,2020), verbose = TRUE)
usethis::use_data(sts_wp, overwrite = TRUE)
