## code to prepare `sts_nc` dataset goes here

sts_nc = getStorms(seasons = c(2003,2021), names = c("ERICA","NIRAN"), loi = "New Caledonia")
usethis::use_data(sts_nc, overwrite = TRUE)
