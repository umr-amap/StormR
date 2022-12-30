## code to prepare `sts_nc` dataset goes here

sts_nc = getStorms(time_period = c(2003,2021), name = c("ERICA","NIRAN"), loi = "New Caledonia")
usethis::use_data(sts_nc, overwrite = TRUE)
