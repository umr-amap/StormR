## code to prepare `sts_nc` dataset goes here

sts_nc = getStorms(loi = "New Caledonia", names = c("ERICA","NIRAN"))
usethis::use_data(sts_nc, overwrite = TRUE)
