## code to prepare `sts_nc` dataset goes here

sts_nc = Storms(loi = "New Caledonia", names = c("ERICA","NIRAN"))
usethis::use_data(sts_nc, overwrite = TRUE)
