## code to prepare `pam` dataset goes here

pam = getStorms(time_period = 2015, name = "PAM", loi = "Vanuatu")
usethis::use_data(pam, overwrite = TRUE)
