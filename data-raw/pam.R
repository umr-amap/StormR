## code to prepare `pam` dataset goes here

pam = getStorms(seasons = 2015, names = "PAM", loi = "Vanuatu")
usethis::use_data(pam, overwrite = TRUE)
