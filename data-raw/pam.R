## code to prepare `pam` dataset goes here

pam <- getStorms(loi = "Vanuatu", names = "PAM")
usethis::use_data(pam, overwrite = TRUE)
