## code to prepare `IBTRACS_SP` dataset goes here
IBTRACS_SP <- initDatabase()
usethis::use_data(IBTRACS_SP, overwrite = TRUE)
