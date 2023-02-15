## code to prepare `IBTRACS_SP` dataset goes here
IBTRACS_SP <- initDatabase()
IBTRACS_SP <- collectData(IBTRACS_SP)
usethis::use_data(IBTRACS_SP, overwrite = TRUE)
