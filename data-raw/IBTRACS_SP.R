## code to prepare `IBTRACS_SP` dataset goes here
IBTRACS_SP <- defDatabase()

ind <- c(1170, 1173, 1179, 1182, 1185, 1219, 1223, 1233, 1235)

IBTRACS_SP@database$names <- IBTRACS_SP@database$names[ind]
IBTRACS_SP@database$seasons <- IBTRACS_SP@database$seasons[ind]
IBTRACS_SP@database$isotimes <- IBTRACS_SP@database$isotimes[,ind]
IBTRACS_SP@database$longitude <- IBTRACS_SP@database$longitude[,ind]
IBTRACS_SP@database$latitude <- IBTRACS_SP@database$latitude[,ind]
IBTRACS_SP@database$msw <- IBTRACS_SP@database$msw[,ind]
IBTRACS_SP@database$sshs <- IBTRACS_SP@database$sshs[,ind]
IBTRACS_SP@database$rmw <- IBTRACS_SP@database$rmw[,ind]
IBTRACS_SP@database$pressure <- IBTRACS_SP@database$pressure[,ind]
IBTRACS_SP@database$poci <- IBTRACS_SP@database$poci[,ind]


usethis::use_data(IBTRACS_SP, overwrite = TRUE)
