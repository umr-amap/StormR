## code to prepare `test_dataset` dataset goes here
test_dataset <- defDatabase(filename = "/home/baptiste/Desktop/Travail/StormR/data/IBTrACS.SP.v04r00.nc")


#st <- Storms(loi = "Vanuatu", seasons = c(2015, 2016), verbose = 0)
#st <- Storms(loi = "New Caledonia", seasons = c(2020, 2021), verbose = 0)
#Change here
ind <- c(449, 452, 458, 461, 464,
         499, 503, 513, 515)

test_dataset@database$names <- test_dataset@database$names[ind]
test_dataset@database$seasons <- test_dataset@database$seasons[ind]
test_dataset@database$isotimes <- test_dataset@database$isotimes[,ind]
test_dataset@database$longitude <- test_dataset@database$longitude[,ind]
test_dataset@database$latitude <- test_dataset@database$latitude[,ind]
test_dataset@database$msw <- test_dataset@database$msw[,ind]
test_dataset@database$sshs <- test_dataset@database$sshs[,ind]
test_dataset@database$rmw <- test_dataset@database$rmw[,ind]
test_dataset@database$pressure <- test_dataset@database$pressure[,ind]
test_dataset@database$poci <- test_dataset@database$poci[,ind]
test_dataset@seasons = c(min = 2015, max = 2021)


usethis::use_data(test_dataset, overwrite = TRUE)
