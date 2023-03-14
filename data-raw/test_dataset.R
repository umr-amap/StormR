## code to prepare `test_dataset` dataset goes here
test_dataset <- defDatabase(filename = "/home/baptiste/Desktop/Travail/StormR/data/IBTrACS.SP.v04r00.nc")

#Change here
ind <- c(435, 438, 444, 447, 450, 484, 488, 498, 500)

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


usethis::use_data(test_dataset, overwrite = TRUE)
