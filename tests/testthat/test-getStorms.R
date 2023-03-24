


test_that("Tests invalid inputs", {

  #Checking sdb input
  expect_error(checkInputsGs(loi = NULL,
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  #Checking loi input
  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = c("Vanuatu", "New Caledonia"),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = 169,
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = c(169,170,-15,-14),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = c(169,170,-15,-14),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = c(169,-100),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))


  #Checking seasons input
  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1960, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2024),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = 2015.7,
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = TRUE,
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = "2015",
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  #Checking names input
  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = TRUE,
                             names = 123,
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = TRUE,
                             names = TRUE,
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2000, 2020),
                             names = NULL,
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))


  #Checking max_dist input
  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = "hui",
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = c(1,3),
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = -10,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = TRUE,
                             verbose = 2,
                             remove_TD = TRUE))

  #Checking verbose input
  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = -1,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = c(1, 3),
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = "hui",
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = TRUE,
                             remove_TD = TRUE))

  #Checking remove_TC input
  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = 1))

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = "hui"))

})






test_that("Storm class getters", {
  
 
  sds <- defDatabase()
  pam <- Storms(sds = sds, loi = "Vanuatu", names = "PAM")

  expect_identical(getNames(pam@data[["PAM"]]), "PAM")
  expect_identical(getSeasons(pam@data[["PAM"]]), 2015)
  expect_identical(getSSHS(pam@data[["PAM"]]), 5)
  expect_identical(getNbObs(pam@data[["PAM"]]), as.integer(57))
  expect_identical(getObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs.all)
  expect_identical(getInObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs)

})





test_that("StormsList class getters", {

  
  sds <- defDatabase()
  sts_nc <- Storms(sds = sds, loi = "New Caledonia")
  

  expect_identical(getStorm(sts_nc, "NIRAN"), sts_nc@data[["NIRAN"]])
  # expect_identical(getNames(sts_nc), c("PAM", "SOLO", "ULA", "UESI", "GRETEL", "LUCAS", "NIRAN"))
  # expect_identical(getSeasons(sts_nc), c("PAM" = as.integer(2015), "SOLO" = as.integer(2015), "ULA" =  as.integer(2016), "UESI" =  as.integer(2020), "GRETEL" = as.integer(2020), "LUCAS" = as.integer(2021), "NIRAN" = as.integer(2021)))
  # expect_identical(getSSHS(sts_nc), c("PAM" = as.integer(5), "SOLO" = as.integer(0), "ULA" =  as.integer(4), "UESI" = as.integer(1), "GRETEL" = as.integer(1), "LUCAS" = as.integer(1), "NIRAN" =  as.integer(5)))
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)
  expect_identical(getLOI(sts_nc), sts_nc@spatial.loi)
  expect_identical(getBuffer(sts_nc), sts_nc@spatial.loi.buffer)
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)

})





test_that("Storms class getters for Storm class", {
  
  
  sds <- defDatabase()
  sts_nc <- Storms(sds = sds, loi = "New Caledonia")

  expect_identical(getNbObs(sts_nc, "NIRAN"), getNbObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getObs(sts_nc, "NIRAN"), getObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getInObs(sts_nc, "NIRAN"), getInObs(getStorm(sts_nc, "NIRAN")))

})





# test_that("Test convert loi function", {
#
#   expect_identical(convertLoi("Vanuatu"), pam@spatial.loi)
#
# })





# test_that("Test makeBuffer function", {
#
#   expect_identical(makeBuffer(pam@spatial.loi, 300 * km), pam@spatial.loi.buffer)
#
# })





test_that("Test retrieveStorms function", {
  
  
  sds <- defDatabase()
# 
#   expect_identical(retrieveStorms(sds@database, "PAM", c(2015,2021), TRUE), as.integer(1))

})





test_that("Test writeStorm function", {
  
  sds <- defDatabase()
  pam <- Storms(sds = sds, loi = "Vanuatu", names = "PAM")
# 
#   expect_identical(writeStorm(list(), list(), sds, 2, getBuffer(pam)),
#                    list(list(getStorm(pam,"PAM")),list("PAM")))

})





# test_that("Test Storms function", {
#
#   expect_identical(Storms(loi = "Vanuatu", names = "PAM", verbose = 0), pam)
#
# })
