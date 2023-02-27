


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

  expect_error(checkInputsGs(sds = IBTRACS_SP,
                             loi = c(-180,-15),
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

  expect_identical(getStorm_name(pam@data[["PAM"]]), pam@data[["PAM"]]@name)
  expect_identical(getStorm_season(pam@data[["PAM"]]), pam@data[["PAM"]]@season)
  expect_identical(getStorm_sshs(pam@data[["PAM"]]), pam@data[["PAM"]]@sshs)
  expect_identical(getStorm_nbObs(pam@data[["PAM"]]), pam@data[["PAM"]]@numobs.all)
  expect_identical(getStorm_obs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs.all)
  expect_identical(getStorm_inObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs)

})





test_that("Storms class getters", {

  expect_identical(getStorm(sts_nc, "NIRAN"), sts_nc@data[["NIRAN"]])
  expect_identical(getNames(sts_nc), sts_nc@names)
  expect_identical(getSeasons(sts_nc), sts_nc@seasons)
  expect_identical(getSeasons(sts_nc, "NIRAN"), sts_nc@seasons[which(sts_nc@names == "NIRAN")])
  expect_identical(getSSHS(sts_nc), sts_nc@sshs)
  expect_identical(getSSHS(sts_nc, "NIRAN"), sts_nc@sshs[which(sts_nc@names == "NIRAN")])
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)
  expect_identical(getLOI(sts_nc), sts_nc@spatial.loi)
  expect_identical(getBuffer(sts_nc), sts_nc@spatial.loi.buffer)
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)

})





test_that("Storms class getters for Storm class", {

  expect_identical(getNbObs(sts_nc, "NIRAN"), getStorm_nbObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getObs(sts_nc, "NIRAN"), getStorm_obs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getInObs(sts_nc, "NIRAN"), getStorm_inObs(getStorm(sts_nc, "NIRAN")))

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

  expect_identical(retrieveStorms(IBTRACS_SP@database, "PAM", c(1980,2022), TRUE), as.integer(c(986, 1170)))

})





test_that("Test writeStorm function", {

  expect_identical(writeStorm(list(), list(), list(), list(), 0, IBTRACS_SP, 1170, getBuffer(pam)),
                   list(list(getStorm(pam,"PAM")),list("PAM"), list(as.integer(2015)), list(as.integer(5)), 1))

})





# test_that("Test getStorms function", {
#
#   expect_identical(getStorms(loi = "Vanuatu", names = "PAM", verbose = 0), pam)
#
# })
