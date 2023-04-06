


test_that("Tests invalid inputs", {
  
  suppressWarnings(sds <- defDatabase(verbose = F))

  #Checking sdb input
  expect_error(checkInputsGs(loi = NULL,
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  #Checking loi input
  expect_error(checkInputsGs(sds = sds,
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = c("Vanuatu", "New Caledonia"),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = 169,
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = c(169,170,-15,-14),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = c(169,170,-15,-14),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = c(169,-100),
                             seasons = c(1980, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))


  #Checking seasons input
  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1960, 2022),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2024),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = 2015.7,
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = TRUE,
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = "2015",
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  #Checking names input
  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = TRUE,
                             names = 123,
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = TRUE,
                             names = TRUE,
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2000, 2020),
                             names = NULL,
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = TRUE))


  #Checking max_dist input
  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = "hui",
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = c(1,3),
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = -10,
                             verbose = 2,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = TRUE,
                             verbose = 2,
                             remove_TD = TRUE))

  #Checking verbose input
  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = -1,
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = c(1, 3),
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = "hui",
                             remove_TD = TRUE))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = TRUE,
                             remove_TD = TRUE))

  #Checking remove_TC input
  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = 1))

  expect_error(checkInputsGs(sds = sds,
                             loi = "Vanuatu",
                             seasons = c(1980, 2020),
                             names = "PAM",
                             max_dist = 300,
                             verbose = 2,
                             remove_TD = "hui"))

})






test_that("Storm class getters", {
  
 
  suppressWarnings(sds <- defDatabase(verbose = F))
  pam <- Storms(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(getNames(pam@data[["PAM"]]), "PAM")
  expect_identical(getSeasons(pam@data[["PAM"]]), 2015)
  expect_identical(getSSHS(pam@data[["PAM"]]), 5)
  expect_identical(getNbObs(pam@data[["PAM"]]), as.integer(57))
  expect_identical(getObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs.all)
  expect_identical(getInObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs)

})





test_that("StormsList class getters", {

  
  suppressWarnings(sds <- defDatabase(verbose = F))
  sts_nc <- Storms(sds = sds, loi = "New Caledonia", verbose = 0)
  

  expect_identical(getStorm(sts_nc, "NIRAN"), sts_nc@data[["NIRAN"]])
  expect_identical(getNames(sts_nc), c("PAM", "SOLO", "ULA", "UESI", "GRETEL", "LUCAS", "NIRAN"))
  expect_identical(getSeasons(sts_nc), c("PAM" = 2015, "SOLO" = 2015, "ULA" = 2016, "UESI" = 2020, "GRETEL" = 2020, "LUCAS" = 2021, "NIRAN" = 2021))
  expect_identical(getSSHS(sts_nc), c("PAM" = 5, "SOLO" = 0, "ULA" = 4, "UESI" = 1, "GRETEL" = 1, "LUCAS" = 1, "NIRAN" = 5))
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)
  expect_identical(getLOI(sts_nc), sts_nc@spatial.loi)
  expect_identical(getBuffer(sts_nc), sts_nc@spatial.loi.buffer)
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)

})

test_that("Storm and StormsList class getters", {
  suppressWarnings(sds <- defDatabase(verbose = F))
  sts_nc <- Storms(sds = sds, loi = "New Caledonia", verbose = 0)
  out <- capture_output_lines(show(sts_nc@data$PAM))
  
  # Check that the Storm output is correct
  expect_match(out[1], "Name: PAM")
  expect_match(out[2], "Season: 2015")
  expect_match(out[3], "Maximum category reached \\(SSHS\\): 5")
  expect_match(out[4], "Indices of observations within buffer: 43 44 45 46 47")
  expect_match(out[7], "1  2015-03-08 12:00:00 168.9000  -7.500000  13   -1  93 100400 100500")
  expect_match(tail(out, n=1), "57 2015-03-15 12:00:00 178.5000 -33.799999  28   -4  37  98200  99300")

  out <- capture_output_lines(show(sts_nc))
    # Check that the Storm output is correct
  expect_match(out[3], "Number of Storms: 7 ")
  expect_match(out[7], "Name: PAM")
  expect_match(out[8], "Season: 2015")
  expect_match(out[72], "Name: SOLO")
  expect_match(out[73], "Season: 2015")
  expect_match(out[109], "Name: ULA")
  expect_match(out[110], "Season: 2016")
  expect_match(out[236], "Name: UESI")
  expect_match(out[237], "Season: 2020")
  expect_match(out[311], "Name: GRETEL")
  expect_match(out[312], "Season: 2020")
  expect_match(out[346], "Name: LUCAS")
  expect_match(out[347], "Season: 2021")
  expect_match(out[403], "Name: NIRAN")
  expect_match(out[404], "Season: 2021")


})



test_that("Storms class getters for Storm class", {
  
  
  suppressWarnings(sds <- defDatabase(verbose = FALSE))
  sts_nc <- Storms(sds = sds, loi = "New Caledonia", verbose = 0)

  expect_identical(getNbObs(sts_nc, "NIRAN"), getNbObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getObs(sts_nc, "NIRAN"), getObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getInObs(sts_nc, "NIRAN"), getInObs(getStorm(sts_nc, "NIRAN")))

})





test_that("Test convert loi function", {
  
  suppressWarnings(sds <- defDatabase(verbose = F))
  pam <- Storms(sds, loi = "Vanuatu", names = "PAM", verbose = 0)
  

  expect_identical(convertLoi("Vanuatu"), pam@spatial.loi)

})





test_that("Test makeBuffer function", {
  
  suppressWarnings(sds <- defDatabase(verbose = F))
  pam <- Storms(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(makeBuffer("Vanuatu",pam@spatial.loi, 300 * km), pam@spatial.loi.buffer)

})





test_that("Test retrieveStorms function", {

  suppressWarnings(sds <- defDatabase(verbose = F))
  pam <- Storms(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(retrieveStorms(sds@database, "PAM", c(2015,2021), TRUE), as.integer(1))

})





test_that("Test writeStorm function", {
  
  suppressWarnings(sds <- defDatabase(verbose = F))
  pam <- Storms(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(writeStorm(list(), list(), sds, 1, getBuffer(pam)),
                   list(list(getStorm(pam,"PAM")),list("PAM")))

})





# test_that("Test Storms function", {
#
#   expect_identical(Storms(loi = "Vanuatu", names = "PAM", verbose = 0), pam)
#
# })
