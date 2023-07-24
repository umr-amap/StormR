


test_that("Tests invalid inputs", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))

  # Checking sdb input
  expect_error(checkInputsDefStormsList(
    loi = NULL,
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  # Checking loi input
  expect_error(checkInputsDefStormsList(
    sds = sds,
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = c("Vanuatu", "New Caledonia"),
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = 169,
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = c(169, 170, -15, -14),
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = c(169, 170, -15, -14),
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = c(169, -100),
    seasons = c(1980, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))


  # Checking seasons input
  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1960, 2022),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2024),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = 2015.7,
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = TRUE,
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = "2015",
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  # Checking names input
  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = TRUE,
    names = 123,
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = TRUE,
    names = TRUE,
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2000, 2020),
    names = NULL,
    maxDist = 300,
    verbose = 2,
    removeTD = TRUE
  ))


  # Checking maxDist input
  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = "hui",
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = c(1, 3),
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = -10,
    verbose = 2,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = TRUE,
    verbose = 2,
    removeTD = TRUE
  ))

  # Checking verbose input
  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = 300,
    verbose = -1,
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = 300,
    verbose = c(1, 3),
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = 300,
    verbose = "hui",
    removeTD = TRUE
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = 300,
    verbose = TRUE,
    removeTD = TRUE
  ))

  # Checking remove_TC input
  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = 1
  ))

  expect_error(checkInputsDefStormsList(
    sds = sds,
    loi = "Vanuatu",
    seasons = c(1980, 2020),
    names = "PAM",
    maxDist = 300,
    verbose = 2,
    removeTD = "hui"
  ))
})


test_that("Test numeric vector loi input", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  sts <- defStormsList(sds = sds, loi = c(168.33, -17.73), verbose = 0)
  expect_identical(getNames(sts), c("PAM", "ZENA", "LUCAS"))
  expect_identical(getObs(getStorm(sts, "LUCAS"))$poci[1:6], c(100100, 100100, 100100, 100200, 100400, 100400))
})




test_that("Storm class getters", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(getNames(pam@data[["PAM"]]), "PAM")
  expect_identical(getSeasons(pam@data[["PAM"]]), 2015)
  expect_identical(getSSHS(pam@data[["PAM"]]), 5)
  expect_identical(getNbObs(pam@data[["PAM"]]), as.integer(57))
  expect_identical(getObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs.all)
  expect_identical(getInObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs)
})





test_that("StormsList class getters", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  sts_nc <- defStormsList(sds = sds, loi = "New Caledonia", verbose = 0)

  expect_identical(getStorm(sts_nc, "NIRAN"), sts_nc@data[["NIRAN"]])
  expect_error(getStorm(sts_nc, "NIRAN", 2015))
  expect_identical(getNames(sts_nc), c("PAM", "SOLO", "ULA", "UESI", "GRETEL", "LUCAS", "NIRAN"))
  expect_identical(
    getSeasons(sts_nc),
    c("PAM" = 2015, "SOLO" = 2015, "ULA" = 2016, "UESI" = 2020, "GRETEL" = 2020, "LUCAS" = 2021, "NIRAN" = 2021)
  )
  expect_identical(
    getSSHS(sts_nc),
    c("PAM" = 5, "SOLO" = 0, "ULA" = 4, "UESI" = 1, "GRETEL" = 1, "LUCAS" = 1, "NIRAN" = 5)
  )
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)
  expect_identical(getLOI(sts_nc), sts_nc@spatialLoi)
  expect_identical(getBuffer(sts_nc), sts_nc@spatialLoiBuffer)
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)
})

test_that("Storm and stormsList class getters", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  sts_nc <- defStormsList(sds = sds, loi = "New Caledonia", verbose = 0)
  out <- capture_output_lines(print(sts_nc@data$PAM))

  # Check that the Storm output is correct
  expect_match(out[1], "Name: PAM")
  expect_match(out[2], "Season: 2015")
  expect_match(out[3], "Maximum category reached \\(SSHS\\): 5")
  expect_match(out[4], "Indices of observations within buffer: 43 44 45 46 47")
  expect_match(out[7], "1  2015-03-08 12:00:00 168.9000  -7.500000  13   -1  93 100400 100500")
  expect_match(tail(out, n = 1), "57 2015-03-15 12:00:00 178.5000 -33.799999  28   -4  37  98200  99300")

  out <- capture_output_lines(print(sts_nc))
  # Check that the Storm output is correct
  expect_match(out[3], "Number of storms: 7 ")
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



test_that("Storms class getters for storm class", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  sts_nc <- defStormsList(sds = sds, loi = "New Caledonia", verbose = 0)

  expect_identical(getNbObs(sts_nc, "NIRAN"), getNbObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getObs(sts_nc, "NIRAN"), getObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getInObs(sts_nc, "NIRAN"), getInObs(getStorm(sts_nc, "NIRAN")))
})





test_that("Test convert loi function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_warning(convertLoi(c(-30, 20)))
  expect_identical(convertLoi("Vanuatu"), pam@spatialLoi)
  expect_identical(sf::st_crs(convertLoi(eezNC))$input, "EPSG:4326")
  expect_identical(
    sf::st_coordinates(convertLoi("SP")),
    sf::st_coordinates(
      sf::st_polygon(list(rbind(c(135, -60), c(290, -60), c(290, 0), c(135, 0), c(135, -60))))
    )
  )

  sr1 <- sf::st_polygon(list(rbind(c(135, -60), c(290, -60), c(290, 0), c(135, 0), c(135, -60))))
  sr2 <- sf::st_polygon(list(rbind(c(180, 0), c(290, 0), c(290, 60), c(180, 60), c(180, 0))))
  spP <- sf::st_multipolygon(list(sr1, sr2))
  test <- sf::st_as_sf(as(spP, "Spatial"))
  sf::st_crs(test) <- as.character("wgs84")
  expect_identical(sf::st_crs(convertLoi(test))$input, "EPSG:4326")
})


test_that("Test computeScaleIndice function", {
  expect_equal(computeScaleIndice(49, sshs), 3)
  expect_equal(computeScaleIndice(10, sshs), -1)
  expect_equal(computeScaleIndice(100, sshs), 5)
})




test_that("Test makeBuffer function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(makeBuffer("Vanuatu", pam@spatialLoi, 300 * km), pam@spatialLoiBuffer)
})





test_that("Test retrieveStorms function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(retrieveStorms(sds@database, "PAM", c(2015, 2021), TRUE), as.integer(1))
})





test_that("Test writeStorm function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  expect_identical(
    writeStorm(list(), list(), sds, 1, getBuffer(pam)),
    list(list(getStorm(pam, "PAM")), list("PAM"))
  )
})
