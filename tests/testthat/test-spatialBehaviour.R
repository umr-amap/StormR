<<<<<<< 123-wind-direction-flipped
test_that("Test spatialBehaviour function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))

  pam <- defStormsList(
    sds = sds,
    loi = "Vanuatu",
    names = "PAM",
    verbose = 0
=======
test_that("Test getRmw function", {
  expect_equal(
    getRmw(seq(0, 80, 5), -15),
    c(
      60, 55, 51, 47, 44, 41, 38, 35, 32,
      30, 28, 25, 24, 22, 20, 19, 17
    )
  )
})





test_that("Test Willougbhy / Holland / Boose functions", {
  expect_equal(
    willoughby(seq(1, 100), 20, 50, -15),
    c(
      1.813,
      3.905,
      6.118,
      8.414,
      10.772,
      13.182,
      15.635,
      18.127,
      20.652,
      23.208,
      25.791,
      28.399,
      31.032,
      33.685,
      36.360,
      39.053,
      41.765,
      44.494,
      47.239,
      50.000,
      49.327,
      48.676,
      48.044,
      47.433,
      46.839,
      46.264,
      45.706,
      45.165,
      44.640,
      44.131,
      43.636,
      43.155,
      42.688,
      42.235,
      41.794,
      41.366,
      40.949,
      40.543,
      40.149,
      39.765,
      39.391,
      39.027,
      38.672,
      38.327,
      37.990,
      37.661,
      37.341,
      37.028,
      36.723,
      36.425,
      36.134,
      35.850,
      35.572,
      35.300,
      35.035,
      34.775,
      34.521,
      34.272,
      34.028,
      33.789,
      33.555,
      33.326,
      33.102,
      32.881,
      32.665,
      32.453,
      32.245,
      32.040,
      31.839,
      31.642,
      31.448,
      31.257,
      31.070,
      30.885,
      30.704,
      30.525,
      30.349,
      30.176,
      30.005,
      29.837,
      29.671,
      29.508,
      29.347,
      29.188,
      29.031,
      28.876,
      28.724,
      28.573,
      28.424,
      28.277,
      28.131,
      27.988,
      27.846,
      27.706,
      27.567,
      27.430,
      27.294,
      27.159,
      27.026,
      26.895
    )
>>>>>>> main
  )

  sb <- spatialBehaviour(pam, method = "Holland", verbose = 0)

  expect_equal(
    terra::minmax(sb[["PAM_MSW"]]),
    matrix(c(6.84600, 76.7339339),
      dimnames = list(c("min", "max"), c("PAM_MSW"))
    )
  )

  sb <- spatialBehaviour(pam, method = "Boose", verbose = 0)
  expect_equal(
    terra::minmax(sb[["PAM_MSW"]]),
    matrix(c(8.08600, 73.8047273),
      dimnames = list(c("min", "max"), c("PAM_MSW"))
    )
  )
})

<<<<<<< 123-wind-direction-flipped
=======
test_that("Test spatialBehaviour function", {
  sb <- spatialBehaviour(pam, method = "Holland", verbose = 0)

  expect_equal(
    terra::minmax(sb[["PAM_MSW"]]),
    matrix(c(6.84600, 76.7339339),
      dimnames = list(c("min", "max"), c("PAM_MSW"))
    )
  )

  sb <- spatialBehaviour(pam, method = "Boose", verbose = 0)
  expect_equal(
    terra::minmax(sb[["PAM_MSW"]]),
    matrix(c(8.08600, 73.8047273),
      dimnames = list(c("min", "max"), c("PAM_MSW"))
    )
  )
})

>>>>>>> main


test_that("Test checkInputsSpatialBehaviour function", {
  # Checking sts input
  expect_error(checkInputsSpatialBehaviour(
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  # Checking product input
  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = 1,
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  # Check windThreshold input
  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c("18", "33", "42", "49", "58", "70"),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(-3, -45),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  # Checking method input
  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = 1,
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = TRUE,
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = c("Willoughby", "Holland"),
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  # Checking asymmetry input
  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = 1,
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = TRUE,
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = c("Chen", "None"),
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  # Checking empiricalRMW inputs
  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = "TRUE",
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = 1,
    spaceRes = "2.5min",
    tempRes = 60,
    verbose = 2
  ))

  # Checking tempRes input
  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = TRUE,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = 2.3,
    verbose = 2
  ))

  expect_error(checkInputsSpatialBehaviour(
    sts = pam,
    product = c("MSW", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    spaceRes = "2.5min",
    tempRes = c(0.25, 0.5),
    verbose = 2
  ))
})






test_that("Test getIndices function", {
  expect_equal(getIndices(pam@data[["PAM"]], 2, "MSW"), seq(26, 49))
  expect_equal(getIndices(pam@data[["PAM"]], 20, "MSW"), seq(8, 57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "MSW"), seq(1, 57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "Profiles"), seq(28, 47))
})



<<<<<<< 123-wind-direction-flipped
=======
test_that("Test stormDisplacement function", {
  # Displacement of 1deg in longitude in one hour.
  expect_equal(
    stormDisplacement(c(0, 1), c(0, 0), 60),
    list(stormSpeed = c(30.92208, 30.92208078), vxDeg = c(1, 1), vyDeg = c(0, 0))
  )
  # Displacement of 1deg in latitude in one hour.
  expect_equal(
    stormDisplacement(c(0, 0), c(1, 0), 60),
    list(stormSpeed = c(30.7151079, 30.7151079), vxDeg = c(0, 0), vyDeg = c(-1, -1))
  )
  # Displacement of 1deg in lat + lon in 3h.
  expect_equal(
    stormDisplacement(c(1, 0), c(1, 0), 180),
    list(stormSpeed = c(14.5277378, 14.5277378), vxDeg = c(-0.33333333, -0.33333333), vyDeg = c(-0.33333333, -0.33333333))
  )
})

>>>>>>> main


test_that("Test getDataInterpolate function", {
  expect_equal(getDataInterpolate(pam@data[["PAM"]], seq(26, 49), 60, FALSE, "Willoughby"), dfGetDataInterpolate)
<<<<<<< 123-wind-direction-flipped
=======
})





test_that("Test computeDirection function", {
  expect_equal(computeDirection(1, 1, 30), 315)
  expect_equal(computeDirection(1, -1, 30), 45)
  expect_equal(computeDirection(-1, -1, 30), 135)
  expect_equal(computeDirection(-1, 1, 30), 225)

  expect_equal(computeDirection(0, 1, 30), 270)
  expect_equal(computeDirection(1, 0, 30), 0)
  expect_equal(computeDirection(0, -1, 30), 90)
  expect_equal(computeDirection(-1, 0, 30), 180)

  expect_equal(computeDirection(1, 1, -30), 135)
  expect_equal(computeDirection(1, -1, -30), 225)
  expect_equal(computeDirection(-1, -1, -30), 315)
  expect_equal(computeDirection(-1, 1, -30), 45)

  expect_equal(computeDirection(0, 1, -30), 90)
  expect_equal(computeDirection(1, 0, -30), 180)
  expect_equal(computeDirection(0, -1, -30), 270)
  expect_equal(computeDirection(-1, 0, -30), 360)
})


test_that("Test computeDirectionBoose function", {
  expect_equal(computeDirectionBoose(1, 1, 30, 1), 275)
  expect_equal(computeDirectionBoose(1, 1, 30, 0), 295)
  expect_equal(computeDirectionBoose(1, -1, 30, 1), 5)
  expect_equal(computeDirectionBoose(1, -1, 30, 0), 25)
  expect_equal(computeDirectionBoose(-1, 1, 30, 1), 185)
  expect_equal(computeDirectionBoose(-1, 1, 30, 0), 205)

  expect_equal(computeDirectionBoose(1, 1, -30, 1), 175)
  expect_equal(computeDirectionBoose(1, 1, -30, 0), 155)
  expect_equal(computeDirectionBoose(-1, -1, -30, 1), 355)
  expect_equal(computeDirectionBoose(-1, -1, -30, 0), 335)
>>>>>>> main
})
