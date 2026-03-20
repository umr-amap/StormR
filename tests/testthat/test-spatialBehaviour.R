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


test_that("Test getDataInterpolate function", {
  expect_equal(getDataInterpolate(pam@data[["PAM"]], seq(26, 49), 60, FALSE, "Willoughby"), dfGetDataInterpolate)
})
