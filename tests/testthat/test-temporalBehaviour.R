



test_that("Test checkInputsTemporalBehaviour function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))

  pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)
  
  # Checking sts input
  expect_error(checkInputsTemporalBehaviour(
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  # Checking points
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = c(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = c(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sds = pam,
    points = data.frame(lon = 169, lat = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sds = pam,
    points = data.frame(x = 400, y = 200),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sds = pam,
    points = data.frame(x = -120, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  # Checking product input
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = 1,
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "MSW",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = c("TS", "PDI", "Exposure"),
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  # Check windThreshold input
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "Exposure",
    windThreshold = c("18", "33", "42", "49", "58", "70"),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "Exposure",
    windThreshold = c(-3, -45),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  # Checking method input
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = 1,
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = TRUE,
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = c("Willoughby", "Holland"),
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  # Checking asymmetry input
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = 1,
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = TRUE,
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = c("Chen", "None"),
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 1
  ))
  
  # Checking empiricalRMW inputs
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = "TRUE",
    tempRes = 60,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = 1,
    tempRes = 60
  ))
  
  # Checking tempRes input
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = TRUE,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 2.3,
    verbose = 1
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = c(0.25, 0.5),
    verbose = 1
  ))
  
  # Checking verbose
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = TRUE
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = "1"
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = c(1, 2)
  ))
  
  expect_error(checkInputsTemporalBehaviour(
    sts = pam,
    points = data.frame(x = 169, y = -19),
    product = "TS",
    windThreshold = c(18, 33, 42, 49, 58, 70),
    method = "Willoughby",
    asymmetry = "Chen",
    empiricalRMW = FALSE,
    tempRes = 60,
    verbose = 3
  ))
})





test_that("Test computePDI function", {
  expect_equal(computePDI(seq(0, 80), 1), 20995.2)
  expect_equal(computePDI(seq(0, 80, 0.75), 0.75), 20351.403)
  expect_equal(computePDI(seq(0, 80, 0.5), 0.5), 20736.8)
  expect_equal(computePDI(seq(0, 80, 0.25), 0.25), 20608.2)
})





test_that("Test computeExposure function", {
  expect_equal(computeExposure(seq(0, 80, 1), 1, c(30, 50)), c(51, 31))
  expect_equal(computeExposure(seq(0, 80, 0.5), 0.5, c(30, 50)), c(50.5, 30.5))
  expect_equal(computeExposure(seq(0, 80, 0.75), 0.75, c(30, 50)), c(50.25, 30))
  expect_equal(computeExposure(seq(0, 80, 0.25), 0.25, c(30, 50)), c(50.25, 30.25))
})
