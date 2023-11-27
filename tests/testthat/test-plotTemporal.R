test_that("Test checkinputs function", {
  
  df <- data.frame(x = c(168.33,
                         167.17,
                         169.17),
                   y = c(-17.73,
                         -15.53,
                         -15.53))
  pointNames <- c("Port_Vila",
                  "Luganville",
                  "point3")
  
  cols <- c("blue", "red", "gray")
  
  rownames(df) <- pointNames
  sds <- defStormsDataset()
  st <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 1)
  
  TS <- temporalBehaviour(st, points = df, product = "TS", tempRes = 0.5, verbose = 0)
  PDI <- temporalBehaviour(st, points = df, product = "TS", tempRes = 0.5, verbose = 0)
  
  # Checking data input
  expect_error(checkInputsPlotTemporal(storm = "PAM"))
  expect_error(checkInputsPlotTemporal(data = PDI, storm = "PAM"))
  
  # Checking storm input
  expect_error(checkInputsPlotTemporal(data = TS))
  expect_error(checkInputsPlotTemporal(data = TS,
                                       storm = "WINSTON"))
  expect_error(checkInputsPlotTemporal(data = TS,
                                       storm = 3))
  
  # Checking var input
  expect_error(checkInputsPlotTemporal(data = TS,
                                       storm = "PAM",
                                       var = "wind"))
  
  
})
