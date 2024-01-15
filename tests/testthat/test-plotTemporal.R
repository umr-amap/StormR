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
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  st <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  TS <- temporalBehaviour(st, points = df, product = "TS", tempRes = 30, verbose = 0)
  PDI <- temporalBehaviour(st, points = df, product = "TS", tempRes = 30, verbose = 0)

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

  tmp1 <- tempfile(fileext = ".png")
  plotTemporal(data = TS, storm = "PAM")
  dev.copy(png, tmp1)
  dev.off()

  tmp2 <- tempfile(fileext = ".png")
  plot(TS$PAM$Port_Vila$speed, type = "l", ylim = c(0, 60), xlab = "",
       ylab = "Wind speed (m/s)", axes = FALSE, col = "blue")
  points(TS$PAM$Port_Vila$speed, col = "red", pch = 3)
  lines(TS$PAM$Luganville$speed, col = "red")
  points(TS$PAM$Luganville$speed, col = "red", pch = 3)
  legend("topleft", pch = 3, col = c("blue", "red"), legend = c("Port Vila", "Luganville"), bty = "n")
  axis(1, at = seq(1, length(TS$PAM$Port_Vila$speed)), labels = TS$PAM$Port_Vila$isoTimes, las = 2)
  axis(2, at = seq(0, 60, 10), labels = seq(0, 60, 10), las = 2)
  dev.copy(png, tmp2)
  dev.off()

  # TODO no visualTest package found here ??
  # expect_true(visualTest::isSimilar(tmp1,
  #                                   fingerprint = getFingerprint(tmp2),
  #                                   threshold = 0.1))

})
