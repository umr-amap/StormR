tew <- computeTEW(prof, pam, mnt, product = "TEW", verbose = 0)
tew_mean <- computeTEW(prof, pam, mnt, product = "TEW1wdMean", verbose = 0)
tew_max <- computeTEW(prof, pam, mnt, product = "TEW1wdMax", verbose = 0)

test_that("Tests checkInputsplotTEW function", {

  # Checking sts input
  expect_error(
    checkInputsplotTEW(
      rasterProduct = tew_mean,
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking rasterProduct input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking colorPalette input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = TRUE,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking main input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = TRUE,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = c("a", "b"),
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking xlim input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = "132",
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = 160,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = c(400, 500),
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = "-50",
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = 60,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = c(-100, 100),
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking labels input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = "TRUE",
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking by input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = TRUE,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 5.6,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking pos input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = TRUE,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 5,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking legends input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = 1,
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "top",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking fan input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = "TRUE",
      dynamicPlot = FALSE,
      dtm = NULL
    )
  )

  # Checking dynamicPlot input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = 1,
      dtm = NULL
    )
  )

  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = c(TRUE, TRUE),
      dtm = NULL
    )
  )

  # Checking dtm input
  expect_error(
    checkInputsplotTEW(
      sts = pam,
      rasterProduct = tew_mean,
      colorPalette = NULL,
      main = NULL,
      legends = "topright",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      fan = FALSE,
      dynamicPlot = FALSE,
      dtm = TRUE
    )
  )
})

test_that("plotTEW works without errors", {
  pdf(NULL)
  # Basic call with single observation layer
  expect_no_error(plotTEW(pam, tew[[1]]))

  # Call with DTM
  expect_no_error(plotTEW(pam, tew_mean, dtm = mnt))

  # Call with summary statistic
  expect_no_error(plotTEW(pam, tew_max))

  # Call with labels
  expect_no_error(plotTEW(pam, tew_mean, labels = TRUE, by = 1, pos = 3))

  # Call with custom xlim/ylim
  expect_no_error(plotTEW(pam, tew_max,
    xlim = c(168, 169),
    ylim = c(-18, -17)
  ))

  # Call with dynamicPlot = TRUE
  expect_no_error(plotTEW(pam, tew_mean, dynamicPlot = TRUE))

  dev.off()
})

test_that("Tests checkInputsPlotTemporalTEW function", {

  # Missing data
  expect_error(checkInputsPlotTemporalTEW(storm = "PAM"))

  # Missing storm
  expect_error(checkInputsPlotTemporalTEW(data = tew))

  # Wrong data type (not a list of lists)
  expect_error(checkInputsPlotTemporalTEW(data = "not_a_list", storm = "PAM"))

  # Storm as character not found
  expect_error(checkInputsPlotTemporalTEW(data = tew, storm = "NOT_A_STORM"))

  # Storm as numeric index out of bounds
  expect_error(checkInputsPlotTemporalTEW(data = tew, storm = 99))

  # Storm as invalid type
  expect_error(checkInputsPlotTemporalTEW(data = tew, storm = TRUE))
})

test_that("plotTemporalTEW works without errors", {
  df2 <- data.frame(x = c(169.097), y = c(-18.723))
  TS2 <- temporalBehaviour(pam, points = df2, product = "TS", tempRes = 30, verbose = 0)
  tew_TS <- computeTEW(TS2, df2, mnt, angle = 6, threshold = 0, verbose = 0)

  pdf(NULL)
  expect_no_error(plotTemporalTEW(tew_TS, storm = "PAM"))
  expect_no_error(plotTemporalTEW(tew_TS, storm = 1))
  dev.off()
})

test_that("computeWindFan returns direction range", {

  fan <- computeWindFan(pam, tew)
  expect_type(fan, "list")
  expect_named(fan, c("d_min", "d_max"))
  expect_type(fan$d_min, "double")
  expect_type(fan$d_max, "double")
  expect_lte(fan$d_min, fan$d_max)
})
