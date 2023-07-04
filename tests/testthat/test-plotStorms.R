



test_that("Test getColors function", {
  expect_error(getColors(50))
  expect_equal(getColors(NA, sshs), NA)
  expect_equal(getColors(10, sshs), "#00CCFF")
  expect_equal(getColors(18, sshs), "#00CCCC")
  expect_equal(getColors(25, sshs), "#00CCCC")
  expect_equal(getColors(33, sshs), "#FFFFB2")
  expect_equal(getColors(36, sshs), "#FFFFB2")
  expect_equal(getColors(42, sshs), "#FECC5C")
  expect_equal(getColors(46, sshs), "#FECC5C")
  expect_equal(getColors(49, sshs), "#FD8D3C")
  expect_equal(getColors(53, sshs), "#FD8D3C")
  expect_equal(getColors(58, sshs), "#F03B20")
  expect_equal(getColors(65, sshs), "#F03B20")
  expect_equal(getColors(70, sshs), "#BD0026")
  expect_equal(getColors(100, sshs), "#BD0026")
})





test_that("Test checkInputsPlotStorms function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)

  # Checking sts input
  expect_error(checkInputsPlotStorms(
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  # Checking names input
  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = 2,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = "JULIA",
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  # Checking category input
  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = c(1, 6),
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = c(-10, 5),
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = 2.5,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = "1",
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = TRUE,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))


  # Checking xlim/ylim input
  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = "132",
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = 160,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = c(400, 500),
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = "-50"
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = 60
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = c(-100, 100)
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = "TRUE",
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = 1,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = "TRUE",
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = 1,
    by = 8,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  # Checking by input
  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = TRUE,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = "hui",
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 5.6,
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = c(1, 2),
    pos = 3,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  # Checking pos input
  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = TRUE,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = "hui",
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 5.6,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = -2,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 5,
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = c(1, 2),
    legends = "topleft",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  # Checking legends input
  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = 1,
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = TRUE,
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = c("topleft", "topright"),
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))

  expect_error(checkInputsPlotStorms(
    sts = pam,
    names = NULL,
    category = NULL,
    labels = TRUE,
    by = 8,
    pos = 3,
    legends = "top",
    loi = TRUE,
    xlim = NULL,
    ylim = NULL
  ))
})
