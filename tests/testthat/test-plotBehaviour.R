



test_that("Tests checkInputsPlotBehaviour function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)
  msw <- spatialBehaviour(pam, verbose = 0)

  # Checking sts input
  expect_error(checkInputsPlotBehaviour(
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  # Checking raster_product input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  # Checking color_palette input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = TRUE,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = c(1, 2, 3),
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  # Checking xlim/ylim input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = "132",
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = 160,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = c(400, 500),
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = "-50",
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = 60,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = c(-100, 100),
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  # Checking labels input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = "TRUE",
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = 1,
    by = 8,
    pos = 3
  ))

  # Checking by input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = TRUE,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = "hui",
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 5.6,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = c(1, 2),
    pos = 3
  ))

  # Checking pos input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = TRUE
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = "hui"
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 5.6
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = -2
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 5
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = c(1, 2)
  ))

  # Checking main input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = TRUE,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = 3,
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = c("pam", "cyclone"),
    legends = "topleft",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  # Checking legends input
  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = 1,
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = TRUE,
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = c("topleft", "topright"),
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))

  expect_error(checkInputsPlotBehaviour(
    sts = pam,
    raster_product = msw,
    color_palette = NULL,
    main = NULL,
    legends = "top",
    xlim = NULL,
    ylim = NULL,
    labels = FALSE,
    by = 8,
    pos = 3
  ))
})
