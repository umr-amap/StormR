




test_that("Tests checkInputsPlotBehaviour function", {
  suppressWarnings(sds <- defStormsDataset(verbose = 0))
  pam <-
    defStormsList(sds,
                  loi = "Vanuatu",
                  names = "PAM",
                  verbose = 0)
  msw <- spatialBehaviour(pam, verbose = 0)
  
  # Checking sts input
  expect_error(
    checkInputsPlotBehaviour(
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking raster_product input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking color_palette input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = TRUE,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = c(1, 2, 3),
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking xlim/ylim input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = "132",
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = 160,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = c(400, 500),
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = "-50",
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = 60,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = c(-100, 100),
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking labels input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = "TRUE",
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = 1,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking by input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = TRUE,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = "hui",
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 5.6,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = c(1, 2),
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking pos input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = TRUE,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = "hui",
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 5.6,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = -2,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 5,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = c(1, 2),
      dynamicPLot = FALSE
    )
  )
  
  # Checking main input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = TRUE,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = 3,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = c("pam", "cyclone"),
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking legends input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = 1,
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = TRUE,
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = c("topleft", "topright"),
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "top",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPLot = FALSE
    )
  )
  
  # Checking dynamicPlot input
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPlot = 1
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPlot = "1"
    )
  )
  
  expect_error(
    checkInputsPlotBehaviour(
      sts = pam,
      raster_product = msw,
      color_palette = NULL,
      main = NULL,
      legends = "topleft",
      xlim = NULL,
      ylim = NULL,
      labels = FALSE,
      by = 8,
      pos = 3,
      dynamicPlot = c(TRUE, TRUE)
    )
  )
  
})


# TODO do not know why it is not working on remote repo
# test_that("test result dynamic plot", {
#   suppressWarnings(sds <- defStormsDataset())
#   pam <-
#     defStormsList(sds,
#                   loi = "Vanuatu",
#                   names = "PAM",
#                   verbose = 0)
#   
#   msw <- spatialBehaviour(pam, verbose = 0)
#   
#   map <- plotBehaviour(pam, msw, dynamicPlot = TRUE)
#   
#   expect_equal(map, mapPamMsw)
# })
