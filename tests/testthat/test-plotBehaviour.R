



test_that("Tests checkInputsPb function", {

  msw <- spatialBehaviour(pam, verbose = 0)

  #Checking sts input
  expect_error(checkInputsPb(raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  #Checking raster_product input
  expect_error(checkInputsPb(sts = pam,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  #Checking color_palette input
  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = c(1, 2, 3),
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  #Checking xlim/ylim input
  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = "132",
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = 160,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = c(400, 500),
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = "-50",
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = 60,
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = c(-100, 100),
                             labels = FALSE,
                             by = 8,
                             pos = 3))

  #Checking labels input
  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = "TRUE",
                             by = 8,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = 1,
                             by = 8,
                             pos = 3))

  #Checking by input
  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = TRUE,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = "hui",
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 5.6,
                             pos = 3))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = c(1, 2),
                             pos = 3))

  #Checking pos input
  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = TRUE))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = "hui"))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 5.6))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = -2))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = 5))

  expect_error(checkInputsPb(sts = pam,
                             raster_product = msw,
                             color_palette = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             labels = FALSE,
                             by = 8,
                             pos = c(1, 2)))

})
