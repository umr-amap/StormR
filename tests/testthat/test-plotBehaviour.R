


test_that("Tests invalid inputs", {

  msw = terra::rast(system.file("extdata", "PAM_MSW.tiff", package = "StormR"))

  #Checking `sts` validity
  expect_error(plotBehaviour())

  #Checking raster_product validity
  expect_error(plotBehaviour(pam))

  #Checking xlim/ylim validity
  expect_error(plotBehaviour(pam, msw, xlim = "132"))
  expect_error(plotBehaviour(pam, msw, xlim = 160))
  expect_error(plotBehaviour(pam, msw, xlim = c(400,500)))

  expect_error(plotBehaviour(pam, msw, ylim = "-50"))
  expect_error(plotBehaviour(pam, msw, ylim = 60))
  expect_error(plotBehaviour(pam, msw, ylim = c(-100,100)))

  #Checking labels validity
  expect_error(plotBehaviour(pam, msw, labels = "TRUE"))
  expect_error(plotBehaviour(pam, msw, labels = 1))

  #Checking by validity
  expect_error(plotBehaviour(pam, msw, by = T))
  expect_error(plotBehaviour(pam, msw, by = "hu"))
  expect_error(plotBehaviour(pam, msw, by = 5.6))
  expect_error(plotBehaviour(pam, msw, by = c(1,2)))

  #Checking pos validity
  expect_error(plotBehaviour(pam, msw, pos = T))
  expect_error(plotBehaviour(pam, msw, pos = "hu"))
  expect_error(plotBehaviour(pam, msw, pos = 5.6))
  expect_error(plotBehaviour(pam, msw, pos = -2))
  expect_error(plotBehaviour(pam, msw, pos = 5))
  expect_error(plotBehaviour(pam, msw, pos = c(1,2)))

  #Checking color_palette validity
  expect_error(plotBehaviour(pam, msw, color_palette = T))
  expect_error(plotBehaviour(pam, msw, color_palette = c(1,2,3)))


})
