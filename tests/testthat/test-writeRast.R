



test_that("Tests checkInputsWr function", {

  msw <- stormBehaviour_sp(pam, verbose = 0)

  msw <- terra::rast(system.file("extdata", "PAM_MSW.tiff", package = "StormR"))

  #Checking rast validity
  expect_error(writeRast(format = ".tiff",
                         filename = NULL,
                         path = "./"))

  #Checking format validity
  expect_error(writeRast(rast = msw,
                         format = 1,
                         filename = NULL,
                         path = "./"))

  expect_error(writeRast(rast = msw,
                         format = ".pdf",
                         filename = NULL,
                         path = "./"))

  expect_error(writeRast(rast = msw,
                         format = c(".tiff",".nc"),
                         filename = NULL,
                         path = "./"))

  #Checking filename validity
  expect_error(writeRast(rast = msw,
                         format = ".tiff",
                         filename = TRUE,
                         path = "./"))

  expect_error(writeRast(rast = msw,
                         format = ".tiff",
                         filename = 2,
                         path = "./"))

  #Checking path validity
  expect_error(writeRast(rast = msw,
                         format = ".tiff",
                         filename = NULL,
                         path = TRUE))

  expect_error(writeRast(rast = msw,
                         format = ".tiff",
                         filename = NULL,
                         path = 2))

})
