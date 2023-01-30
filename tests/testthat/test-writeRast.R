


test_that("Tests invalid inputs", {


  msw = terra::rast(system.file("extdata", "PAM_MSW.tiff", package = "StormR"))

  #Checking rast validity
  expect_error(writeRast())

  #Checking format validity
  expect_error(writeRast(msw,format = 1))
  expect_error(writeRast(msw,format = ".pdf"))
  expect_error(writeRast(msw,format = c(".tiff",".nc")))

  #Checking filename validity
  expect_error(writeRast(sts, filename = T))
  expect_error(writeRast(sts, filename = 2))

  #Checking path validity
  expect_error(writeRast(sts, path = T))
  expect_error(writeRast(sts, path = 2))



})
