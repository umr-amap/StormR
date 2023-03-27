
library(ncdf4)
msw <- spatialBehaviour(pam, verbose = 0)

test_that("Tests checkInputsWr function", {

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

# Test case 1: Test that the function correctly writes a tiff file
test_that("Function correctly writes a tiff file", {
  writeRast(msw)
  expect_true(file.exists("./PAM_MSW.tiff"))
  file.remove("./PAM_MSW.tiff")
})

# Test case 2: Test that the function correctly writes a netCDF file for MSW product
test_that("Function correctly writes a netCDF file for MSW product", {
  # Call the function with netCDF format and MSW product
  writeRast(msw, format = ".nc")
  # Check that the file was created
  expect_true(file.exists("./PAM_MSW.nc"))
  # Check the longname attribute
  nc <- nc_open("./PAM_MSW.nc")
  longname_attr <- ncatt_get(nc, "MSW", "long_name")
  expect_equal(longname_attr$value, "maximum sustained wind (m/s)")
  nc_close(nc)
  # Remove the file after the test is done
  file.remove("./PAM_MSW.nc")
})

# Test case 3: Test that the function correctly writes a netCDF file for PDI product
test_that("Function correctly writes a netCDF file for PDI product", {
  pdi <- spatialBehaviour(pam, product = "PDI", verbose = 0)
  # Call the function with netCDF format and PDI product
  writeRast(pdi, format = ".nc")
  # Check that the file was created
  expect_true(file.exists("./PAM_PDI.nc"))
  # Check the longname attribute
  nc <- nc_open("./PAM_PDI.nc")
  longname_attr <- ncatt_get(nc, "PDI", "long_name")
  expect_equal(longname_attr$value, "power dissipation index")
  nc_close(nc)
  # Remove the file after the test is done
  file.remove("./PAM_PDI.nc")
})

# Test case 4: Test that the function correctly writes a netCDF file for Exposure product
test_that("Function correctly writes a netCDF file for Exposure product", {
  exposure <- spatialBehaviour(pam, product = "Exposure", verbose = 0)
  # Call the function with netCDF format and Exposure product
  writeRast(exposure, format = ".nc", filename = "exposure")
  # Check that the file was created
  expect_true(file.exists("./exposure.nc"))
  # Check the longname attribute
  nc <- nc_open("./exposure.nc")
  longname_attr <- ncatt_get(nc, "Exposure", "long_name")
  expect_equal(longname_attr$value, "Wind threshold exposure")
  nc_close(nc)
  # Remove the file after the test is done
  file.remove("./exposure.nc")
})

# Test case 5: Test that the function correctly writes a netCDF file for profile product
test_that("Function correctly writes a netCDF file for profile product", {
  profiles <- spatialBehaviour(pam, product = "Profiles", verbose = 0)
  # Call the function with netCDF format and profile product
  writeRast(profiles, format = ".nc", filename = "profile")
  # Check that the file was created
  expect_true(file.exists("./profile.nc"))
  # Check the longname attribute
  nc <- nc_open("./profile.nc")
  longname_attr <- ncatt_get(nc, "Speed", "long_name")
  expect_equal(longname_attr$value, "radial wind speed")
  nc_close(nc)
  # Remove
  file.remove("./profile.nc")
})