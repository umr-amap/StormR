suppressWarnings(sds <- defStormsDataset(verbose = 0))
pam <- defStormsList(sds, loi = "Vanuatu", names = "PAM", verbose = 0)
msw <- spatialBehaviour(pam, verbose = 0)

test_that("Tests checkInputsWriteRastiteRast function", {
  # Checking rast validity
  expect_error(writeRast(
    filename = NULL,
    path = "./"
  ))

  # Checking filename validity
  expect_error(writeRast(
    rast = msw,
    filename = TRUE,
    path = "./"
  ))

  expect_error(writeRast(
    rast = msw,
    filename = 2,
    path = "./"
  ))

  expect_error(writeRast(
    rast = msw,
    filename = "filename.pdf",
    path = "./"
  ))

  expect_error(writeRast(
    rast = msw,
    filename = "filename",
    path = "./"
  ))

  # Checking path validity
  expect_error(writeRast(
    rast = msw,
    filename = NULL,
    path = TRUE
  ))

  expect_error(writeRast(
    rast = msw,
    filename = NULL,
    path = 2
  ))
})

# Test case 1: Test that the function correctly writes a tiff file
test_that("Function correctly writes a tiff file", {
  writeRast(msw)
  expect_true(file.exists("./PAM_MSW.tiff"))
  # Check that default is overwrite = FALSE)
  expect_error(writeRast(msw))
  # Check that setting overwrite = TRUE works
  writeRast(msw, overwrite = TRUE)
  # Check that additional arguments are compatible with terra::writeRaster()
  writeRast(msw, overwrite = TRUE, verbose = TRUE, scale = 10, steps = 2)
  file.remove("./PAM_MSW.tiff")
})

# Test case 2: Test that the function correctly writes a netCDF file for MSW product
test_that("Function correctly writes a netCDF file for MSW product", {
  # Call the function with netCDF format and MSW product
  writeRast(msw, filename = "PAM_MSW.nc", path = "./")
  # Check that the file was created
  expect_true(file.exists("./PAM_MSW.nc"))
  # Check the longname attribute
  nc <- ncdf4::nc_open("./PAM_MSW.nc")
  longname_attr <- ncdf4::ncatt_get(nc, "PAM_MSW", "long_name")
  expect_equal(longname_attr$value, "maximum sustained wind")
  ncdf4::nc_close(nc)
  # Remove the file after the test is done
  file.remove("./PAM_MSW.nc")
})

# Test case 3: Test that the function correctly writes a netCDF file for PDI product
test_that("Function correctly writes a netCDF file for PDI product", {
  pdi <- spatialBehaviour(pam, sds, product = "PDI", verbose = 0)
  # Call the function with netCDF format and PDI product
  writeRast(pdi, filename = "PAM_PDI.nc", path = "./")
  # Check that the file was created
  expect_true(file.exists("./PAM_PDI.nc"))
  # Check the longname attribute
  nc <- ncdf4::nc_open("./PAM_PDI.nc")
  longname_attr <- ncdf4::ncatt_get(nc, "PAM_PDI", "long_name")
  expect_equal(longname_attr$value, "power dissipation index")
  ncdf4::nc_close(nc)
  # Remove the file after the test is done
  file.remove("./PAM_PDI.nc")
})

# Test case 4: Test that the function correctly writes a netCDF file for Exposure product
test_that("Function correctly writes a netCDF file for Exposure product", {
  exposure <- spatialBehaviour(pam, product = "Exposure", verbose = 0)
  # Call the function with netCDF format and Exposure product
  writeRast(exposure, filename = "exposure.nc", path = "./")
  # Check that the file was created
  expect_true(file.exists("./exposure.nc"))
  # Check the longname attribute
  nc <- ncdf4::nc_open("./exposure.nc")
  longname_attr <- ncdf4::ncatt_get(nc, "PAM_Exposure", "long_name")
  expect_equal(longname_attr$value, "Wind threshold exposure")
  ncdf4::nc_close(nc)
  # Remove the file after the test is done
  file.remove("./exposure.nc")
})

# Test case 5: Test that the function correctly writes a netCDF file for profile product
test_that("Function correctly writes a netCDF file for profile product", {
  profiles <- spatialBehaviour(pam, product = "Profiles", verbose = 0)
  # Call the function with netCDF format and profile product
  writeRast(profiles, filename = "profile.nc", path = "./")
  # Check that the file was created
  expect_true(file.exists("./profile.nc"))
  # Check the longname attribute
  nc <- ncdf4::nc_open("./profile.nc")
  longname_attr <- ncdf4::ncatt_get(nc, "PAM_Speed", "long_name")
  expect_equal(longname_attr$value, "radial wind speed")
  ncdf4::nc_close(nc)
  # Remove
  file.remove("./profile.nc")
})
