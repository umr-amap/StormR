


test_that("Tests invalid inputs", {

  #Checking `time_period`  validity
  expect_error(getStorms(time_period = "hui"),
               "time_period must be numeric")

  expect_error(getStorms(time_period = 2015.6, name = "PAM"),
               "time_period must be as integers")

  expect_error(getStorms(time_period = c(1850,2020)),
               "lower bound of time range is not valid")

  expect_error(getStorms(time_period = c(2000,2030)),
               "upper bound of time range is not valid")


  expect_error(getStorms(time_period =  c(2000,2005,2020)),
               "time_period must be either length 1 or 2")

  #Checking `name` validity
  expect_error(getStorms(time_period = 2010, name = 1),
               "name must be a vector of characters")


  #Checking `time_period` and `name` matching
  expect_error(getStorms(time_period = c(2016,2015), name = "WINSTON"),
               "name and time_period must be the same length")

  #Checking `aoi` and `max_dist` validity
  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         aoi = "hui"),
               "aoi must be a vector of numeric ")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         aoi = 165),
               "aoi must be a length 2 vector")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         aoi = c(165,-17), max_dist = "hui"),
               "max_dist must be numeric ")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         aoi = c(165,-17), max_dist = c(1,3)),
               "max_dist must be a length 1 vector ")

  #Checking `loi` validity

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17)),
               "loi must be a matrix, array or SpatialPolygons")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = "NA"), #NA for North Atlantic Basin e.g
               "loi must be focused on South Pacific Basin `SP`")


  #Checking access to data
  # expect_error(getStorms(time_period = 2017, name = "PAM"),
  #              "Storm not found")



})
