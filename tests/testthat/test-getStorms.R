


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

  #Checking `loi` validity
  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = "hui"),
               "invalid entry for loi")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = 165),
               "invalid class for loi")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c("hj","jk")),
               "loi must be length one ")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(380, 100)),
               "loi must have valid lon/lat coordinates ")


  #Checking `max_dist` validity
  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17), max_dist = "hui"),
               "max_dist must be numeric ")

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17), max_dist = c(1,3)),
               "max_dist must be a length 1 vector ")




  #Checking access to data
  # expect_error(getStorms(time_period = 2017, name = "PAM"),
  #               "Storm not found")



})
