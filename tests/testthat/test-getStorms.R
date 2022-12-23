


test_that("Tests invalid inputs", {


  #Checking `time_period`  validity
  expect_error(getStorms(basin = "BA"))
  expect_error(getStorms(basin = c("SA","SP")))

  #Checking `time_period`  validity
  expect_error(getStorms(time_period = "hui"))
  expect_error(getStorms(time_period = 2015.6, name = "PAM"))
  expect_error(getStorms(time_period = c(1850,2020)))
  expect_error(getStorms(time_period = c(2000,2030)))
  expect_error(getStorms(time_period =  c(2000,2005,2020)))

  #Checking `name` validity
  expect_error(getStorms(time_period = 2010, name = 1))

  #Checking `time_period` and `name` matching
  expect_error(getStorms(time_period = c(2016,2015), name = "WINSTON"))

  #Checking `loi` validity
  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = "hui"))

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = 165))

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c("hj","jk")))

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(380, 100)))

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = "Nouvelle Caledonie"))

  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c("Vanuatu", "Fiji")))



  #Checking `max_dist` validity
  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17), max_dist = "hui"))
  expect_error(getStorms(time_period = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17), max_dist = c(1,3)))




  #Checking access to data
  expect_error(getStorms(time_period = 2015, name = "PM"),
               "Storm not found")
  expect_error(getStorms(time_period = 2017, name = "PAM"),
                "Storm not found")



})
