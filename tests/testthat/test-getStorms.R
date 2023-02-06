


test_that("Tests invalid inputs", {


  #Checking `basin`  validity
  expect_error(getStorms(basin = "BA"))
  expect_error(getStorms(basin = c("SA","SP")))

  #Checking `seasons`  validity
  expect_error(getStorms(seasons = "hui"))
  expect_error(getStorms(seasons = 2015.6, name = "PAM"))
  expect_error(getStorms(seasons = c(1850,2020)))
  expect_error(getStorms(seasons = c(2000,2030)))
  expect_error(getStorms(seasons =  c(2000,2005,2020)))

  #Checking `name` validity
  expect_error(getStorms(seasons = 2010, name = 1))

  #Checking `seasons` and `name` matching
  expect_error(getStorms(seasons = c(2016,2015), name = "WINSTON"))

  #Checking `loi` validity
  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = "hui"))

  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = 165))

  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c("hj","jk")))

  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(380, 100)))

  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = "Nouvelle Caledonie"))

  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c("Vanuatu", "Fiji")))



  #Checking `max_dist` validity
  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17), max_dist = "hui"))
  expect_error(getStorms(seasons = c(2016,2015), name = c("WINSTON","PAM"),
                         loi = c(165,-17), max_dist = c(1,3)))




  #Checking access to data
  expect_error(getStorms(seasons = 2015, name = "PM"),
               "Storm not found")
  expect_error(getStorms(seasons = 2017, name = "PAM"),
                "Storm not found")



})
