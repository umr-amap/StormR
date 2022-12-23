


test_that("Tests invalid inputs", {

  sts = getStorms(time_period = c(2000,2021))

  #Checking `sts` validity
  expect_error(plotStorms())

  #Checking names validity
  expect_error(plotStorms(sts, names = 2))
  expect_error(plotStorms(sts, names = "JULIA"))

  #Checking category validity
  expect_error(plotStorms(sts, category = c(1,6)))
  expect_error(plotStorms(sts, category = c(-10,5)))
  expect_error(plotStorms(sts, category = 2.5))
  expect_error(plotStorms(sts, category = "1"))

  #Checking grtc validity
  expect_error(plotStorms(sts, grtc = "2"))
  expect_error(plotStorms(sts, grtc = 3.6))
  expect_error(plotStorms(sts, grtc = c(2,4)))

  #Checking xlim/ylim validity
  expect_error(plotStorms(sts, xlim = "132"))
  expect_error(plotStorms(sts, xlim = 160))
  expect_error(plotStorms(sts, xlim = c(400,500)))

  expect_error(plotStorms(sts, ylim = "-50"))
  expect_error(plotStorms(sts, ylim = 60))
  expect_error(plotStorms(sts, ylim = c(-100,100)))

  #Checking logical inputs validity
  expect_error(plotStorms(sts, all_basin = "TRUE"))
  expect_error(plotStorms(sts, all_basin = 1))
  expect_error(plotStorms(sts, legends = "TRUE"))
  expect_error(plotStorms(sts, legends = 1))
  expect_error(plotStorms(sts, loi = "TRUE"))
  expect_error(plotStorms(sts, loi = 1))
  expect_error(plotStorms(sts, labels = "TRUE"))
  expect_error(plotStorms(sts, labels = 1))


  #Checking by validity
  expect_error(plotStorms(sts, by = T))
  expect_error(plotStorms(sts, by = "hu"))
  expect_error(plotStorms(sts, by = 5.6))
  expect_error(plotStorms(sts, by = c(1,2)))


  #Checking pos validity
  expect_error(plotStorms(sts, pos = T))
  expect_error(plotStorms(sts, pos = "hu"))
  expect_error(plotStorms(sts, pos = 5.6))
  expect_error(plotStorms(sts, pos = -2))
  expect_error(plotStorms(sts, pos = 5))
  expect_error(plotStorms(sts, pos = c(1,2)))





})
