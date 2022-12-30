


test_that("Tests invalid inputs", {

  #Checking `sts` validity
  expect_error(plotStorms())

  #Checking names validity
  expect_error(plotStorms(sts_wp, names = 2))
  expect_error(plotStorms(sts_wp, names = "JULIA"))

  #Checking category validity
  expect_error(plotStorms(sts_wp, category = c(1,6)))
  expect_error(plotStorms(sts_wp, category = c(-10,5)))
  expect_error(plotStorms(sts_wp, category = 2.5))
  expect_error(plotStorms(sts_wp, category = "1"))

  #Checking grtc validity
  expect_error(plotStorms(sts_wp, grtc = "2"))
  expect_error(plotStorms(sts_wp, grtc = 3.6))
  expect_error(plotStorms(sts_wp, grtc = c(2,4)))

  #Checking xlim/ylim validity
  expect_error(plotStorms(sts_wp, xlim = "132"))
  expect_error(plotStorms(sts_wp, xlim = 160))
  expect_error(plotStorms(sts_wp, xlim = c(400,500)))

  expect_error(plotStorms(sts_wp, ylim = "-50"))
  expect_error(plotStorms(sts_wp, ylim = 60))
  expect_error(plotStorms(sts_wp, ylim = c(-100,100)))

  #Checking logical inputs validity
  expect_error(plotStorms(sts_wp, all_basin = "TRUE"))
  expect_error(plotStorms(sts_wp, all_basin = 1))
  expect_error(plotStorms(sts_wp, legends = "TRUE"))
  expect_error(plotStorms(sts_wp, legends = 1))
  expect_error(plotStorms(sts_wp, loi = "TRUE"))
  expect_error(plotStorms(sts_wp, loi = 1))
  expect_error(plotStorms(sts_wp, labels = "TRUE"))
  expect_error(plotStorms(sts_wp, labels = 1))

  #Checking by validity
  expect_error(plotStorms(sts_wp, by = T))
  expect_error(plotStorms(sts_wp, by = "hu"))
  expect_error(plotStorms(sts_wp, by = 5.6))
  expect_error(plotStorms(sts_wp, by = c(1,2)))

  #Checking pos validity
  expect_error(plotStorms(sts_wp, pos = T))
  expect_error(plotStorms(sts_wp, pos = "hu"))
  expect_error(plotStorms(sts_wp, pos = 5.6))
  expect_error(plotStorms(sts_wp, pos = -2))
  expect_error(plotStorms(sts_wp, pos = 5))
  expect_error(plotStorms(sts_wp, pos = c(1,2)))

})
