


test_that("Tests invalid inputs", {

  #Checking sts input
  expect_error(plotStorms())

  #Checking names input
  expect_error(plotStorms(sts_wp, names = 2))
  expect_error(plotStorms(sts_wp, names = "JULIA"))

  #Checking category input
  expect_error(plotStorms(sts_wp, category = c(1,6)))
  expect_error(plotStorms(sts_wp, category = c(-10,5)))
  expect_error(plotStorms(sts_wp, category = 2.5))
  expect_error(plotStorms(sts_wp, category = "1"))

  #Checking xlim/ylim input
  expect_error(plotStorms(sts_wp, xlim = "132"))
  expect_error(plotStorms(sts_wp, xlim = 160))
  expect_error(plotStorms(sts_wp, xlim = c(400,500)))

  expect_error(plotStorms(sts_wp, ylim = "-50"))
  expect_error(plotStorms(sts_wp, ylim = 60))
  expect_error(plotStorms(sts_wp, ylim = c(-100,100)))

  #Checking logical inputs
  expect_error(plotStorms(sts_wp, legends = "TRUE"))
  expect_error(plotStorms(sts_wp, legends = 1))
  expect_error(plotStorms(sts_wp, loi = "TRUE"))
  expect_error(plotStorms(sts_wp, loi = 1))
  expect_error(plotStorms(sts_wp, labels = "TRUE"))
  expect_error(plotStorms(sts_wp, labels = 1))

  #Checking by input
  expect_error(plotStorms(sts_wp, by = T))
  expect_error(plotStorms(sts_wp, by = "hu"))
  expect_error(plotStorms(sts_wp, by = 5.6))
  expect_error(plotStorms(sts_wp, by = c(1,2)))

  #Checking pos input
  expect_error(plotStorms(sts_wp, pos = T))
  expect_error(plotStorms(sts_wp, pos = "hu"))
  expect_error(plotStorms(sts_wp, pos = 5.6))
  expect_error(plotStorms(sts_wp, pos = -2))
  expect_error(plotStorms(sts_wp, pos = 5))
  expect_error(plotStorms(sts_wp, pos = c(1,2)))

})
