


test_that("Tests invalid inputs", {

  sts = getStorms(time_period = 2015, name = "PAM", loi = "Vanuatu")
  msw = stormBehaviour(sts)

  #Checking `sts` validity
  expect_error(plotBehaviour())

  #Checking raster_product validity
  expect_error(plotBehaviour(sts))

  #Checking xlim/ylim validity
  expect_error(plotBehaviour(sts, xlim = "132"))
  expect_error(plotBehaviour(sts, xlim = 160))
  expect_error(plotBehaviour(sts, xlim = c(400,500)))

  expect_error(plotBehaviour(sts, ylim = "-50"))
  expect_error(plotBehaviour(sts, ylim = 60))
  expect_error(plotBehaviour(sts, ylim = c(-100,100)))

  #Checking labels validity
  expect_error(plotBehaviour(sts, labels = "TRUE"))
  expect_error(plotBehaviour(sts, labels = 1))


  #Checking by validity
  expect_error(plotBehaviour(sts, by = T))
  expect_error(plotBehaviour(sts, by = "hu"))
  expect_error(plotBehaviour(sts, by = 5.6))
  expect_error(plotBehaviour(sts, by = c(1,2)))


  #Checking pos validity
  expect_error(plotBehaviour(sts, pos = T))
  expect_error(plotBehaviour(sts, pos = "hu"))
  expect_error(plotBehaviour(sts, pos = 5.6))
  expect_error(plotBehaviour(sts, pos = -2))
  expect_error(plotBehaviour(sts, pos = 5))
  expect_error(plotBehaviour(sts, pos = c(1,2)))





})
