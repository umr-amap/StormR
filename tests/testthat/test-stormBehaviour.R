




test_that("Tests invalid inputs", {

  sts = getStorms(time_period = 2015, name = "PAM", loi = "Vanuatu")

  #Checking `sts` validity
  expect_error(stormBehaviour())

  #Checking product validity
  expect_error(stormBehaviour(sts, product = "WIND"))
  expect_error(stormBehaviour(sts, product = 2))
  expect_error(stormBehaviour(sts, product = T))
  expect_error(stormBehaviour(sts, product = c("MSW", "PDI")))


  #Checking method validity
  expect_error(stormBehaviour(sts, method = "willi"))
  expect_error(stormBehaviour(sts, method = 2))
  expect_error(stormBehaviour(sts, method = T))
  expect_error(stormBehaviour(sts, method = c("Willoughby", "Holland80")))


  #Checking asymmetry validity
  expect_error(stormBehaviour(sts, asymmetry = "V3"))
  expect_error(stormBehaviour(sts, asymmetry = 2))
  expect_error(stormBehaviour(sts, asymmetry = T))
  expect_error(stormBehaviour(sts, asymmetry = c("V1", "V2")))


  #Checking result validity
  res = data.frame(longitude = c(160,162), latitude = c(160,162))
  res2 = data.frame(longitude = c(380,-180), latitude = c(100,-100))
  m = cbind(c(160,162),c(160,162))
  expect_error(stormBehaviour(sts, result = "2Dstruct"))
  expect_error(stormBehaviour(sts, result = T))
  expect_error(stormBehaviour(sts, result = 2))
  expect_error(stormBehaviour(sts, result = c("profiles","analytic")))
  expect_error(stormBehaviour(sts, result = res))
  expect_error(stormBehaviour(sts, result = res2))
  expect_error(stormBehaviour(sts, result = m))



  #Checking space_res validity
  expect_error(stormBehaviour(sts, space_res  = "TRUE"))
  expect_error(stormBehaviour(sts, space_res  = T))
  expect_error(stormBehaviour(sts, space_res  = 2.3))
  expect_error(stormBehaviour(sts, space_res  = c(2,3)))


  #Checking time_res validity
  expect_error(stormBehaviour(sts, time_res  = "TRUE"))
  expect_error(stormBehaviour(sts, time_res  = T))
  expect_error(stormBehaviour(sts, time_res  = 2.3))
  expect_error(stormBehaviour(sts, time_res  = c(2,3)))


  #Checking logical inputs validity
  expect_error(stormBehaviour(sts, empirical_rmw = "TRUE"))
  expect_error(stormBehaviour(sts, empirical_rmw = 1))
  expect_error(stormBehaviour(sts, verbose = "TRUE"))
  expect_error(stormBehaviour(sts, verbose = 1))
  expect_error(stormBehaviour(sts, focus_loi = "TRUE"))
  expect_error(stormBehaviour(sts, focus_loi = 1))



})
