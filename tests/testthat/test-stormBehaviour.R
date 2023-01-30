




test_that("Tests invalid inputs", {


  #Checking `sts` validity
  expect_error(stormBehaviour())

  #Checking product validity
  expect_error(stormBehaviour(pam, product = "WIND"))
  expect_error(stormBehaviour(pam, product = 2))
  expect_error(stormBehaviour(pam, product = T))
  expect_error(stormBehaviour(pam, product = c("MSW", "PDI")))


  #Checking method validity
  expect_error(stormBehaviour(pam, method = "willi"))
  expect_error(stormBehaviour(pam, method = 2))
  expect_error(stormBehaviour(pam, method = T))
  expect_error(stormBehaviour(pam, method = c("Willoughby", "Holland80")))


  #Checking asymmetry validity
  expect_error(stormBehaviour(pam, asymmetry = "V3"))
  expect_error(stormBehaviour(pam, asymmetry = 2))
  expect_error(stormBehaviour(pam, asymmetry = T))
  expect_error(stormBehaviour(pam, asymmetry = c("V1", "V2")))


  #Checking result validity
  res = data.frame(longitude = c(160,162), latitude = c(160,162))
  res2 = data.frame(longitude = c(380,-180), latitude = c(100,-100))
  m = cbind(c(160,162),c(160,162))
  expect_error(stormBehaviour(pam, result = "2Dstruct"))
  expect_error(stormBehaviour(pam, result = T))
  expect_error(stormBehaviour(pam, result = 2))
  expect_error(stormBehaviour(pam, result = c("profiles","analytic")))
  expect_error(stormBehaviour(pam, result = res))
  expect_error(stormBehaviour(pam, result = res2))
  expect_error(stormBehaviour(pam, result = m))



  #Checking space_res validity
  expect_error(stormBehaviour(pam, space_res  = "TRUE"))
  expect_error(stormBehaviour(pam, space_res  = T))
  expect_error(stormBehaviour(pam, space_res  = c(2,3)))


  #Checking time_res validity
  expect_error(stormBehaviour(pam, time_res  = "TRUE"))
  expect_error(stormBehaviour(pam, time_res  = T))
  expect_error(stormBehaviour(pam, time_res  = 2.3))
  expect_error(stormBehaviour(pam, time_res  = c(2,3)))


  #Checking logical inputs validity
  expect_error(stormBehaviour(pam, empirical_rmw = "TRUE"))
  expect_error(stormBehaviour(pam, empirical_rmw = 1))
  expect_error(stormBehaviour(pam, verbose = "TRUE"))
  expect_error(stormBehaviour(pam, verbose = 1))
  expect_error(stormBehaviour(pam, focus_loi = "TRUE"))
  expect_error(stormBehaviour(pam, focus_loi = 1))

})


test_that("get indices", {

  expect_equal(getIndices(pam@data[["PAM"]], 2, "analytic", TRUE), seq(26,49))
  expect_equal(getIndices(pam@data[["PAM"]], 20, "analytic", TRUE), seq(8,57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "analytic", TRUE), seq(1,57))
  expect_equal(getIndices(pam@data[["PAM"]], 2, "analytic", FALSE), seq(1,57))


})



