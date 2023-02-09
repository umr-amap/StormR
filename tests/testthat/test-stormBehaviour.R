




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
  expect_error(stormBehaviour(pam, method = c("Willoughby", "Holland")))


  #Checking asymmetry validity
  expect_error(stormBehaviour(pam, asymmetry = "V3"))
  expect_error(stormBehaviour(pam, asymmetry = 2))
  expect_error(stormBehaviour(pam, asymmetry = T))
  expect_error(stormBehaviour(pam, asymmetry = c("V1", "V2")))


  #Checking format validity
  m <- cbind(c(160,162),c(160,162))
  expect_error(stormBehaviour(pam, format = "2Dstruct"))
  expect_error(stormBehaviour(pam, format = T))
  expect_error(stormBehaviour(pam, format = 2))
  expect_error(stormBehaviour(pam, format = c("profiles","analytic")))
  expect_error(stormBehaviour(pam, format = m))



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



