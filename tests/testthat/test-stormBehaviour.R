




test_that("Tests invalid inputs", {


  #Checking `sts` validity
  expect_error(stormBehaviour_sp())

  #Checking product validity
  expect_error(stormBehaviour_sp(pam, product = "WIND"))
  expect_error(stormBehaviour_sp(pam, product = 2))
  expect_error(stormBehaviour_sp(pam, product = T))

  #Checking method validity
  expect_error(stormBehaviour_sp(pam, method = "willi"))
  expect_error(stormBehaviour_sp(pam, method = 2))
  expect_error(stormBehaviour_sp(pam, method = T))
  expect_error(stormBehaviour_sp(pam, method = c("Willoughby", "Holland")))


  #Checking asymmetry validity
  expect_error(stormBehaviour_sp(pam, asymmetry = "V3"))
  expect_error(stormBehaviour_sp(pam, asymmetry = 2))
  expect_error(stormBehaviour_sp(pam, asymmetry = T))
  expect_error(stormBehaviour_sp(pam, asymmetry = c("V1", "V2")))


  #Checking format validity
  m <- cbind(c(160,162),c(160,162))
  expect_error(stormBehaviour_sp(pam, format = "2Dstruct"))
  expect_error(stormBehaviour_sp(pam, format = T))
  expect_error(stormBehaviour_sp(pam, format = 2))
  expect_error(stormBehaviour_sp(pam, format = c("profiles","analytic")))
  expect_error(stormBehaviour_sp(pam, format = m))



  #Checking space_res validity
  expect_error(stormBehaviour_sp(pam, space_res  = "TRUE"))
  expect_error(stormBehaviour_sp(pam, space_res  = T))
  expect_error(stormBehaviour_sp(pam, space_res  = c(2,3)))


  #Checking time_res validity
  expect_error(stormBehaviour_sp(pam, time_res  = "TRUE"))
  expect_error(stormBehaviour_sp(pam, time_res  = T))
  expect_error(stormBehaviour_sp(pam, time_res  = 2.3))
  expect_error(stormBehaviour_sp(pam, time_res  = c(2,3)))


  #Checking logical inputs validity
  expect_error(stormBehaviour_sp(pam, empirical_rmw = "TRUE"))
  expect_error(stormBehaviour_sp(pam, empirical_rmw = 1))


})


test_that("get indices", {

  expect_equal(getIndices(pam@data[["PAM"]], 2, "analytic"), seq(26,49))
  expect_equal(getIndices(pam@data[["PAM"]], 20, "analytic"), seq(8,57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "analytic"), seq(1,57))

})



