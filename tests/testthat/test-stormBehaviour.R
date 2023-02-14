




test_that("Tests invalid inputs", {



  #Checking sts input
  expect_error(checkInputsSb(product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(40, 50),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  #Checking product input
  expect_error(checkInputsSb(sts = pam,
                             product = 1,
                             wind_threshold = c(40, 50),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = "TS",
                             wind_threshold = c(40, 50),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  #Check wind_threshold input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  #Checking method input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(40, 50),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = 1,
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = TRUE,
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = c("Willougbhy", "Holland"),
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  #Checking asymmetry input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = 1,
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = TRUE,
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = c("Boose01", "None"),
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  #Checking empirical_rmw inputs
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = "TRUE",
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = 1,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))


  #Checking format input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "2Dstruct",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = TRUE,
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = 2,
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = TRUE,
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = c("profiles","analytic"),
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = 2))

  #Checking space_res input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = 2.5,
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = TRUE,
                             time_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = c("30sec", "2.5min", "5min", "10min"),
                             time_res = 1,
                             verbose = 2))


  #Checking time_res input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = TRUE,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 2.3,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = c(0.25, 0.5),
                             verbose = 2))

  #Checking verbose input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = -1))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = TRUE))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             method = "Willougbhy",
                             asymmetry = "Boose01",
                             empirical_rmw = FALSE,
                             format = "analytic",
                             space_res = "2.5min",
                             time_res = 1,
                             verbose = c(1, 3)))

})





test_that("Test  getIndices function", {

  expect_equal(getIndices(pam@data[["PAM"]], 2, "analytic"), seq(26,49))
  expect_equal(getIndices(pam@data[["PAM"]], 20, "analytic"), seq(8,57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "analytic"), seq(1,57))

})








