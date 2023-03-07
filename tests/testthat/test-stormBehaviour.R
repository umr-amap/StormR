



test_that("Test getRmw function", {

  expect_equal(getRmw(seq(0,80,5), -15), c(60, 55, 51, 47, 44, 41, 38, 35, 32,
                                           30, 28, 25, 24, 22, 20, 19, 17))

})





test_that("Test Willougbhy and Holland functions", {

  expect_equal(Willoughby(seq(1,100), 20, 50, -15),
               c(1.813,  3.905,  6.118,  8.414, 10.772, 13.182, 15.635, 18.127, 20.652, 23.208,
                 25.791, 28.399, 31.032, 33.685, 36.360, 39.053, 41.765, 44.494, 47.239, 50.000,
                 49.327, 48.676, 48.044, 47.433, 46.839, 46.264, 45.706, 45.165, 44.640, 44.131,
                 43.636, 43.155, 42.688, 42.235, 41.794, 41.366, 40.949, 40.543, 40.149, 39.765,
                 39.391, 39.027, 38.672, 38.327, 37.990, 37.661, 37.341, 37.028, 36.723, 36.425,
                 36.134, 35.850, 35.572, 35.300, 35.035, 34.775, 34.521, 34.272, 34.028, 33.789,
                 33.555, 33.326, 33.102, 32.881, 32.665, 32.453, 32.245, 32.040, 31.839, 31.642,
                 31.448, 31.257, 31.070, 30.885, 30.704, 30.525, 30.349, 30.176, 30.005, 29.837,
                 29.671, 29.508, 29.347, 29.188, 29.031, 28.876, 28.724, 28.573, 28.424, 28.277,
                 28.131, 27.988, 27.846, 27.706, 27.567, 27.430, 27.294, 27.159, 27.026, 26.895))

  expect_equal(Holland(seq(1,100), 20, 50, 1020*100, 915*100, -15),
               c(25.621, 31.978, 36.024, 38.946, 41.180, 42.944, 44.365, 45.524, 46.474, 47.257,
                 47.901, 48.428, 48.856, 49.199, 49.469, 49.675, 49.825, 49.926, 49.983, 50.001,
                 49.984, 49.937, 49.861, 49.761, 49.638, 49.495, 49.333, 49.155, 48.962, 48.755,
                 48.536, 48.306, 48.066, 47.817, 47.559, 47.295, 47.023, 46.746, 46.463, 46.175,
                 45.883, 45.587, 45.288, 44.985, 44.681, 44.373, 44.064, 43.753, 43.441, 43.128,
                 42.814, 42.499, 42.183, 41.868, 41.552, 41.236, 40.921, 40.606, 40.291, 39.977,
                 39.664, 39.352, 39.040, 38.729, 38.420, 38.112, 37.805, 37.499, 37.195, 36.892,
                 36.591, 36.291, 35.993, 35.696, 35.401, 35.108, 34.817, 34.527, 34.239, 33.953,
                 33.669, 33.386, 33.106, 32.827, 32.550, 32.275, 32.003, 31.732, 31.463, 31.196,
                 30.931, 30.667, 30.406, 30.147, 29.890, 29.635, 29.381, 29.130, 28.881, 28.633))

})





test_that("Test checkInputsSb function", {

  #Checking sts input
  expect_error(checkInputsSb(product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  #Checking product input
  expect_error(checkInputsSb(sts = pam,
                             product = 1,
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = "TS",
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  #Check wind_threshold input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c("18",  "33",  "42",  "49",  "58",  "70"),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(-3, -45),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  #Checking method input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = 1,
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = TRUE,
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = c("Willougbhy", "Holland"),
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  #Checking asymmetry input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = 1,
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = TRUE,
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = c("Chen", "None"),
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  #Checking empirical_rmw inputs
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = "TRUE",
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = 1,
                             space_res = "2.5min",
                             temp_res = 1,
                             verbose = 2))

  #Checking temp_res input
  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = TRUE,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = 2.3,
                             verbose = 2))

  expect_error(checkInputsSb(sts = pam,
                             product = c("MSW", "PDI", "Exposure"),
                             wind_threshold = c(18,  33,  42,  49,  58,  70),
                             method = "Willougbhy",
                             asymmetry = "Chen",
                             empirical_rmw = FALSE,
                             space_res = "2.5min",
                             temp_res = c(0.25, 0.5),
                             verbose = 2))

})





test_that("Test getIndices function", {

  expect_equal(getIndices(pam@data[["PAM"]], 2, "MSW"), seq(26,49))
  expect_equal(getIndices(pam@data[["PAM"]], 20, "MSW"), seq(8,57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "MSW"), seq(1,57))
  expect_equal(getIndices(pam@data[["PAM"]], 30, "Profiles"), seq(28,47))

})





test_that("Test getDataInterpolate function", {

  expect_equal(getDataInterpolate(pam@data[["PAM"]], seq(26,49), 4, 3, FALSE, "Willoughby"), df_getDataInterpolate)

})





test_that("Test computeDirection function", {

  expect_equal(computeDirection(1, 1, 30), 315)
  expect_equal(computeDirection(1, -1, 30), 45)
  expect_equal(computeDirection(-1, -1, 30), 135)
  expect_equal(computeDirection(-1, 1, 30), 225)

  expect_equal(computeDirection(0, 1, 30), 270)
  expect_equal(computeDirection(1, 0, 30), 0)
  expect_equal(computeDirection(0, -1, 30), 90)
  expect_equal(computeDirection(-1, 0, 30), 180)

  expect_equal(computeDirection(1, 1, -30), 135)
  expect_equal(computeDirection(1, -1, -30), 225)
  expect_equal(computeDirection(-1, -1, -30), 315)
  expect_equal(computeDirection(-1, 1, -30), 45)

  expect_equal(computeDirection(0, 1, -30), 90)
  expect_equal(computeDirection(1, 0, -30), 180)
  expect_equal(computeDirection(0, -1, -30), 270)
  expect_equal(computeDirection(-1, 0, -30), 360)

})





test_that("Test checkInputsSbPt function", {

  #Checking sts input
  expect_error(checkInputsSbPt(points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  #Checking product input
  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = 1,
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "MSW",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = c("TS", "PDI", "Exposure"),
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  #Check wind_threshold input
  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "Exposure",
                               wind_threshold = c("18",  "33",  "42",  "49",  "58",  "70"),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "Exposure",
                               wind_threshold = c(-3, -45),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  #Checking method input
  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = 1,
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = TRUE,
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = c("Willougbhy", "Holland"),
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  #Checking asymmetry input
  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "chen",
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = 1,
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = TRUE,
                               empirical_rmw = FALSE,
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = c("Chen", "None"),
                               empirical_rmw = FALSE,
                               temp_res = 1))

  #Checking empirical_rmw inputs
  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = "TRUE",
                               temp_res = 1))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = 1,
                               temp_res = 1))

  #Checking temp_res input
  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = TRUE))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = 2.3))

  expect_error(checkInputsSbPt(sts = pam,
                               points = data.frame(lon = 169, lat = -19),
                               product = "TS",
                               wind_threshold = c(18,  33,  42,  49,  58,  70),
                               method = "Willougbhy",
                               asymmetry = "Chen",
                               empirical_rmw = FALSE,
                               temp_res = c(0.25, 0.5)))

})





test_that("Test computePDI function", {

  expect_equal(computePDI(seq(0,80), 1), 6.48)
  expect_equal(computePDI(seq(0,80,0.75), 0.75), 6.38)
  expect_equal(computePDI(seq(0,80,0.5), 0.5), 6.44)
  expect_equal(computePDI(seq(0,80,0.25), 0.25), 6.42)

})





test_that("Test computeExposure function", {

  expect_equal(computeExposure(seq(0,80,1), 1, c(30,50)), c(51, 31))
  expect_equal(computeExposure(seq(0,80,0.5), 0.5, c(30,50)), c(50.5, 30.5))
  expect_equal(computeExposure(seq(0,80,0.75), 0.75, c(30,50)), c(50.25, 30))
  expect_equal(computeExposure(seq(0,80,0.25), 0.25, c(30,50)), c(50.25, 30.25))

})










