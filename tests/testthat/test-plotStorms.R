


test_that("Tests invalid inputs", {

  #Checking `sts`  validity
  expect_error(plotStorms(),
               "no data to plot")

  # #Checking `shapefile` validity
  # expect_error(plotStorms(sts,),
  #              "no data to plot")




})
