


test_that("Tests invalid inputs", {


  #Checking seasons input
  expect_error(getStorms(loi = "Vanuatu", seasons = "hui"))
  expect_error(getStorms(loi = "Vanuatu", seasons = 2015.6, names = "PAM"))
  expect_error(getStorms(loi = "Vanuatu", seasons = c(1850,2020)))
  expect_error(getStorms(loi = "Vanuatu", seasons = c(2000,2030)))
  expect_error(getStorms(loi = "Vanuatu", seasons =  c(2000,2005,2020)))

  #Checking names input
  expect_error(getStorms(loi = "Vanuatu", seasons = 2010, names = 1))


  #Checking max_dist input
  expect_error(getStorms(loi = c(165,-17), seasons = c(2016,2015), names = c("WINSTON","PAM"),
                         max_dist = "hui"))
  expect_error(getStorms(loi = c(165,-17), seasons = c(2016,2015), names = c("WINSTON","PAM"),
                         max_dist = c(1,3)))




  #Checking access to data
  expect_error(getStorms(loi = "Vanuatu", seasons = 2015, names = "PM"))
  expect_error(getStorms(loi = "Vanuatu", seasons = 2017, names = "PAM"))


})



test_that("Storm class getters", {

  expect_identical(getName(pam@data[["PAM"]]), pam@data[["PAM"]]@name)
  expect_identical(getSeason(pam@data[["PAM"]]), pam@data[["PAM"]]@season)
  expect_identical(getsshs(pam@data[["PAM"]]), pam@data[["PAM"]]@sshs)
  expect_identical(getNbObs(pam@data[["PAM"]]), pam@data[["PAM"]]@numobs.all)
  expect_identical(getObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs.all)
  expect_identical(getInObs(pam@data[["PAM"]]), pam@data[["PAM"]]@obs)

})


test_that("Storms class getters", {

  expect_identical(getStorm(sts_nc, "NIRAN"), sts_nc@data[["NIRAN"]])
  expect_identical(getNames(sts_nc), sts_nc@names)
  expect_identical(getSeasons(sts_nc), sts_nc@seasons)
  expect_identical(getSeasons(sts_nc, "NIRAN"), sts_nc@seasons[which(sts_nc@names == "NIRAN")])
  expect_identical(getSSHS(sts_nc), sts_nc@sshs)
  expect_identical(getSSHS(sts_nc, "NIRAN"), sts_nc@sshs[which(sts_nc@names == "NIRAN")])
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)
  expect_identical(getLOI(sts_nc), sts_nc@spatial.loi)
  expect_identical(getBuffer(sts_nc), sts_nc@spatial.loi.buffer)
  expect_identical(getBufferSize(sts_nc), sts_nc@buffer)

})

test_that("Storms class getters for Storm class", {

  expect_identical(getStormNbObs(sts_nc, "NIRAN"), getNbObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getStormObs(sts_nc, "NIRAN"), getObs(getStorm(sts_nc, "NIRAN")))
  expect_identical(getStormInObs(sts_nc, "NIRAN"), getInObs(getStorm(sts_nc, "NIRAN")))


})

