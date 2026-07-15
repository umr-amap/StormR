
test_that("Tests checkInputscomputeTEW.SpatRaster function", {

  # Checking sts input (missing)
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = prof,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      product = "TEW1wd",
      verbose = 0
    )
  )

  # Checking dtm input (missing)
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = prof,
      sts = pam,
      angle = 6,
      threshold = 0,
      product = "TEW1wd",
      verbose = 0
    )
  )

  # Checking profiles input (missing)
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      sts = pam,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      product = "TEW1wd",
      verbose = 0
    )
  )

  # Checking profiles input (wrong type)
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = "not_a_raster",
      sts = pam,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      product = "TEW1wd",
      verbose = 0
    )
  )

  # Checking product input (invalid)
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = prof,
      sts = pam,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      product = "InvalidProduct",
      verbose = 0
    )
  )

  # Checking threshold input
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = prof,
      sts = pam,
      dtm = mnt,
      angle = 6,
      threshold = -1,
      product = "TEW1wd",
      verbose = 0
    )
  )

  # Checking angle input
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = prof,
      sts = pam,
      dtm = mnt,
      angle = -1,
      threshold = 0,
      product = "TEW1wd",
      verbose = 0
    )
  )

  # Checking verbose input
  expect_error(
    checkInputscomputeTEW.SpatRaster(
      profiles = prof,
      sts = pam,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      product = "TEW1wd",
      verbose = 3
    )
  )
})

test_that("Tests checkInputscomputeTEW.list function", {

  # Checking points input (missing)
  expect_error(
    checkInputscomputeTEW.list(
      profiles = TS,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      verbose = 0
    )
  )

  # Checking dtm input (missing)
  expect_error(
    checkInputscomputeTEW.list(
      profiles = TS,
      points = df,
      angle = 6,
      threshold = 0,
      verbose = 0
    )
  )

  # Checking profiles input (missing)
  expect_error(
    checkInputscomputeTEW.list(
      points = df,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      verbose = 0
    )
  )

  # Checking profiles input (wrong type)
  expect_error(
    checkInputscomputeTEW.list(
      profiles = "not_a_list",
      points = df,
      dtm = mnt,
      angle = 6,
      threshold = 0,
      verbose = 0
    )
  )

  # Checking angle input
  expect_error(
    checkInputscomputeTEW.list(
      profiles = TS,
      points = df,
      dtm = mnt,
      angle = -1,
      threshold = 0,
      verbose = 0
    )
  )

  # Checking threshold input
  expect_error(
    checkInputscomputeTEW.list(
      profiles = TS,
      points = df,
      dtm = mnt,
      angle = 6,
      threshold = -1,
      verbose = 0
    )
  )
})

test_that("computeShade computes correct values with numeric inputs", {
  z_deg <- 90 - 6
  z <- z_deg * pi / 180

  # Flat terrain: slope = 0, any aspect
  shade_flat <- computeShade(0, 0, angle = 6, degreeDir = 0)
  expected_flat <- cos(0) * cos(z) + sin(0) * sin(z) * cos(0 - 0)
  expect_equal(shade_flat, expected_flat)

  # Slope = 0.5 rad, aspect = 0 (north), wind from north (0 deg)
  shade_north <- computeShade(0.5, 0, angle = 6, degreeDir = 0)
  expected_north <- cos(0.5) * cos(z) + sin(0.5) * sin(z) * cos(0 - 0)
  expect_equal(shade_north, expected_north)

  # Slope = 0.5 rad, aspect = 0, wind from south (180 deg) -> leeward
  shade_south <- computeShade(0.5, 0, angle = 6, degreeDir = 180)
  expected_south <- cos(0.5) * cos(z) + sin(0.5) * sin(z) * cos(180 * pi / 180 - 0)
  expect_equal(shade_south, expected_south)

  # Steeper slope
  shade_steep <- computeShade(1.0, pi / 4, angle = 10, degreeDir = 45)
  z2 <- (90 - 10) * pi / 180
  expected_steep <- cos(1.0) * cos(z2) + sin(1.0) * sin(z2) * cos(45 * pi / 180 - pi / 4)
  expect_equal(shade_steep, expected_steep)

  # Shade values should be in reasonable range (< 1.1)
  expect_lt(abs(shade_flat), 1.1)
  expect_lt(abs(shade_north), 1.1)
  expect_lt(abs(shade_south), 1.1)
  expect_lt(abs(shade_steep), 1.1)

  # Windward slope should give higher exposure than leeward
  expect_gt(shade_north, shade_south)
})

test_that("getTerrain returns slope and aspect in radians", {
  topo <- getTerrain(mnt)

  expect_type(topo, "list")
  expect_named(topo, c("slope", "aspect"))
  expect_s4_class(topo$slope, "SpatRaster")
  expect_s4_class(topo$aspect, "SpatRaster")
  expect_true(terra::compareGeom(topo$slope, topo$aspect, stopOnError = FALSE))
  expect_true(terra::compareGeom(topo$slope, mnt, stopOnError = FALSE))
})

test_that("getValueMaxSpeed and getWindDirection extract direction at max speed", {

  layersMSW <- names(prof)[grep("_Speed_", names(prof))]
  layersDir <- names(prof)[grep("_Direction_", names(prof))]

  # Must have at least one pair
  expect_gt(length(layersMSW), 0)
  expect_gt(length(layersDir), 0)

  idx <- 1
  speed_layer <- prof[[layersMSW[idx]]]
  dir_layer <- prof[[layersDir[idx]]]

  dir_val <- getValueMaxSpeed(dir_layer, speed_layer)
  dir_val2 <- getWindDirection(speed_layer, dir_layer)

  expect_type(dir_val, "double")
  expect_length(dir_val, 1)
  expect_true(dir_val >= 0 && dir_val <= 360 || is.na(dir_val))
  expect_equal(dir_val2, dir_val)
})

test_that("computeTEWProfiles returns exposure profiles with usePixel = FALSE", {

  layersMSW <- names(prof)[grep("_Speed_", names(prof))]
  layersDir <- names(prof)[grep("_Direction_", names(prof))]

  result <- computeTEWProfiles(prof, layersMSW, layersDir, topo, angle = 6, threshold = 0, usePixel = FALSE)

  expect_s4_class(result, "SpatRaster")
  expect_gt(terra::nlyr(result), 0)

  # Layer names should contain TEW1wd
  layer_names <- names(result)
  expect_true(all(grepl("TEW1wd", layer_names)))
})

test_that("computeTEWProfiles returns exposure profiles with usePixel = TRUE", {

  layersMSW <- names(prof)[grep("_Speed_", names(prof))]
  layersDir <- names(prof)[grep("_Direction_", names(prof))]

  result <- computeTEWProfiles(prof, layersMSW, layersDir, topo, angle = 6, threshold = 0, usePixel = TRUE)

  expect_s4_class(result, "SpatRaster")
  expect_gt(terra::nlyr(result), 0)

  # Layer names should contain TEW_ (not TEW1wd)
  layer_names <- names(result)
  expect_true(all(grepl("TEW_", layer_names)))
  expect_true(!any(grepl("TEW1wd", layer_names)))
})

test_that("computeTEWProfiles returns NULL when no layers meet threshold", {

  layersMSW <- names(prof)[grep("_Speed_", names(prof))]
  layersDir <- names(prof)[grep("_Direction_", names(prof))]

  # Very high threshold that no layer can meet
  result <- computeTEWProfiles(prof, layersMSW, layersDir, topo, angle = 6, threshold = 1e6, usePixel = FALSE)

  expect_null(result)
})

test_that("computeTEWIntegrated returns a single-layer SpatRaster", {

  layersMSW <- names(prof)[grep("_Speed_", names(prof))]
  layersDir <- names(prof)[grep("_Direction_", names(prof))]

  result <- computeTEWIntegrated(prof, layersMSW, layersDir, topo, angle = 6, threshold = 0)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 1)
})

test_that("computeTEW.SpatRaster produces correct products", {

  # Test TEWIntegrated (default)
  result <- computeTEW(prof, pam, mnt, product = "TEWIntegrated", verbose = 0)
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 1)
  expect_true(grepl("PAM_TEW_Integrated", names(result)))

  # Test TEW1wdMean
  result_mean <- computeTEW(prof, pam, mnt, product = "TEW1wdMean", verbose = 0)
  expect_s4_class(result_mean, "SpatRaster")
  expect_equal(terra::nlyr(result_mean), 1)
  expect_true(grepl("PAM_TEW1wd_Mean", names(result_mean)))

  # Test TEW1wdMax
  result_max <- computeTEW(prof, pam, mnt, product = "TEW1wdMax", verbose = 0)
  expect_s4_class(result_max, "SpatRaster")
  expect_equal(terra::nlyr(result_max), 1)
  expect_true(grepl("PAM_TEW1wd_Max", names(result_max)))

  # Test TEW1wd (multi-layer)
  result_1wd <- computeTEW(prof, pam, mnt, product = "TEW1wd", verbose = 0)
  expect_s4_class(result_1wd, "SpatRaster")
  expect_gt(terra::nlyr(result_1wd), 0)
  expect_true(all(grepl("TEW1wd", names(result_1wd))))

  # Test TEW (pixel-based, multi-layer)
  result_tew <- computeTEW(prof, pam, mnt, product = "TEW", verbose = 0)
  expect_s4_class(result_tew, "SpatRaster")
  expect_gt(terra::nlyr(result_tew), 0)
  expect_true(all(grepl("TEW_", names(result_tew))))
  expect_true(!any(grepl("TEW1wd", names(result_tew))))
})


test_that("computeTEW.SpatRaster with very high threshold returns NULL", {

  expect_warning(
    result <- computeTEW(prof, pam, mnt, product = "TEW1wd", threshold = 1e6, verbose = 0),
    "No layers met the wind speed threshold"
  )
  expect_null(result)
})

test_that("computeTEW.list returns data.frames with tew column", {

  result <- computeTEW(TS, df, mnt, angle = 6, threshold = 0, verbose = 0)

  expect_type(result, "list")
  expect_true("PAM" %in% names(result))
  expect_type(result$PAM, "list")
  expect_equal(length(result$PAM), nrow(df))
  expect_true("tew" %in% names(result$PAM[[1]]))
  expect_type(result$PAM[[1]]$tew, "double")
  expect_equal(length(result$PAM[[1]]$tew), length(result$PAM[[1]]$speed))
})

test_that("computeTEW.list respects threshold", {

  result_low <- computeTEW(TS, df, mnt, angle = 6, threshold = 0, verbose = 0)
  result_high <- computeTEW(TS, df, mnt, angle = 6, threshold = 100, verbose = 0)

  # Higher threshold should result in more NA tew values (or all NA)
  na_low <- sum(is.na(result_low$PAM[[1]]$tew))
  na_high <- sum(is.na(result_high$PAM[[1]]$tew))
  expect_gte(na_high, na_low)
})
