



test_that("Test checkInputsdefStormsDataset function", {
  # Checking filename input
  expect_error(checkInputsdefStormsDataset(
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = 1,
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = TRUE,
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))


  expect_error(checkInputsdefStormsDataset(
    filename = c("data", ".nc"),
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  # Checking fields input
  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = 1,
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = TRUE,
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))


  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb_to_pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb_to_pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = TRUE,
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = TRUE
  ))

  # Checking basin input
  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = 1,
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = TRUE,
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = c("NA", "WP"),
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "basin",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  # Checking seasons
  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = 2000,
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(2020, 2000),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c("2000", "2020"),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = TRUE,
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))


  # Checking unitConversion input
  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = 1,
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = TRUE,
    verbose = 1
  ))


  # Checking verbose input
  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = 1,
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_error(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = 1,
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = "1"
  ))

  # Warnings
  expect_warning(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "pressure" = "usa_pres",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_warning(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "poci" = "usa_poci"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))

  expect_warning(checkInputsdefStormsDataset(
    filename = "database.nc",
    fields = c(
      "basin" = "basin",
      "names" = "name",
      "seasons" = "season",
      "isoTime" = "iso_time",
      "lon" = "usa_lon",
      "lat" = "usa_lat",
      "msw" = "usa_wind",
      "sshs" = "usa_sshs",
      "rmw" = "usa_rmw",
      "pressure" = "usa_pres"
    ),
    basin = "SP",
    seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
    unitConversion = c(
      msw = "knt2ms",
      rmw = "nm2km",
      pressure = "mb2pa",
      poci = "mb2pa"
    ),
    verbose = 1
  ))
})
