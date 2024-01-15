
test_that("Test conversions functions",{
  
  expect_equal(knt2ms(1), 0.514)
  expect_equal(mph2ms(1), 0.44704)
  expect_equal(kmh2ms(1), 1 / 3.6)
  expect_equal(nm2km(1), 1.852)
  expect_equal(b2pa(1), 100000)
  expect_equal(mb2pa(1), 100)
  expect_equal(psi2pa(1), 6895)
  expect_equal(atm2pa(1), 101300)
  
})



test_that("Test checkInputsdefStormsDataset function", {
  # Checking filename input
  expect_error(
    checkInputsdefStormsDataset(
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = 1,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = TRUE,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = c("data", ".nc"),
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  # Checking fields input
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = 1,
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = TRUE,
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(), "%Y"))),
      unitConversion = c(msw = "knt2ms",
                         rmw = "nm2km",
                         poci = "mb2pa"),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      verbose = TRUE
    )
  )
  
  # Checking basin input
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = 1,
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = TRUE,
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = c("NA", "WP"),
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "basin",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  # Checking seasons
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
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
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  
  # Checking unitConversion input
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = 1,
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = TRUE,
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  

  # Checking verbose input
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
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
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = -1
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
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
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = "1"
    )
  )
  
  expect_error(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = TRUE
    )
  )
  

  # Check Warnings
  expect_warning(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "pressure" = "usa_pres",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  
  expect_warning(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "poci" = "usa_poci"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
  
  expect_warning(
    checkInputsdefStormsDataset(
      filename = "database.nc",
      sep = NULL,
      fields = c(
        "basin" = "basin",
        "names" = "name",
        "seasons" = "season",
        "isoTime" = "iso_time",
        "lon" = "usa_lon",
        "lat" = "usa_lat",
        "msw" = "usa_wind",
        "rmw" = "usa_rmw",
        "pressure" = "usa_pres"
      ),
      basin = "SP",
      seasons = c(1980, as.numeric(format(Sys.time(
        
      ), "%Y"))),
      unitConversion = c(
        msw = "knt2ms",
        rmw = "nm2km",
        pressure = "mb2pa",
        poci = "mb2pa"
      ),
      notNamed = "NOT_NAMED",
      verbose = 1
    )
  )
})


test_that("Test getDataFrom functions", {
  
  fields = c(
    names = "name",
    seasons = "season",
    isoTime = "iso_time",
    lon = "usa_lon",
    lat = "usa_lat",
    msw = "usa_wind",
    basin = "basin",
    sshs = "usa_sshs",
    rmw = "usa_rmw",
    pressure = "usa_pres",
    poci = "usa_poci"
  )
 
  unitConversion = c(
    msw = "knt2ms",
    rmw = "nm2km",
    pressure = "mb2pa",
    poci = "mb2pa"
  )
  
  filename = system.file("extdata", "test_dataset.csv", package = "StormR")
  databaseFromCsv <- getDataFromCsvFile(filename=filename,
                                        sep = ",",
                                        fields=fields,
                                        basin=NULL,
                                        seasons=c(2015,2020),
                                        unitConversion=unitConversion,
                                        notNamed="NOT_NAMED",
                                        verbose=0)
  
  filename = system.file("extdata", "test_dataset.nc", package = "StormR")
  databaseFromNc <- getDataFromNcdfFile(filename=filename,
                                        fields=fields,
                                        basin=NULL,
                                        seasons=c(2015,2020),
                                        unitConversion=unitConversion,
                                        notNamed="NOT_NAMED",
                                        verbose=0)
  
  expect_equal(convertVariables(databaseFromCsv, unitConversion), sdsFromCsv@database)
  expect_equal(convertVariables(databaseFromNc, unitConversion), sdsFromNc@database)
  
})
