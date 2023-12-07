test_that("returned object classes are correct", {

  # load data
  casth <- terra::rast(system.file("extdata", "castro2011.birds.tif",
                            package="net.raster"))
  castl <- terra::rast(system.file("extdata", "castro2011.plants.tif",
                            package="net.raster"))
  castnet <- read.csv(system.file("extdata", "castro2011adptd.csv",
                                  package="net.raster"), row.names=1, sep= ";" )

  t <- prep.web(casth, castl, castnet)

  # tests
  expect_type(t, "list")
})

test_that("error is returned when an argument is missing", {

  # load data
  casth <- terra::rast(system.file("extdata", "castro2011.birds.tif",
                            package="net.raster"))
  castl <- terra::rast(system.file("extdata", "castro2011.plants.tif",
                            package="net.raster"))
  castnet <- read.csv(system.file("extdata", "castro2011adptd.csv",
                                  package="net.raster"), row.names=1, sep= ";" )

  # tests
  expect_error(prep.web(casth, castl))
  expect_error(prep.web(casth, castnet))
  expect_error(prep.web(castl, castnet))
  expect_error(prep.web(castnet))
  expect_error(prep.web(casth))
  expect_error(prep.web(castl))

})
