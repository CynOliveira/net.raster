test_that("returned object classes are correct", {

  # load data
  casth <- terra::rast(system.file("extdata", "castro2011.birds.tif",
                            package="net.raster"))
  castl <- terra::rast(system.file("extdata", "castro2011.plants.tif",
                            package="net.raster"))
  castnet <- read.csv(system.file("extdata", "castro2011adptd.csv",
                                  package="net.raster"), row.names=1, sep= ";" )

  t <- computeModules.spat(casth, castl, castnet)

  # tests
  expect_s4_class(t, "SpatRaster")
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
  expect_error(computeModules.spat(casth, castl))
  expect_error(computeModules.spat(casth, castnet))
  expect_error(computeModules.spat(castl, castnet))
  expect_error(computeModules.spat(castnet))
  expect_error(computeModules.spat(casth))
  expect_error(computeModules.spat(castl))

})
