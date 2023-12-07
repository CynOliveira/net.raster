test_that("returned object classes are correct", {

  # load data
  rasth <- terra::rast(system.file("extdata", "rasth.tif",
                            package="net.raster"))
  rastl <- terra::rast(system.file("extdata", "rastl.tif",
                            package="net.raster"))
  bipnet <- read.csv(system.file("extdata", "bipnet.csv",
                                 package="net.raster"), row.names=1, sep= ";" )

  t <- specieslevel.spat (rasth, rastl, bipnet, index="species strength", level="higher")

  # tests
  expect_s4_class(t, "SpatRaster")
})

test_that("error is returned when an argument is missing", {

  # load data
  rasth <- terra::rast(system.file("extdata", "rasth.tif",
                            package="net.raster"))
  rastl <- terra::rast(system.file("extdata", "rastl.tif",
                            package="net.raster"))
  bipnet <- read.csv(system.file("extdata", "bipnet.csv",
                                 package="net.raster"), row.names=1, sep= ";" )
  # tests
  expect_error(specieslevel.spat(rasth, rastl, level="higher"))
  expect_error(specieslevel.spat(rasth, bipnet, level="higher"))
  expect_error(specieslevel.spat(rastl, bipnet, level="higher"))
  expect_error(specieslevel.spat(bipnet, level="higher"))
  expect_error(specieslevel.spat(rasth, level="higher"))
  expect_error(specieslevel.spat(rastl, level="higher"))

})
