test_that("returned object classes are correct", {

  # load data
  rasth <- terra::rast(system.file("extdata", "rasth.tif",
                            package="net.raster"))
  rastl <- terra::rast(system.file("extdata", "rastl.tif",
                            package="net.raster"))
  bipnet <- read.csv(system.file("extdata", "bipnet.csv",
                                  package="net.raster"), row.names=1, sep= ";" )

  t <- networklevel.spat (rasth, rastl, bipnet, level="higher") #connectance (default)

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
  #connectance (default)
  expect_error(networklevel.spat(rasth, rastl, level="higher"))
  expect_error(networklevel.spat(rasth, bipnet, level="lower"))
  expect_error(networklevel.spat(rastl, bipnet, level="both"))
  expect_error(networklevel.spat(bipnet, level="higher"))
  expect_error(networklevel.spat(rasth, level="lower"))
  expect_error(networklevel.spat(rastl, level="both"))

})

test_that("error is returned when user select more than one index at a time", {

  # load data
  rasth <- terra::rast(system.file("extdata", "rasth.tif",
                                   package="net.raster"))
  rastl <- terra::rast(system.file("extdata", "rastl.tif",
                                   package="net.raster"))
  bipnet <- read.csv(system.file("extdata", "bipnet.csv",
                                 package="net.raster"), row.names=1, sep= ";" )

  # tests
  expect_error(networklevel.spat(rasth, rastl, bipnet, index="ALL"))
  expect_error(networklevel.spat(rasth, rastl, bipnet, index="ALLBUTDD"))
})
