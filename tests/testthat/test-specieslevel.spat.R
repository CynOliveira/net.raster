test_that("returned object classes are correct", {

  # load data
  rasth <- rast(system.file("extdata", "rasth.tif",
                            package="net.raster"))
  rastl <- rast(system.file("extdata", "rastl.tif",
                            package="net.raster"))
  bipnet <- read.csv(system.file("extdata", "bipnet.csv",
                                 package="net.raster"), row.names=1, sep= ";" )

  t <- specieslevel.spat (rasth, rastl, bipnet, index="species strength", level="higher")

  # tests
  expect_s4_class(t, "SpatRaster")
})

test_that("error is returned when an argument is missing", {

  # load data
  rasth <- rast(system.file("extdata", "rasth.tif",
                            package="net.raster"))
  rastl <- rast(system.file("extdata", "rastl.tif",
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

test_that("error is returned when the raster does not have a longitude/latitude
          coordinate reference system (CRS)", {

            x <- terra::rast(system.file("extdata", "rast.presab.tif",
                                         package="phyloraster"))
            tree <- ape::read.tree(system.file("extdata", "tree.nex",
                                               package="phyloraster"))

            # getting fewer cells to test all values
            x <- terra::crop(x, terra::ext(c(150.0157, 150.8157,
                                             -23.044, -22.8563)))

            w <- terra::project(x, "EPSG:2169")

            data <- phyloraster::phylo.pres(w, tree)
            # branch.length <- data$branch.length
            # n.descen <- data$n.descendants
            # inv.R <- phyloraster::inv.range(data$x)

            # tests
            expect_error(geo.phylo(data$x,
                                   data$n.descendants))
          })
