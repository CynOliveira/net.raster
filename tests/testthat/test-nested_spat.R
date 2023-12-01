test_that("returned object classes are correct", {

  # load data
  rh <- terra::rast(system.file("extdata", "rasth.tif",
                               package="renm"))
  rl <- terra::rast(system.file("extdata", "rastl.tif",
                                package="renm"))
  web <- read.csv(system.file("data-raw", "web.csv",
                                     package="renm"))

  names(web)[1] <- c(" ")

  rownames(web) <- c("citharexylum_myrianthum", "morus_nigra",
                        "cestrum_mariquitense", "momordica_charantia",
                        "schinus_terebinthifolia", "eugenia_uniflora",
                        "melia_azedarach", "cecropia_pachystachya",
                        "maytenus_aquifolia")

  web <- web[-1]

  colnames(web) <- c("elaenia_flavogaster", "myiozetetes_similis",
                        "pitangus_sulphuratus", "myiodynastes_maculatus",
                        "megarynchus_pitangua", "myiarchus_ferox",
                        "tyrannus_melancholicus", "mimus_saturninus",
                        "turdus_amaurochalinus", "zonotrichia_capensis",
                        "pyrrhocoma_ruficeps", "tachyphonus_coronatus",
                        "tangara_cayana", "turdus_leucomelas","tangara_sayaca")

  t <- nested.spat(rh, rl, web, method="weighted NODF", rescale=FALSE,
                               normalised=TRUE)

  # tests
  expect_s4_class(t, "SpatRaster")
  })
