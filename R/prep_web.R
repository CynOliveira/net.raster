#' @title Checks if the raster and network names are the same
#'
#' @description Checks whether the species names of each Spatraster (higher and
#' lower level) match the species names in the network. To do this, it creates a
#' subnet from a subset of species present in raster stacks. In addition to
#' allowing all net.rasters functions to be correctly executed, it is useful if
#' the rasters contain fewer species than the bipartite network, as it was not
#' possible to model or obtain rasters for all species recorded on the web.
#'
#'
#' @param rh SpatRaster of higher level species
#' @param rl SpatRaster of lower level species
#' @param web A bipartite network, which can be weighted or not
#'
#'
#' @return A list with a matrix (subnetwork with the species present in rasters)
#' and a logical vector indicating if the species are from higher level.
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(net.raster)
#' # load bipartite network and the raster stacks of higher level and lower level
#' species
#' bipnet <- read.csv(system.file("extdata", "bipnet.csv",
#' package="net.raster"), row.names=1)
#' rasth <- rast(system.file("extdata", "rasth.tif",
#' package="net.raster"))
#' rastl <- rast(system.file("extdata", "rastl.tif",
#' package="net.raster"))
#' # applying the function to check species names on all data
#' resu  <- prep_web (rasth, rastl, bipnet)
#' resu
#'
#'}
#' @export
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira

prep_web <- function(rh, rl, web) {
  sph <- names(rh)
  spl <- names(rl)

  ch <- sum(sph %in% colnames(web))
  cl <- sum(spl %in% rownames(web))

  if(ch==0 | cl==0){
    stop("The species names in web do not match with lower level or higher level")
  }

  web_sub <- web[spl, sph] #subset of species present in raster stacks

  hlyr <- rep(c(T,F), c(length(sph),length(spl)))
  return(list(web_sub=web_sub, hlyr=hlyr))


}

