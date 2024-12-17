#' @title Prepares raster and network data
#'
#' @description Excludes in the network the species that not interact. Also checks
#' whether the species names of each Spatraster (higher and lower level) match
#' the species names in the cleaned network.
#'
#' @param rh SpatRaster. A raster (stack) containing presence-absence data (0 or 1)
#' for the higher level set of species.
#' @param rl A SpatRaster. A raster (stack) containing presence-absence data (0 or 1)
#' for the lower level set of species.
#' @param web Matrix. A weighted bipartite network matrix, binary (o or 1) or
#' not, where the lower level species (e.g. plants) are rows and higher level (e.g.
#' frugivores or pollinators)species are columns. The layers (species) of each
#' raster must be sorted according to the bipartite network order!
#'
#' @return A list with a matrix (subnetwork with the species present in rasters),
#' a logical vector indicating which species are from higher level among all,
#' and the two cleaned raster stacks of species.Note that net.raster only allows
#' unweighted calculation of bipartite network metrics.
#'
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
#' resu  <- prep.web (rasth, rastl, bipnet)
#' resu
#'
#'}
#' @export
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira

prep.web <- function(rh, rl, web) {
  web <- data.frame(web[rowSums(web)>0, colSums(web)>0]) #excluding species that not interact

  sph <- intersect(colnames(web), names(rh))
  spl <- intersect(rownames(web), names(rl))

  if(length(sph)==0 | length(spl)==0){
    stop("The species names in web do not match with lower level or higher level")
  }

  hlyr <- rep(c(T,F), c(length(sph),length(spl)))

  return(list(web_sub=data.frame(web[spl, sph]), #subset of species present in raster stacks
              hlyr=hlyr,
              rh = rh[[sph]],
              rl = rl[[spl]]))

}



