#' @title Several nestedness metrics for each raster cell
#'
#' @description Calculates any of several nestedness metrics available for
#'  bipartite networks, which can be weighted or not, for each raster cell.
#'
#' @param x SpatRaster. Each pixel of a raster (stack) containing presence-
#' absence data (0 or 1) for both levels species (higher or lower trophic levels)
#' @param web Matrix. Each weighted bipartite subnetwork (in pixel) formed by the co-occurrence
#' modeled for the pairs of recorded interacting species, where the lower level
#' species are rows and higher level species are columns
#' @param hlyr Logical vector indicating if the species are from higher level
#'
#' @inheritParams bipartite::nested
#' @inheritParams backports::suppressWarnings
#'
#' @return SpatRaster
#'
#' @examples
#'
#' @export
#'
nested_vec <- function(x, web, hlyr, method = "NODF",
                        rescale=FALSE, normalised=TRUE){

  h.pix <- x[hlyr]==1
  l.pix <- x[!hlyr]==1

  resu <- NA

  if(all(is.na(x))){
    return(resu)
  }

  x[is.na(x)] <- 0

  if(sum(x, na.rm = T)==0){
    return(resu)
  }

  if(sum(h.pix, na.rm = T)==0|sum(l.pix, na.rm = T)==0)
    return(resu)

  # print(dim(web))
  # print(c(length(l.pix), length(h.pix)))
  web <- web[l.pix,h.pix]

  if(sum(web, na.rm = T)==0){
    return(resu)
  }



  nested.pix <- try(suppressWarnings(bipartite::nested(web, method=method, rescale=rescale,
                                      normalised=normalised)))

  if(!inherits(nested.pix, "try-error")){
    resu <- nested.pix
  }

  return(resu)
}


#' @title Several nestedness metrics for raster data
#'
#' @description Calculates any of several nestedness metrics available for
#'  bipartite networks, which can be weighted or not, for raster data. Users may
#'  choose one of the seven available nestedness indices. See more about them at
#'  https://cran.r-project.org/web/packages/bipartite/bipartite.pdf
#'
#' @param rh SpatRaster. A raster (stack) containing presence-absence data (0 or 1)
#' for the higher level set of species.
#' @param rl A SpatRaster. A raster (stack) containing presence-absence data (0 or 1)
#' for the lower level set of species.
#' @param web Matrix. A bipartite network matrix, where the lower level species
#' (e.g. plants) are rows and higher level (e.g. pollinators)species are columns.
#' The layers (species) of each raster must be sorted according to the bipartite
#' network order. The "prep.web" function test this internally, if not tested
#' before.
#'
#' @inheritParams bipartite::nested
#'
#' @return Spatraster with the nestedness metric required.
#'
#' @details
#' Note that if a network is very small, with few nodes or links, it may be
#' impossible to calculate the choosen metric or calculated result may be
#' unreliable.As the calculation is made with the subnet of each pixel, even in
#' cases like this, it is possible to visualize the spatialized metric on a
#' macroecological scale. See more about that and the available nestedness
#' indices at https://cran.r-project.org/web/packages/bipartite/bipartite.pdf
#' We also strongly recommend that users select one method at a time for spatial
#' calculation.
#'
#' @authors Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @references
#' Carsten F. Dormann ("bipartite" package)
#' Almeida-Neto, M., Guimaraes, P., Guimaraes, P.R., Loyola, R.D. and Ulrich,
#'  W. 2008. A consistent metric for nestedness analysis in ecological systems:
#'  reconciling concept and measurement. Oikos 117, 1227–1239.
#'  Almeida-Neto, M. and Ulrich, W. (2011) A straightforward computational
#'  approach for measuring nestedness using quantitative matrices. Environmental
#'  Modelling & Software, 26, 173–178
#'  Blüthgen, N., J. Fründ, D. P. Vazquez, and F. Menzel. 2008. What do
#'  interaction network metrics tell us about specialisation and biological
#'  traits? Ecology 89, 3387–3399.
#'  Brualdi, R.A. and Sanderson, J.G. 1999. Nested species subsets, gaps, and
#'  discrepancy. Oecologia 119, 256–264.
#'  Felix, G.M., Pinheiro, R.B.P., Poulin, R., Krasnov, B.R. & Mello, M.A.R.
#'  (2017). The compound topology of a continent-wide interaction network
#'  explained by an integrative hypothesis of specialization. bioRxiv
#'  Galeano, J., Pastor, J.M., Iriondo and J.M. 2008. Weighted-Interaction
#'  Nestedness Estimator (WINE): A new estimator to calculate over frequency
#'  matrices. arXiv 0808.3397v2 [physics.bio-ph]
#'  Rodríguez-Gironés, M.A. and Santamaría, L. 2006. A new algorithm to calculate
#'  the nestedness temperature of presence-absence matrices. J. Biogeogr. 33,
#'  924–935.
#'  Stone, L. and Roberts, A. 1990. The checkerboard score and species
#'  distributions. Oecologia 85, 74–79.
#'  Almeida-Neto, M. and Ulrich, W. 2010. A straightforward computational approach
#'  for measuring nestedness using quantitative matrices. Environmental
#'  Modelling & Software, in press.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(bipartite)
#' # load bipartite network and the raster stacks of higher level and lower level
#' species
#' bipnet <- read.csv(system.file("extdata", "bipnet.csv",
#' package="net.raster"), row.names=1, sep= ";" )#change separator if necessary
#' rasth <- rast(system.file("extdata", "rasth.tif",
#' package="net.raster"))
#' rastl <- rast(system.file("extdata", "rastl.tif",
#' package="net.raster"))
#' # applying the function to compute weighted NODF (default)
#' wNODF <- nested.spat (rasth, rastl, bipnet)
#' plot(wNODF)
#' # applying the function to compute C score nestdeness
#' Cscore <- nested.spat (rasth, rastl, bipnet, method = "C score")
#' plot(Cscore)
#'
#'}
#'
#' @export
nested.spat <- function(rh, rl, web, method="weighted NODF", rescale=FALSE,
                        normalised=TRUE) {

  pw <- prep.web(rh, rl, web)

  wlr <- terra::app(c(rh, rl),
                    nested_vec,
                    web=pw$web_sub, hlyr=pw$hlyr,
                    method=method, rescale=rescale) #level=level, weighted=weighted


  return(wlr)
}
