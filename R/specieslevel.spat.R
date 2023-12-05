#' @title Several network indices at the species level for each raster cell
#'
#' @description Calculate various simple numbers and indices related to network
#' nodes (species) for each pixel.
#'
#' @param x SpatRaster. Each pixel of a raster (stack) containing presence-
#' absence data (0 or 1) for both levels species (higher or lower trophic levels)
#' @param web Matrix. Each weighted bipartite subnetwork (in pixel) formed by the co-occurrence
#' modeled for the pairs of recorded interacting species, where the lower level
#' species are rows and higher level species are columns
#' @param hlyr Logical vector indicating if the species are from higher level
#'
#' @inheritParams bipartite::specieslevel
#' @inheritParams backports::suppressWarnings
#'
#' @return SpatRaster
#'
#' @authors Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @references
#'  Carsten F. Dormann & Jochen Fründ ("bipartite" package)
#'  Alarcon, R., Waser, N.M. and Ollerton, J. 2008. Year-to-year variation in the
#'  topology of a plantpollinator interaction network. Oikos 117, 1796–1807
#'  Barrat, A., M. Barthélemy, R. Pastor-Satorras, and A. Vespignani. 2004. The
#'  architecture of complex weighted networks. Proceedings of the National Academy
#'  of Sciences of the USA 101, 3747—3752. doi: 10.1073/pnas.0400087101.
#'  Bascompte, J., Jordano, P. and Olesen, J. M. (2006) Asymmetric coevolutionary
#'  networks facilitate biodiversity maintenance. Science 312, 431–433
#'  Berlow, E. L., A. M. Neutel, J. E. Cohen, P. C. de Ruiter, B. Ebenman, M.
#'  Emmerson, J. W. Fox,V. A. A. Jansen, J. I. Jones, G. D. Kokkoris, D. O.
#'  Logofet, A. J. McKane, J. M. Montoya & O.
#'  Petchey (2004) Interaction strengths in food webs: issues and opportunities.
#'  Journal of Animal Ecology 73, 585-–598
#'  Blüthgen, N., Menzel, F. and Blüthgen, N. (2006) Measuring specialization
#'  in species interaction networks. BMC Ecology 6, 9
#'  Dormann, C.F. (2011) How to be a specialist? Quantifying specialisation in
#'  pollination networks.Network Biology 1, 1–20
#'  Feinsinger, P., Spears, E.E. and Poole,R. W. (1981) A simple measure of
#'  niche breadth. Ecology 62, 27–32.
#'  Julliard, R., Clavel, J., Devictor, V., Jiguet, F. and Couvet, D. (2006)
#'  Spatial segregation of specialists and generalists in bird communities.
#'  Ecology Letters 9, 1237-–1244
#'  Martín Gonzáles, A.M., Dalsgaard, B. and Olesen, J.M. (2010) Centrality
#'  measures and the importance of generalist species in pollination networks.
#'  Ecological Complexity, 7, 36–43
#'  Opsahl, T. & Panzarasa, P. (2009). Clustering in weighted networks. Social
#'  Networks, 31, 155–163
#'  Poisot, T., Lepennetier, G., Martinez, E., Ramsayer, J., and Hochberg, M.E.
#'  (2011a) Resource availability affects the structure of a natural bacteria-
#'  bacteriophage community. Biology Letters 7,201–204
#'  Poisot, T., Bever, J.D., Nemri, A., Thrall, P.H., and Hochberg, M.E. (2011b)
#'  A conceptual framework for the evolution of ecological specialisation.
#'  Ecology Letters 14, 841–851
#'  Poisot, T., E. Canard, N. Mouquet, and M. E. Hochberg (2012) A comparative
#'  study of ecological specialization estimators. Methods in Ecology and
#'  Evolution 3, 537-–544. doi: 10.1111/j.2041-210X.2011.00174.x.
#'  Vázquez, D. P., Melian, C. J., Williams, N. M., Blüthgen N., Krasnov B. R.
#'  and Poulin, R. (2007) Species abundance and asymmetric interaction strength
#'  in ecological networks. Oikos 116, 1120–1127
#'
#' @examples
#'
#' @export
#'

sl_vec <- function(x, web, hlyr, index="closeness", level="higher", weighted=F,
                   logbase=exp(1), low.abun=NULL,
                   high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0),
                   nested.method="NODF",
                   nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE){

  h.pix <- x[hlyr]==1
  l.pix <- x[!hlyr]==1

  if(level=="higher"){
    resu <- rep(NA, length(h.pix)) # numeric(length(h.pix))
  } else {
    resu <- rep(NA, length(l.pix)) # numeric(length(l.pix))
  }

  if(all(is.na(x))){
    return(resu)
  }

  x[is.na(x)] <- 0

  if(sum(x, na.rm = T)==0){
    return(resu)
  }

  web <- web[l.pix,h.pix]

  if(sum(web, na.rm = T)==0){
    return(resu)
  }

  # nrow(web)|ncol(web)

  ## closeness of bird species (clos)
  web_clos.pix <- try(suppressWarnings(bipartite::specieslevel(web,
                                                  index=index, level=level)))

  if(!inherits(web_clos.pix, "try-error")){
    # return(web_clos.pix)
    if(level=="higher"){
      # print(web_clos.pix)
      spp.res <- which(h.pix)[colnames(web) %in% rownames(web_clos.pix)]
      resu[spp.res] <- web_clos.pix[,ifelse(weighted,2,1)]
    } else {
      spp.res <- which(l.pix)[rownames(web) %in% rownames(web_clos.pix)]
      resu[spp.res] <- web_clos.pix[,ifelse(weighted,2,1)]
    }
  }
  return(resu)
}


#' @title Several network indices at the species level for raster data
#'
#' @description Calculate various simple numbers and indices related to network
#' nodes (species) for raster data. You can see the spatial variation of important
#' metrics such as species strength and centralities (closeness, betweenness,
#' and degree). View all available indexes in Details and note that users
#' may select one index at a time for time-efficient spatial calculation.
#' Users can also choose both  or one level of interest at a time (higher - default
#' or lower), as well as choose unweighted metrics whenever their calculation
#' allows, with the default being weighted calculation.
#'
#' @param rh SpatRaster. A raster (stack) containing presence-absence data (0 or 1)
#' for the higher level set of species.
#' @param rl A SpatRaster. A raster (stack) containing presence-absence data (0 or 1)
#' for the lower level set of species.
#' @param web Matrix. A weighted bipartite network matrix, binary (o or 1) or not, where
#' the lower level species (e.g. plants) are rows and higher level (e.g.
#' pollinators)species are columns. The layers (species) of each raster must be
#' sorted according to the bipartite network order, use the prep_web() to check!
#'
#' @inheritParams terra::app
#'
#' @return Spatraster object with the choose species level metric for higher
#' trophic level (default), lower level or both.
#'
#' @details
#' Note that if a network is very small, with few nodes or links, it may be
#' impossible to calculate the choose metric or calculated result may be
#' unreliable. As the calculation is made with the subnet of each pixel, even in
#' cases like this, it is possible to visualize the spatialized metric on a
#' macroecological scale. Users may select one index at a time for time-efficient
#' spatial calculation. The current available indices for species level metrics
#' are listed bellow and they are adapted from "bipartite" package and users
#' can find more information about them at the specieslevel() documentation on CRAN
#' (https://cran.r-project.org/web/packages/bipartite/bipartite.pdf).
#' • ‘degree’,
#' • ‘ND’ - normalised degrees,
#' • ‘species strength’,
#' • ‘nestedrank’,
#' • ‘interaction push pull’ - interaction push/pull
#' • ‘PDI’ - Paired Differences Index,
#' • ‘resource range’,
#' • ‘species specificity’,
#' • ‘PSI’ - pollination service index ,
#' • ‘betweenness’ - betweenness centrality and its weighted counterpart,
#' • ‘closeness’ - closeness centrality and its weighted counterpart,
#' • ‘Fisher’ - Fisher’s alpha index,
#' • ‘diversity’ - Shannon diversity of interactions
#' • ‘effective partners’,
#' • ‘proportional generality’,
#' • ‘proportional similarity’,
#' • ‘d’ - Blüthgen’s d’,
#'
#' @authors Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @references
#'  Carsten F. Dormann & Jochen Fründ ("bipartite" package)
#'  Alarcon, R., Waser, N.M. and Ollerton, J. 2008. Year-to-year variation in the
#'  topology of a plantpollinator interaction network. Oikos 117, 1796–1807
#'  Barrat, A., M. Barthélemy, R. Pastor-Satorras, and A. Vespignani. 2004. The
#'  architecture of complex weighted networks. Proceedings of the National Academy
#'  of Sciences of the USA 101, 3747—3752. doi: 10.1073/pnas.0400087101.
#'  Bascompte, J., Jordano, P. and Olesen, J. M. (2006) Asymmetric coevolutionary
#'  networks facilitate biodiversity maintenance. Science 312, 431–433
#'  Berlow, E. L., A. M. Neutel, J. E. Cohen, P. C. de Ruiter, B. Ebenman, M.
#'  Emmerson, J. W. Fox,V. A. A. Jansen, J. I. Jones, G. D. Kokkoris, D. O.
#'  Logofet, A. J. McKane, J. M. Montoya & O.
#'  Petchey (2004) Interaction strengths in food webs: issues and opportunities.
#'  Journal of Animal Ecology 73, 585-–598
#'  Blüthgen, N., Menzel, F. and Blüthgen, N. (2006) Measuring specialization
#'  in species interaction networks. BMC Ecology 6, 9
#'  Dormann, C.F. (2011) How to be a specialist? Quantifying specialisation in
#'  pollination networks.Network Biology 1, 1–20
#'  Feinsinger, P., Spears, E.E. and Poole,R. W. (1981) A simple measure of
#'  niche breadth. Ecology 62, 27–32.
#'  Julliard, R., Clavel, J., Devictor, V., Jiguet, F. and Couvet, D. (2006)
#'  Spatial segregation of specialists and generalists in bird communities.
#'  Ecology Letters 9, 1237-–1244
#'  Martín Gonzáles, A.M., Dalsgaard, B. and Olesen, J.M. (2010) Centrality
#'  measures and the importance of generalist species in pollination networks.
#'  Ecological Complexity, 7, 36–43
#'  Opsahl, T. & Panzarasa, P. (2009). Clustering in weighted networks. Social
#'  Networks, 31, 155–163
#'  Poisot, T., Lepennetier, G., Martinez, E., Ramsayer, J., and Hochberg, M.E.
#'  (2011a) Resource availability affects the structure of a natural bacteria-
#'  bacteriophage community. Biology Letters 7,201–204
#'  Poisot, T., Bever, J.D., Nemri, A., Thrall, P.H., and Hochberg, M.E. (2011b)
#'  A conceptual framework for the evolution of ecological specialisation.
#'  Ecology Letters 14, 841–851
#'  Poisot, T., E. Canard, N. Mouquet, and M. E. Hochberg (2012) A comparative
#'  study of ecological specialization estimators. Methods in Ecology and
#'  Evolution 3, 537-–544. doi: 10.1111/j.2041-210X.2011.00174.x.
#'  Vázquez, D. P., Melian, C. J., Williams, N. M., Blüthgen N., Krasnov B. R.
#'  and Poulin, R. (2007) Species abundance and asymmetric interaction strength
#'  in ecological networks. Oikos 116, 1120–1127
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
#' # applying the function to compute closeness centrality (default metric) for
#' lower level
#' lowclos <- specieslevel.spat (rasth, rastl, bipnet, level="lower")
#' plot(lowclos)
#' # calculating closeness, now for higher level (default)
#' allclos <- specieslevel.spat (rasth, rastl, bipnet)
#' plot(allclos)
#' # computing normalised degree for both levels
#' higND <- specieslevel.spat (rasth, rastl, bipnet, index="ND", level="both")
#' plot(higND)
#'}
#' @export
#'
specieslevel.spat <- function(rh, rl, web, index="closeness", level="higher",
                              weighted=F, logbase=exp(1), low.abun=NULL,
                              high.abun=NULL, PDI.normalise=TRUE,
                              PSI.beta=c(1,0), nested.method="NODF",
                              nested.normalised=TRUE, nested.weighted=TRUE,
                              empty.web=TRUE) {
  pw <- prep.web(rh, rl, web)

  if(index=="ALL"){
    stop("You must calculate one species level metrics at a time")
  }

  sph <- names(rh)
  spl <- names(rl)

  web_sub <- web[spl, sph]

  hlyr <- rep(c(T,F), c(length(sph),length(spl)))

  slr <- terra::app(c(rh, rl),
                    sl_vec,
                    web=web_sub, hlyr=hlyr,
                    index=index, level=level, weighted=weighted)
  if(level=="higher"){
    names(slr) <- sph
  } if(level=="lower"){
    names(slr) <- spl
  } else {
    names(slr) <- c(spl, sph)
  }

  return(slr)
}




