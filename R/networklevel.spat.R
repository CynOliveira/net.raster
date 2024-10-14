#' @title Several network indices at the network level for each raster cell
#'
#' @description Calculate various indices and values for the bipartite subnetwork
#' of each pixel, formed by the co-occurrence modeled for the pairs of recorded
#' interacting species.
#'
#' @inheritParams sl_vec
#' @inheritParams bipartite::networklevel
#'
#' @return Vector
#'
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @export
#'
nl_vec <- function(x, web, hlyr, index="connectance", level="both", #weighted=F,
                   ISAmethod="Bluethgen",  SAmethod = "Bluethgen",
                   extinctmethod = "r", nrep = 100, CCfun="median", dist="horn",
                   normalise=T, empty.web=T, logbase="e", intereven="prod",
                   H2_integer=T, #fcweighted=F,
                   fcdist="euclidean", legacy=F){

  weighted = F
  fcweighted = F

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

  networklevel.pix <- try(suppressWarnings(bipartite::networklevel(web,
                                                  index=index, level=level,
                                                  #weighted=weighted,
                                                  ISAmethod=ISAmethod,
                                                  SAmethod=SAmethod ,
                                                  extinctmethod=extinctmethod,
                                                  nrep=nrep, CCfun=CCfun,
                                                  dist=dist,normalise=normalise,
                                                  empty.web=empty.web,
                                                  logbase=logbase,
                                                  intereven=intereven,
                                                  H2_integer=H2_integer,
                                                  #fcweighted=fcweighted,
                                                  fcdist=fcdist, legacy=legacy)))

  if(!inherits(networklevel.pix, "try-error")){
    resu <- networklevel.pix
  }

  return(resu)
}


#' @title Several network indices at the network level for raster data.
#'
#' @description Calculate various indices and values for bipartite networks on
#' raster data. You can see the spatial variation of important metrics such as
#' connectance, web asymmetry, network specialization (H2) and two options to
#' compute nestedness. View all available indexes in Details. Note that
#' net.raster only allows unweighted calculation of bipartite network metrics.
#'
#' @inheritParams prep.web
#' @inheritParams nl_vec
#' @inheritParams terra::app
#'
#' @return Spatraster with the choose network level metric
#'
#' @details
#' Note that if a network is very small, with few nodes or links, it may be
#' impossible to calculate the choose metric or calculated result may be
#' unreliable. As the calculation is made with the subnet of each pixel, even in
#' cases like this, it is possible to visualize the spatialized metric on a
#' macroecological scale. Users may select one index at a time for time-efficient
#' spatial calculation, being the default the "connectance". Also note that some
#' indices may have a high processing time for the entire raster, depending on
#' their algorithms and the processing capacity and RAM of the machine used.
#' The current available indices for network level metrics are listed bellow and
#' users can find more information about them at \link[bipartite]{networklevel}.
#'
#' There are metrics computed for the entire network:
#' • ‘connectance’,
#' • ‘web asymmetry’,
#' • ‘links per species’,
#' • ‘number of compartments’,
#' • ‘compartment diversity’,
#' • ‘cluster coefficient’,
#' • ‘nestedness’
#' • ‘NODF’,
#' • ‘ISA’ (or alternatively ‘interaction strength asymmetry’ or ‘dependence
#' asymmetry’),
#' • ‘SA’ (or alternatively ‘specialisation asymmetry’),
#' • ‘linkage density’,
#' • ‘Fisher alpha’,
#' • ‘interaction evenness’,
#' • ‘Alatalo interaction evenness’,
#' • ‘Shannon diversity’,
#' • ‘H2’ (network specialization);
#'
#' and/or those metrics invoked through "grouplevel":
#' • ‘number of species’ in the respective trophic level,
#' • ‘mean number of links’,
#' • ‘mean number of shared partners’,
#' • ‘degree distribution’,
#' • ‘togetherness’,
#' • ‘C score’,
#' • ‘V ratio’,
#' • ‘discrepancy’,
#' • ‘extinction slope’,
#' • ‘robustness’,
#' • ‘niche overlap’,
#' • ‘generality’,
#' • ‘vulnerability’,
#' • ‘fc’ (or alternatively ‘functional complementarity’).
#'
#' @seealso \code{\link{prep.web}}, \code{\link{nested.spat}},
#'  \code{\link{computeModules.spat}}
#'
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @references
#'  Carsten F. Dormann
#'  Almeida-Neto, M., Loyola, R.D., Ulrich, W., Guimaraes, P., Guimaraes, Jr.,
#'  P.R. 2008. A consistent metric for nestedness analysis in ecological systems:
#'  reconciling concept and measurement. Oikos 117, 1227–1239
#'  Almeida-Neto, M. & Ulrich, W. (2011) A straightforward computational approach
#'  for measuring nestedness using quantitative matrices. Environmental Modelling
#'  & Software 26, 173–178
#'  Bascompte, J., Jordano, P. and Olesen, J. M. 2006 Asymmetric coevolutionary
#'  networks facilitate biodiversity maintenance. Science 312, 431–433
#'  Bersier, L. F., Banasek-Richter, C. and Cattin, M. F. (2002) Quantitative
#'  descriptors of food-web matrices. Ecology 83, 2394–2407
#'  Blüthgen, N. (2010) Why network analysis is often disconnected from community
#'  ecology: A critique and an ecologist’s guide. Basic and Applied Ecology 11,
#'  185–195
#'  Blüthgen, N., Menzel, F., Hovestadt, T., Fiala, B. and Blüthgen N. 2007
#'  Specialization, constraints and conflicting interests in mutualistic networks.
#'  Current Biology 17, 1–6
#'  Burgos, E., H. Ceva, R.P.J. Perazzo, M. Devoto, D. Medan, M. Zimmermann, and
#'  A. Maria Delbue (2007) Why nestedness in mutualistic networks? Journal of
#'  Theoretical Biology 249, 307–313
#'  Corso G, de Araújo AIL, de Almeida AM (2008) A new nestedness estimator in
#'  community networks.
#'  Devoto M., Bailey S., Craze P., and Memmott J. (2012) Understanding and
#'  planning ecological restoration of plant-pollinator networks. Ecology Letters
#'  15, 319–328.
#'  Dormann, C.F., Fründ, J., Blüthgen, N., and Gruber, B. (2009) Indices, graphs
#'  and null models: analysing bipartite ecological networks. The Open Ecology
#'  Journal 2, 7–24.
#'  Dunne, J. A., R. J. Williams, and N. D. Martinez. 2002 Food-web structure
#'  and network theory: the role of connectance and size. Proceedings of the
#'  National Academy of Science USA 99, 12917–12922
#'  Gotelli, N. J., and G. R. Graves. 1996 Null Models in Ecology. Smithsonian
#'  Institution Press, Washington D.C.
#'  Krebs, C. J. 1989. Ecological Methodology. Harper Collins, New York.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(bipartite)
#' # load bipartite network and the raster stacks of higher level and lower level
#' species
#' bipnet <- read.csv(system.file("extdata", "bipnet.csv",
#' package="net.raster"), row.names=1, sep= ";" ) #change separator if necessary
#' rasth <- rast(system.file("extdata", "rasth.tif",
#' package="net.raster"))
#' rastl <- rast(system.file("extdata", "rastl.tif",
#' package="net.raster"))
#' # applying the function to compute connectance (default) for both levels
#' (default), that is, the entire network
#' conn <- networklevel.spat (rasth, rastl, bipnet)
#' plot(conn)
#' # calculating  H2 for he entire network
#' webh2 <- networklevel.spat (rasth, rastl, bipnet, index="H2")
#' plot(webh2)
#' # computing degree distribution for one group, the lower level
#' lowdd <- networklevel.spat (rasth, rastl, bipnet, index="degree distribution",
#' level="lower")
#' plot(lowdd)
#'}
#' @export
#'
networklevel.spat <- function(rh, rl, web,
                              index="connectance", level="both", #weighted=F,
                              ISAmethod="Bluethgen",  SAmethod = "Bluethgen",
                              extinctmethod = "r", nrep = 100, CCfun="median",
                              dist="horn", normalise=T, empty.web=T,
                              logbase="e", intereven="prod",H2_integer=T,
                              #fcweighted=T,
                              fcdist="euclidean", legacy=F) {

  pw <- prep.web(rh, rl, web)

  if(index %in% c("ALL", "ALLBUTDD", "info", "binary", "quantitative", "‘topology")){
    stop("You must calculate one network level metrics at a time")
  }

  nlr <- terra::app(c(pw$rh, pw$rl),
                    nl_vec,
                    web=pw$web_sub, hlyr=pw$hlyr,
                    index=index, level=level,
                    #weighted=weighted,
                    ISAmethod=ISAmethod, SAmethod=SAmethod,
                    extinctmethod=extinctmethod, nrep=nrep, CCfun=CCfun,
                    dist=dist,normalise=normalise, empty.web=empty.web,
                    logbase=logbase, intereven=intereven, H2_integer=H2_integer,
                    #fcweighted=fcweighted,
                    fcdist=fcdist, legacy=legacy)


  return(nlr)
}
