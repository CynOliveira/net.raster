#' @title Newman's modularity for each raster cell
#'
#' @description Calculates modules by applying Newman's modularity measure to a
#' weighted bipartite network. The calculation will be made for each raster
#' cell, that is, for each subnetwork formed by the co-occurrence modeled for
#' the pairs of recorded interacting species.
#'
#' @inheritParams sl_vec
#' @inheritParams bipartite::computeModules
#'
#' @return Vector
#'
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @references
#' Rouven Strauss, with fixes by Carsten Dormann and Tobias Hegemann;
#' modified to accommodate Beckett’s algorithm by Carsten Dormann ("bipartite"
#' package)
#' Beckett, S.J. 2016 Improved community detection in weighted
#' bipartite networks. Royal Society open science 3, 140536.
#' Dormann, C. F., and R. Strauß. 2014. Detecting modules in quantitative
#' bipartite networks:the QuanBiMo algorithm. Methods in Ecology & Evolution 5
#' 90–98 (and arXiv q-bio.QM 1304.3218.)
#' Liu X. & Murata T. 2010. An Efficient Algorithm for Optimizing Bipartite
#' Modularity in Bipartite Networks. Journal of Advanced Computational
#' Intelligence and Intelligent Informatics (JACIII) 14408–415.
#' Newman M.E.J. 2004. Physical Review E 70 056131
#' Newman, M.E.J. 2006. Modularity and community structure in networks.
#' Proceedings of the National Academy of Sciences of the United States of
#' America, 103, 8577—8582.
#'
#'
#' @export


computMod_vec <- function(x, web, hlyr, method="Beckett", deep = FALSE,
                          deleteOriginalFiles = TRUE,
                          steps = 1000000, tolerance = 1e-10,
                          experimental = FALSE, forceLPA=FALSE){

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

  if(sum(h.pix, na.rm = T)<=1|sum(l.pix, na.rm = T)<=1)
    return(resu)

  # print(dim(web))
  # print(c(length(l.pix), length(h.pix)))
  web <- web[l.pix,h.pix] #subset of web

  if(sum(web, na.rm = T)==0){
    return(resu)
  }

  # log <- capture.output({ #alternative
  sink(tempfile(), type = "out")
  computMod.pix <- try(suppressWarnings(bipartite::computeModules(web,
                                                method=method,
                                                deep = deep,
                                                deleteOriginalFiles =
                                                deleteOriginalFiles,
                                                steps = steps, tolerance =
                                                tolerance,
                                                experimental = experimental,
                                                forceLPA=forceLPA)), silent = TRUE)
  # })
  sink()

  if(!inherits(computMod.pix, "try-error")){
    resu <- computMod.pix@likelihood #modularity value
  }

  return(resu)
}


#' @title Newman's modularity for raster data
#'
#' @description Calculates modules by applying Newman's modularity measure to a
#' spatial weighted bipartite network. Users must choose between the algorithms
#' of Stephen Beckett (2016) or Dormann & Strauss (2016) (method="DormannStrauss").
#' The default is the Beckett algorithm, which is faster and generally better.
#' View more about it at  \link[bipartite]{computeModules}
#'
#' @inheritParams prep.web
#' @inheritParams terra::app
#'
#' @return Spatraster with the spatial Newman's modularity
#'
#' @details
#' Note that if a network is very small, with few nodes or links, it may be
#' impossible to calculate the metric or calculated result may be unreliable.
#' As the calculation is made with the subnet of each pixel, even in cases
#' like this, it can be possible to visualize the spatialized metric on a
#' macroecological scale.
#'
#' @seealso \code{\link{prep.web}}
#'
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira
#'
#' @references
#' Rouven Strauss, with fixes by Carsten Dormann and Tobias Hegemann;
#' modified to accommodate Beckett’s algorithm by Carsten Dormann ("bipartite"
#' package)
#' Beckett, S.J. 2016 Improved community detection in weighted
#' bipartite networks. Royal Society open science 3, 140536.
#' Dormann, C. F., and R. Strauß. 2014. Detecting modules in quantitative
#' bipartite networks:the QuanBiMo algorithm. Methods in Ecology & Evolution 5
#' 90–98 (and arXiv q-bio.QM 1304.3218.)
#' Liu X. & Murata T. 2010. An Efficient Algorithm for Optimizing Bipartite
#' Modularity in Bipartite Networks. Journal of Advanced Computational
#' Intelligence and Intelligent Informatics (JACIII) 14408–415.
#' Newman M.E.J. 2004. Physical Review E 70 056131
#' Newman, M.E.J. 2006. Modularity and community structure in networks.
#' Proceedings of the National Academy of Sciences of the United States of
#' America, 103, 8577—8582.
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
#' # computing Newman's modularity with faster method, Beckett algorithm (default)
#' compMod <- computeModules.spat (rasth, rastl, bipnet)
#' plot(compMod)
#' # calculating Newman's modularity with alternative method
#' compModDS <- computeModules.spat (rasth, rastl, bipnet, method="DormannStrauss")
#' plot(compModDS)
#'
#'}
#' @export

computeModules.spat <- function(rh, rl, web, method="Beckett", deep = FALSE,
                                deleteOriginalFiles = TRUE,
                                steps = 1000000, tolerance = 1e-10,
                                experimental = FALSE, forceLPA=FALSE) {

  if (deep & method=="Beckett"){
    stop("Beckett cannot currently be used recursively. \n With 'deep=T' please
         use method of 'DormannStrauss'.")}

  pw <- prep.web(rh, rl, web)

  wlr <- terra::app(c(pw$rh, pw$rl),
                    computMod_vec,
                    web=pw$web_sub, hlyr=pw$hlyr,
                    method=method, deep = deep,
                    deleteOriginalFiles = deleteOriginalFiles,
                    steps = steps, tolerance = tolerance, experimental = experimental,
                    forceLPA=forceLPA) #level=level, weighted=weighted


  return(wlr)
}




