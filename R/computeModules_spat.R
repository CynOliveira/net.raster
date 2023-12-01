#' @title ComputeModules_spat
#'
#' @description Calculates modules by applying Newman's modularity measure to a
#' weighted bipartite network. The calculation will be made for each raster
#' cell, that is, for each subnetwork formed by the co-occurrence modeled for
#' the pairs of interacting species.
#'
#' @param x A SpatRaster containing presence-absence data (0 or 1)
#' for a set of species. The layers (species) must be sorted according to the
#' tree order.
#' @param hlyr
#' @param web A bipartite weighted graph
#' @inheritParams bipartite::computeModules
#'
#' @return SpatRaster
#' @export
#'
#' @examples
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira
#' @references Rouven Strauss, with fixes by Carsten Dormann and Tobias Hegemann;
#'  modified to accommodate Beckett’s algorithm by Carsten Dormann


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

  if(sum(h.pix, na.rm = T)==0|sum(l.pix, na.rm = T)==0)
    return(resu)

  # print(dim(web))
  # print(c(length(l.pix), length(h.pix)))
  web <- web[l.pix,h.pix]

  if(sum(web, na.rm = T)==0){
    return(resu)
  }



  computMod.pix <- try(bipartite::computeModules(web,
                                                method=method,
                                                deep = deep,
                                                deleteOriginalFiles =
                                                deleteOriginalFiles,
                                                steps = steps, tolerance =
                                                tolerance,
                                                experimental = experimental,
                                                forceLPA=forceLPA))

  if(!inherits(computMod.pix, "try-error")){
    resu <- computMod.pix@likelihood
  }

  return(resu)
}


#' computeModules for spatial data
#'
#'This function takes a bipartite weighted graph and computes modules by
#'applying Newman's modularity measure in a bipartite weighted version to it.
#' metaComputeModules re-runs the algorithm several times, returning the most
#' modular result, to stabilise modularity computation.
#'
#' @param rh
#' @param rl
#' @param web
#'
#' @return
#' @export
#'
#' @examples
#' @author

computeModules.spat <- function(rh, rl, web, method="Beckett", deep = FALSE,
                                deleteOriginalFiles = TRUE,
                                steps = 1000000, tolerance = 1e-10,
                                experimental = FALSE, forceLPA=FALSE) {

  pw <- prep.web(rh, rl, web)

  wlr <- terra::app(c(rh, rl),
                    computMod_vec,
                    web=pw$web_sub, hlyr=pw$hlyr,
                    method=method, deep = deep,
                    deleteOriginalFiles = deleteOriginalFiles,
                    steps = steps, tolerance = tolerance, experimental = experimental,
                    forceLPA=forceLPA) #level=level, weighted=weighted


  return(wlr)
}




