#' @title prep_web
#'
#' @description Checks whether the species names of each Spatraster (higher and lower level)
#' match the species names in the network.
#'
#'
#' @param rh SpatRaster of higher level species in the network
#' @param rl SpatRaster of lower level species in the network
#' @param web A bipartite network, which can be weighted or not
#'
#'
#' @return A vector (?) with the species names of higher and lower levels
#' @export
#'
#' @examples
#' @author Neander Marcel Heming and Cynthia Valéria Oliveira

prep.web <- function(rh, rl, web) {
  sph <- names(rh)
  spl <- names(rl)

  ch <- sum(sph %in% colnames(web))
  cl <- sum(spl %in% rownames(web))

  if(ch==0 | cl==0){
    stop("The species names in web do not match with lower level or higher level")
  }

  web_sub <- web[spl, sph]
  # print(web_sub)

  hlyr <- rep(c(T,F), c(length(sph),length(spl)))
  return(list(web_sub=web_sub, hlyr=hlyr))


}

