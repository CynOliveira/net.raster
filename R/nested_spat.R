#' Calculate nestedness metrics
#'
#' @description blbabla
#'
#' @param x pixel, a  cell of a grid
#' @param web a bipartite network
#' @inheritParams bipartite::nested
#'
#' @return SpatRaster
#' @export
#'
#' @examples
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



  nested.pix <- try(bipartite::nested(web, method=method, rescale=rescale,
                                      normalised=normalised))

  if(!inherits(nested.pix, "try-error")){
    resu <- nested.pix
  }

  return(resu)
}


#' nested_spat metrics
#' @description
#' blabla
#'
#' @param rh
#' @param rl
#' @param web
#' @inheritParams bipartite::nested
#'
#' @return
#' @export
#'
#' @examples
nested.spat <- function(rh, rl, web, method="weighted NODF", rescale=FALSE,
                        normalised=TRUE) {

  pw <- prep.web(rh, rl, web)

  wlr <- terra::app(c(rh, rl),
                    nested_vec,
                    web=pw$web_sub, hlyr=pw$hlyr,
                    method=method, rescale=rescale) #level=level, weighted=weighted


  return(wlr)
}
