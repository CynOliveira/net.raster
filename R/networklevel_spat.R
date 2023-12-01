#' networklevel_spat
#'
#' @description  blablabla
#'
#' @inheritParams bipartite::networklevel
#' @param x - pixel, a  cell of a grid
#' @param web - a bipartite network
#'
#'
#' @return SpatRaster
#' @export
#'
#' @examples
nl_vec <- function(x, web, hlyr, index="ALLBUTDD", level="both", weighted=T,
                   ISAmethod="Bluethgen",  SAmethod = "Bluethgen",
                   extinctmethod = "r", nrep = 100, CCfun="median", dist="horn",
                   normalise=T, empty.web=T, logbase="e", intereven="prod",
                   H2_integer=T, fcweighted=T, fcdist="euclidean", legacy=F){

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

  networklevel.pix <- try(bipartite::networklevel(web,
                                                  index=index, level=level,
                                                  weighted=weighted,
                                                  ISAmethod=ISAmethod,
                                                  SAmethod=SAmethod ,
                                                  extinctmethod=extinctmethod,
                                                  nrep=nrep, CCfun=CCfun,
                                                  dist=dist,normalise=normalise,
                                                  empty.web=empty.web,
                                                  logbase=logbase,
                                                  intereven=intereven,
                                                  H2_integer=H2_integer,
                                                  fcweighted=fcweighted,
                                                  fcdist=fcdist, legacy=legacy))

  if(!inherits(networklevel.pix, "try-error")){
    resu <- networklevel.pix
  }

  return(resu)
}


#' networklevel_spat metrics
#' @description
#' rastrerize network level metrics
#'
#' @param rh
#' @param rl
#' @param web
#' @inheritParams bipartite::networklevel
#' @return
#' @export
#'
#' @examples
networklevel.spat <- function(rh, rl, web,
                              index="ALLBUTDD", level="both", weighted=T,
                              ISAmethod="Bluethgen",  SAmethod = "Bluethgen",
                              extinctmethod = "r", nrep = 100, CCfun="median",
                              dist="horn", normalise=T, empty.web=T,
                              logbase="e", intereven="prod",H2_integer=T,
                              fcweighted=T, fcdist="euclidean", legacy=F) {

  pw <- prep.web(rh, rl, web)

  nlr <- terra::app(c(rh, rl),
                    nl_vec,
                    web=pw$web_sub, hlyr=pw$hlyr,
                    index=index, level=level,
                    weighted=weighted, ISAmethod=ISAmethod, SAmethod=SAmethod,
                    extinctmethod=extinctmethod, nrep=nrep, CCfun=CCfun,
                    dist=dist,normalise=normalise, empty.web=empty.web,
                    logbase=logbase, intereven=intereven, H2_integer=H2_integer,
                    fcweighted=fcweighted, fcdist=fcdist, legacy=legacy)


  return(nlr)
}
