#' @title specieslevel_spat
#' @description
#' A short description...
#'
#'
#' @param
#' @inheritParams bipartite::specieslevel
#' @inheritParams
#'
#' @return
#' @export
#'
#' @examples
#' @author

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
  metaweb_clos.pix <- try(bipartite::specieslevel(web,
                                                  index=index, level=level))

  if(!inherits(metaweb_clos.pix, "try-error")){
    # return(metaweb_clos.pix)
    if(level=="higher"){
      # print(metaweb_clos.pix)
      spp.res <- which(h.pix)[colnames(web) %in% rownames(metaweb_clos.pix)]
      resu[spp.res] <- metaweb_clos.pix[,ifelse(weighted,2,1)]
    } else {
      spp.res <- which(l.pix)[rownames(web) %in% rownames(metaweb_clos.pix)]
      resu[spp.res] <- metaweb_clos.pix[,ifelse(weighted,2,1)]
    }
  }
  return(resu)
}


#' Calculate various indices for network properties at the species level
#' for spatial data
#'
#' Apart from the properties of the entire web, also its participants can be
#' described specifically. Various simple numbers and indices are calculated
#' and returned.
#'
#' @param rh
#' @param rl
#' @inheritParams sl_vec
#'
#' @return
#' @export
#'
#' @examples
#' @author

specieslevel.spat <- function(rh, rl, web, index="closeness", level="higher",
                              weighted=F, logbase=exp(1), low.abun=NULL,
                              high.abun=NULL, PDI.normalise=TRUE,
                              PSI.beta=c(1,0), nested.method="NODF",
                              nested.normalised=TRUE, nested.weighted=TRUE,
                              empty.web=TRUE) {

  if(index=="ALL"){
    stop("You must calculate one species level metrics at a time")
  }

  sph <- names(rh)
  spl <- names(rl)

  web_sub <- web[spl, sph]
  #print(web_sub)
  hlyr <- rep(c(T,F), c(length(sph),length(spl)))

  slr <- terra::app(c(rh, rl),
                    sl_vec,
                    web=web_sub, hlyr=hlyr,
                    index=index, level=level, weighted=weighted)
  if(level=="higher"){
    names(slr) <- sph
  } else {
    names(slr) <- spl
  }

  return(slr)
}

