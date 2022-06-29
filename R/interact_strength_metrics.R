## Interaction strenght (b)

#' This function calculates interaction strength for each specie in the network.
#'
#' @param x an adjacency matrix, which have one row and one column for each species in the bipartite network. The elements of the matrix can be any number, but in case of binary networks they will be either 0 or 1. A matrix element of 1 (or greater) signals that the respective column species (plants) and row species (frugivores) interact in the network.
#' @param ... arguments to be passed to internal functions
#'
#' @return numeric
#' @export
#'
#' @examples
#' #' net1 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1), nrow = 4, ncol = 4, dimnames = list(c("B1", "B2", "B3", "B4", "B5"), c("P1", "P2", "P3", "P4", "P5")))
#' b(net1)
#'
b <- function(x, ...){
  bij <- x/rowSums(x)
  return(bij)
}

#' Interaction strength asymmetry (AS)
#'
#'This function calculates interaction strength asymmetry for each specie in the network.
#'
#' @param x an adjacency matrix, which have one row and one column for each species in the bipartite network. The elements of the matrix can be any number, but in case of binary networks they will be either 0 or 1. A matrix element of 1 (or greater) signals that the respective column species (plants) and row species (frugivores) interact in the network.
#' @param ... arguments to be passed to internal functions
#'
#' @return numeric
#' @export
#'
#' @examples
#' #' net1 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1), nrow = 4, ncol = 4, dimnames = list(c("B1", "B2", "B3", "B4", "B5"), c("P1", "P2", "P3", "P4", "P5")))
#' AS(net1)
#'
AS <- function(x, ...){
  bij <- x/rowSums(x)
  bji <- x/colSums(x)
  num <- (bij - bji)
  den <- (bij + bji)
  asij <- (num/den)
  return(asij)
}


