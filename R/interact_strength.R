#### Calculate interaction strength and its asymmetry for bipartite networks ####

# Input data need to be adjacency matrices, which have one row and one column for each species in the bipartite network. The elements of the matrix can be any number, but in case of binary networks they will be either 0 or 1. A matrix element of 1 (or greater) signals that the respective column species (plants) and row species (frugivores) interact in the network. Zero signals that they do not interact.

## Interaction strenght (b)

b <- function(x, ...){
  bij <- x/rowSums(x)
  return(bij)
}

## Interaction strength asymmetry (AS)
AS <- function(x, ...){
  bij <- x/rowSums(x)
  bji <- x/colSums(x)
  num <- (bij - bji)
  den <- (bij + bji)
  asij <- (num/den)
  return(asij)
}

## Example

net1 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 5, ncol = 5, dimnames = list(c("B1", "B2", "B3", "B4", "B5"), c("P1", "P2", "P3", "P4", "P5")))

net1 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 5, ncol = 5, dimnames = list(c("B1", "B2", "B3", "B4", "B5"), c("P1", "P2", "P3", "P4", "P5")))
print(net1)

net2 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1), nrow = 4, ncol = 4, dimnames = list(c("B1", "B2", "B4", "B6") , c("P2", "P5", "P6", "P7")))
print(net2)


b(net1)
b(net2)

AS(net1)
AS(net2)
