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

