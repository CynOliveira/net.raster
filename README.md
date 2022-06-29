# renm

<!-- badges: start -->
<!-- badges: end -->

The goal of testPackage is to rasterize network metrics for ecological and macroecological purposes, initially focusing on bipartite networks of mutualistic interactions. By now, it has a function for interaction strength metrics. Input data need to be adjacency matrices, which have one row and one column for each species in the bipartite network. The elements of the matrix can be any number, but in case of binary networks they will be either 0 or 1. A matrix element of 1 (or greater) signals that the respective column species (plants) and row species (frugivores) interact in the network. 

## Installation

You can install the development version of testPackage like so:

``` r
#  install.packages("devtools")
devtools::install_github("CynOliveira/renm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(renm)
## basic example code for interaction strength metrics
net1 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 5, ncol = 5, dimnames = list(c("B1", "B2", "B3", "B4", "B5"), c("P1", "P2", "P3", "P4", "P5")))

net2 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1), nrow = 4, ncol = 4, dimnames = list(c("B1", "B2", "B4", "B6") , c("P2", "P5", "P6", "P7")))

b(net1)
b(net2)

AS(net1)
AS(net2)
```
