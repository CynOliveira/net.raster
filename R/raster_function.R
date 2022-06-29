###### Input data need to be adjacency matrices, which have one row and one column for each species in the bipartite network. The elements of the matrix can be any number, but in case of binary networks they will be either 0 or 1. A matrix element of 1 (or greater) signals that the respective column species (plants) and row species (frugivores) interact in the network.

#### testando a função com os dados do Rosauer
library(terra)

datar <- read.csv("D:/PERFIL/Documents/phylogrid_package/phylospatial-master_script/PhyloEndemism_in_R/Tree Frog Data/sites_x_tips.csv",
                  header = TRUE, sep = ";")
head(datar)

# transforma em matriz
datarm <- as.matrix(datar)
head(datarm)
nrow(datarm)
ncol(datarm)

### Fazendo cálculos com dados espacializados
# usamos o pacote 'terra'
library(terra)
# vamos 'espacializar' nosso conjunto de dados
# cada layer será a pres de uma espécie

sitesr <- rast(datarm, type = "xyz")
names(sitesr) <- colnames(datarm[,-c(1,2)])
windows()
plot(sitesr)

# Salvando o raster no seu diretorio
writeRaster(sitesr, "raster_treefrogs_rosauer.tif", overwrite = TRUE)
