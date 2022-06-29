#### spatializing interactions occurrences

library(terra)

datar <- read.table("C:/Users/Cynthia/Desktop/Sites/sites_interact.csv",
                    header = TRUE, sep = ";")
?read.csv
head(datar)


# transforma em matriz
datarm <- as.matrix(datar, xy=T, na.rm = TRUE)
head(datarm)
nrow(datarm)
ncol(datarm)

### Fazendo cálculos com dados espacializados
# usamos o pacote 'terra'
library(terra)
# vamos 'espacializar' nosso conjunto de dados
# cada layer será a pres de uma espécie
options(na.action = "na.fail")

spac_inter <- rast(datarm, type = "xyz")
names(sitesr) <- colnames(datarm[,-c(1,2)])
windows()
plot(sitesr)

# Salvando o raster no seu diretorio
writeRaster(sitesr, "sites_inter.tif", overwrite = TRUE)
