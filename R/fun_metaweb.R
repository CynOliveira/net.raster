#### Pacotes ####

install.packages("dplyr")
install.packages("reshape2")
install.packages("bipartite")
install.packages("effects")
install.packages("MuMIn")
install.packages("emmeans")
install.packages("performance")


library(dplyr)
library(reshape2)
library(bipartite)
library(effects)
library(MuMIn)
library(emmeans)
library(performance) # ver se mantém

#### Criando a edge-list da meta-rede ####

# Acessamos as planilhas individuais em formato csv, reunidas em uma pasta em que estão apenas elas de arquivos nesse formato

path <- "C:/Users/Cynthia/Desktop/Inputs" #aqui, mude para seu diretório

Allfiles <- list.files(path=path, pattern="*.csv")

Allfiles

# Criamos então uma lista com as matrizes de interação, com auxílio da função lapply

im_list <- lapply(Allfiles, function(x) read.csv(paste (path, x, sep = "/"), sep = ","))

View(im_list) # Checando a lista criada

im_list[[1]] # Checando um dos elementos (uma das matrizes) na minha lista



# Agora, precisamos transformar as matrizes de interação em edgelists, com auxílio da função melt do pacote reshape2. Isso é importante para criarmos uma tabela e facilitar o procedimento, sendo usual nos trabalhos com redes. Também usamos a função sapply, adequada para listas. Ao final desse pass, por meio da indexação, teremos na edgelist apenas os valores "1".

el.list <- sapply(im_list, function(x) melt(x), simplify = F)
for(cc in 1:length(el.list)){
  colnames(el.list[[cc]]) <- c("Plant", "Bird", "Weight") # Mudando os nomes das colunas
  el.list[[cc]] <- subset(el.list[[cc]], el.list[[cc]]$Weight > 0) # Filtrando para as interacoes observadas
}

# Agora, agrupamos então todas as edgelists em uma única meta-lista com auxilio do rbind

el.meta <- el.list[[1]]
for(ee in 2:length(el.list)){
  el.meta <- rbind(el.meta, el.list[[ee]])
}

# Precisamos ainda agrupar os pares de interações que se repetem nas diferentes redes locais observadas. Fazemos isso com auxílio do aggregate, agrupando a coluna "Weigth" com base nas aves e plantas

el.meta <- aggregate.data.frame(x = el.meta$Weight, by = list(el.meta$Plant, el.meta$Bird), FUN = length)

# Esse procedimento requer que a gente nomeie as colunas outra vez (como estavam antes)
colnames(el.meta) <- c("Plant", "Bird", "Weight")

# Temos nossa edgelist da meta-rede!
View(el.meta)

#### Meta-rede como matriz binária ####

Metaweb <- read.table("C:/Users/Cynthia/Desktop/Outputs/Metaweb.csv", head=T,row.names=1, sep=",")
str(Metaweb)

# transformar esse dataframe bipartido em matrix esparsa: realmente necessário?
#"A sparse matrix is a type of matrix that has most of the elements equal to zero but there is no restriction for the number of zero elements. As a general criterion the number of non−zero elements are expected to be equal to the number of rows or number of columns"


write.csv(metaweb_bip,"C:/Users/Cynthia/Desktop/Outputs/metaweb_bip.csv")

metaweb_bip <- read.table("C:/Users/Cynthia/Desktop/Outputs/metaweb_bip.csv", head=T,row.names=1, sep=",")


#transformar a edge list da metaweb em um data.frame bipartido (metaweb_bp) para as análises do bipartite - usando função do reshape2

metaweb_bip <- acast(Metaweb, Plant ~ Bird, value.var = 'Weight')
View(metaweb_bip)

## fonte: https://stackoverflow.com/questions/62578927/convert-data-frame-edgelist-into-matrix-while-keeping-row-and-column-names



