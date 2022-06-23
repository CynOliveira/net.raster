#### Pacotes ####
options(install.lock = FALSE)

install.packages("dplyr")
install.packages("reshape2")
install.packages("bipartite")
install.packages("effects")
install.packages("MuMIn")
install.packages("emmeans")
install.packages("lme4")
install.packages("glmmTMB")
install.packages('TMB', type = 'source')

library(dplyr)
library(reshape2)
library(bipartite)
library(effects)
library(MuMIn)
library(emmeans)
library(lme4)
library(glmmTMB)
library(TMB)

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
head(el.meta) #checando

# Temos nossa edgelist da meta-rede!
View(el.meta)

#acho que aqui tá faltando um passo de write.csv, mas tô lendo a planilha que tinha exportado no próximo comando e tá certo
#RESGATAR COMO eu transformei minha edge list em matriz binária

#### Meta-rede como matriz binária ponderada ####

Metaweb <- read.table("C:/Users/Cynthia/Desktop/Outputs/Metaweb.csv", head=T,row.names=1, sep=",")
View(Metaweb)
str(Metaweb)

#transformar a edge list da metaweb em um data.frame bipartido (metaweb_bip) para as análises do bipartite
#acho que tinha usado com acast

# agora transformar isso em matriz binária

#write.csv(metaweb_bip,"C:/Users/Cynthia/Desktop/Outputs/metaweb_bip.csv")
metaweb_bip <- read.table("C:/Users/Cynthia/Desktop/Outputs/metaweb_bip.csv", head=T,row.names=1, sep=",")
View(metaweb_bip) #cheia de NA
#write.csv(metaweb_bip, file="metawebbip2.csv", na=" ", row.names=FALSE, quote=FALSE) #gerou
#levei pra pasta de Outputs (único jeito que funcionou)
#metaweb_bip2 <- read.table("C:/Users/Cynthia/Desktop/Outputs/metaweb_bip.csv", head=T,row.names=1, sep=",")

#View(metaweb_bip2) #segue cheia de NA

#### Calculando a conectância da meta-rede e das redes locais ####
#Função criada para calcular Conectância
#Nossas matrizes originais apresentam r linhas (plantas) e c colunas (aves frugívoras) A conectividade é definida como C = I/(r·c), sendo I o total de interações realizadas.Porém, para facilitar nosso trabalho, criamos uma lista de interações (edgelist) em que constam apenas os valores "1". Assim, podemos calcular C igualando I ao número de linhas dessa lista

Connec <- function(el){
  N_i <- nrow(el) # Total de interações é o próprio número de linhas da lista criada
  N_birds <- length(unique(el[, 2])) # Número de espécies de aves
  N_plants <- length(unique(el[, 1])) # Número de espécies de plantas
  Conn <- N_i / (N_birds * N_plants) # Calculando a Conectância
  return(Conn) # Retornando o resultado da Conectância
}

Meta.Con <- Connec(el = el.meta) #calculando conectância para a meta-rede

Meta.Con

# calculando a conectância para as redes locais

Loc.Con <- vector(length = 25) #substituir pelo número de redes
for(ee in 1:length(el.list)){
  Loc.Con[ee] <- Connec(el.list[[ee]])
}

Loc.Con
