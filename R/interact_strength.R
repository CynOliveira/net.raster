#### Calculando a conectância da meta-rede e das redes locais ####


#Função criada para calcular Conectância

#Nossas matrizes originais apresentam r linhas (plantas) e c colunas (aves frugívoras) A conectividade é definida como C = I/(r·c), sendo I o total de interações  , para facilitar nosso trabalho, criamos uma lista de interações (edgelist) em que constam apenas os valores "1". Assim, podemos calcular C igualando I ao número de linhas dessa lista

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



