# Clusterização -----------------------------------------------------------

library(cluster)
library(readxl)
library(klaR)
library(clustMixType)
library(dplyr)
library(factoextra)

tabela_completav2 <- data.frame(read_excel("tabela_completav2.xlsx"))

# summary((tabela_completav2))
# str(tabela_completav2)

# transformação as.factor -------------------------------------------------


colunas_para_transformar <- 1:16 

tabela_completav2[, colunas_para_transformar] <- lapply(tabela_completav2[, colunas_para_transformar], as.factor)

colunas_para_transformar <- 20:46

tabela_completav2[, colunas_para_transformar] <- lapply(tabela_completav2[, colunas_para_transformar], as.factor)


# Variaveis para clusterizzação -------------------------------------------

str(tabela_completav2)


col_processada <- c("Valor.Contrato.Total", "Produto", "Regional",
                    # "Filial", "Situação",
                    "Porte.RFB", #"SC.Competitiva",  
                    "Divisão")
#, "Grupo", "Classe", "Subclasse")

df_receber_cluster <- c("CNPJ","Razão.Social", "Valor.Contrato.Total", "Produto", "Regional",
                        # "Filial", "Situação",
                        "Porte.RFB", #"SC.Competitiva",  
                        "Divisão")



df_receber_cluster <- tabela_completav2[, df_receber_cluster]
data_cluster <- tabela_completav2[, col_processada]

any(is.na(data_cluster))
any(is.na(df_receber_cluster))

data_cluster <- na.omit(data_cluster)
df_receber_cluster <- na.omit(df_receber_cluster)

names(data_cluster)
boxplot(data_cluster)
# media e desvio padrao da var metrica ------------------------------------
# View(df_receber_cluster)

media_original <- mean(df_receber_cluster$Valor.Contrato.Total)
desvio_padrao_original <- sd(df_receber_cluster$Valor.Contrato.Total)
print(media_original)
print(desvio_padrao_original)


num_vars <- c('Valor.Contrato.Total')
data_cluster[, num_vars] <- scale(data_cluster[, num_vars])
df_receber_cluster[, num_vars] <- scale(df_receber_cluster[, num_vars])

data_cluster <- select(data_cluster, -Valor.Contrato.Total)

# Clusterv5 -----------------------------------------------------------------

set.seed(123)
# Encontre o número ideal de clusters usando o método do cotovelo
# Função para calcular a inércia
calcular_inercia <- function(data, clusters) {
  inercia <- numeric(length(clusters))
  for (k in clusters) {
    kmodes_model <- kmodes(data, k)
    cluster_sizes <- table(kmodes_model$cluster)
    inercia[k] <- sum(sapply(1:k, function(i) {
      cluster <- data[kmodes_model$cluster == i, ]
      sum(cluster_sizes[i] * colSums(cluster != t(data)))
    }))
  }
  return(inercia)
}

# Calcule o número ideal de clusters usando o método do cotovelo
clusters_testados <- 1:10  # Supondo que estamos testando até 10 clusters
inercia <- calcular_inercia(data_cluster, clusters_testados)

plot(clusters_testados, inercia, type = "b", xlab = "Número de Clusters", ylab = "Inércia", main = "Método do Cotovelo")


num_clusterswss <- 7
kmodes_finalwss <- kmodes(data_cluster, num_clusterswss)

# Exiba os clusters atribuídos a cada observação


names(data_cluster)


# last dance --------------------------------------------------------------
install.packages("clusterCrit")
library(clusterCrit)

# Função para calcular a distância média intra-cluster para dados categóricos
distancia_media_intra_cluster <- function(data, cluster) {
  num_clusters <- length(unique(cluster))
  distancia_intra_cluster <- numeric(num_clusters)
  for (i in 1:num_clusters) {
    cluster_i <- data[cluster == i, ]
    # Calcular a matriz de distância de Hamming entre os membros do cluster e o primeiro membro
    distancia_hamming <- apply(cluster_i, 1, function(x) sum(x != cluster_i[1, ]))
    # Calcular a distância média dentro do cluster
    distancia_media <- mean(distancia_hamming)
    distancia_intra_cluster[i] <- distancia_media
  }
  return(distancia_intra_cluster)
}

# Função para calcular o índice Davies-Bouldin para dados categóricos
indice_davies_bouldin <- function(data, cluster) {
  num_clusters <- length(unique(cluster))
  distancia_intra_cluster <- distancia_media_intra_cluster(data, cluster)
  db_index <- numeric(num_clusters)
  for (i in 1:num_clusters) {
    max_db <- -Inf
    for (j in 1:num_clusters) {
      if (j != i) {
        db <- (distancia_intra_cluster[i] + distancia_intra_cluster[j]) / max(distancia_intra_cluster[i], distancia_intra_cluster[j])
        if (db > max_db) {
          max_db <- db
        }
      }
    }
    db_index[i] <- max_db
  }
  db_index <- mean(db_index)
  return(db_index)
}

# Calcular o índice Davies-Bouldin para seus clusters categóricos
db_index <- indice_davies_bouldin(data_cluster, kmodes_finalwss$cluster)

print(db_index)


# Vetor para armazenar os valores do índice Davies-Bouldin
db_indices <- numeric(10)

# Calcular o índice Davies-Bouldin para cada número de clusters
for (k in 1:10) {
  # Executar o algoritmo kmodes com k clusters
  kmodes_model <- kmodes(data_cluster, k)
  # Calcular o índice Davies-Bouldin
  db_indices[k] <- indice_davies_bouldin(data_cluster, kmodes_model$cluster)
}

cores <- rep("blue", length(inercia))
cores[5] <- "red"  # Define a cor vermelha para o oitavo cluster

plot(1:10, db_indices, type = "b", pch = 19, col = cores, frame = FALSE, xlab = "Número de Clusters", ylab = "Índice Davies-Bouldin", main = "Índice Davies-Bouldin vs. Número de Clusters")



