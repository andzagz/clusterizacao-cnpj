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



# RESULTABLE --------------------------------------------------------------

# Inicializar um data frame para armazenar os resultados
resultados <- data.frame(Cluster = 1:num_clusterswss)

# Para cada variável de interesse
for (var in c("Produto", "Regional", "Porte.RFB", "Divisão")) {
  # Inicializar vetores para armazenar as categorias mais frequentes
  categorias_frequentes <- character(num_clusterswss)
  
  # Para cada cluster, calcular a categoria mais frequente
  for (i in 1:num_clusterswss) {
    cluster_indices <- which(kmodes_finalwss$cluster == i)
    cluster_var <- df_receber_cluster[[var]][cluster_indices]
    categorias_frequentes[i] <- names(sort(table(cluster_var), decreasing = TRUE))[1]
  }
  
  # Adicionar as categorias mais frequentes ao data frame de resultados
  resultados[[var]] <- categorias_frequentes
}

# Calcular o tamanho de cada cluster
tamanho_clusters <- table(kmodes_finalwss$cluster)

# Adicionar o tamanho dos clusters ao data frame de resultados
resultados$Tamanho_Cluster <- tamanho_clusters

# Visualizar os resultados
View(resultados)


library(openxlsx)

# Definir o caminho e o nome do arquivo Excel
caminho_arquivo <- "resultados_cluster.xlsx"

# Escrever os resultados para o arquivo Excel
write.xlsx(resultados, caminho_arquivo, rowNames = FALSE)
