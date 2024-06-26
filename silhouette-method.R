library(cluster)
library(readxl)
library(klaR)
library(clustMixType)
library(dplyr)
library(factoextra)

# Carregar a planilha
tabela_completav2 <- data.frame(read_excel("tabela_completav2.xlsx"))

# Transformar variáveis categóricas em fatores
colunas_para_transformar <- c(1:16, 20:46)
tabela_completav2[, colunas_para_transformar] <- lapply(tabela_completav2[, colunas_para_transformar], as.factor)

# Selecionar variáveis para clusterização
col_processada <- c("Valor.Contrato.Total", "Produto", "Regional",
                    "Porte.RFB", "Divisão")

df_receber_cluster <- c("CNPJ","Razão.Social", "Valor.Contrato.Total", "Produto", "Regional",
                        "Porte.RFB", "Divisão")

df_receber_cluster <- tabela_completav2[, df_receber_cluster]
data_cluster <- tabela_completav2[, col_processada]

# Remover valores ausentes
data_cluster <- na.omit(data_cluster)
df_receber_cluster <- na.omit(df_receber_cluster)

# Normalizar variável numérica
media_original <- mean(df_receber_cluster$Valor.Contrato.Total)
desvio_padrao_original <- sd(df_receber_cluster$Valor.Contrato.Total)

num_vars <- c('Valor.Contrato.Total')
data_cluster[, num_vars] <- scale(data_cluster[, num_vars])
df_receber_cluster[, num_vars] <- scale(df_receber_cluster[, num_vars])

# Definir semente aleatória
set.seed(123)

# Cálculo do coeficiente de silhueta para diferentes números de clusters
sil <- vector("list", 20)
for (i in 1:20) {
  sil[[i]] <- silhouette(kproto(data_cluster, k = i, lambda = 1))$sil
}

# Encontrar o número ideal de clusters com base na silhueta média
medias_sil <- sapply(sil, mean)
k_ideal <- which.max(medias_sil)

# Visualizar a silhueta para cada ponto
ggplot(data.frame(sil = unlist(sil[[k_ideal]])), aes(x = sil)) +
  geom_boxplot() +
  labs(x = "Silhueta")

# Visualizar a silhueta para cada cluster
ggplot(data.frame(sil = unlist(sil[[k_ideal]]), cluster = kproto(data_cluster, k = k_ideal, lambda = 1)$cluster) +
         geom_point(aes(x = sil, y = cluster, color = cluster)) +
         labs(x = "Silhueta", y = "Cluster")
       
       # Atribuir o cluster a cada ponto
       df_receber_cluster$cluster <- kproto(data_cluster, k = k_ideal, lambda = 1)$cluster
       
       # Salvar resultados
       write.csv(df_receber_cluster, "df_receber_cluster_silhueta.csv")
       
       