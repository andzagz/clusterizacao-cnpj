# Clusterização -----------------------------------------------------------


# Packages ----------------------------------------------------------------

  

pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


install.packages("clustMixType")
install.packages("fastDummies")
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


# selecting variables and cleaning -------------------------------------------

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


# saving values for standardization ------------------------------------
View(df_receber_cluster)

media_original <- mean(df_receber_cluster$Valor.Contrato.Total)
desvio_padrao_original <- sd(df_receber_cluster$Valor.Contrato.Total)
print(media_original)
print(desvio_padrao_original)


num_vars <- c('Valor.Contrato.Total')
data_cluster[, num_vars] <- scale(data_cluster[, num_vars])
df_receber_cluster[, num_vars] <- scale(df_receber_cluster[, num_vars])


# Cluster -----------------------------------------------------------------

set.seed(123)


  # Elbow Method
wss <- c()
for (i in 1:20) {
  kproto_result <- kproto(data_cluster, k = i, lambda = 1)
  wss[i] <- kproto_result$tot.withinss
}

cores <- rep("blue", length(wss))
cores[6] <- "red"  # Define a cor vermelha para o oitavo cluster


  # Elbow Method Grafic
plot(1:20, wss, type = "b", pch = 19, col = cores, frame = FALSE,  
     xlab = "Número de clusters", ylab = "Within Sum of Squares (WSS)",
     main = "Método do Cotovelo para Escolha do Número de Clusters")



  # Applying the K-prototype function to find the most suitable cluster number using the elbow method.
kproto_result_optimo <- kproto(data_cluster, k = 6, lambda = 1)
df_receber_cluster$Cluster <- as.factor(kproto_result_optimo$cluster)


View(df_receber_cluster)


# Denormalization ----------------------------------------------------------


df_receber_cluster$Valor.Contrato.Total <- 
  (df_receber_cluster$Valor.Contrato.Total * desvio_padrao_original) + media_original


# Function to calculate descriptive statistics for each cluster
cluster_summary <- function(cluster_data, cluster_var, metric_vars, cat_vars) {
  cluster_stats <- cluster_data %>%
    group_by({{cluster_var}}) %>%
    summarise(across(all_of(metric_vars), ~mean(., na.rm = TRUE)))
  
  cat_stats <- cluster_data %>%
    group_by({{cluster_var}}, across(all_of(cat_vars))) %>%
    count() %>%
    group_by({{cluster_var}}) %>%
    top_n(1, n) %>%
    arrange({{cluster_var}}, desc(n))
  
  return(list(cluster_stats = cluster_stats, cat_stats = cat_stats))
}

names(df_receber_cluster)

# Metric and categorical variables for analysis
metric_vars <- c("Valor.Contrato.Total")
cat_vars <- c("Produto", "Regional", "Porte.RFB", "Divisão")




# EFunction Cluster_summary
summary_result <- cluster_summary(df_receber_cluster, Cluster, metric_vars, cat_vars)

# Combine information
result_table <- left_join(summary_result$cluster_stats, summary_result$cat_stats, by = "Cluster")
result_table <- rename(result_table,
                                            "Média do Valor de contrato" = Valor.Contrato.Total)


# Results
View(result_table)

observacoes_por_cluster <- df_receber_cluster %>%
  count(Cluster)


print(observacoes_por_cluster)

result_table <- left_join(result_table, observacoes_por_cluster, by = "Cluster")
result_table <- result_table %>% rename("Tamanho do Cluster" = "n.y")
result_table <- result_table %>% select(-"n.x")
View(result_table)



# write xlsx file
library(openxlsx)

nome_excel <- "result_tableCotovelo.xlsx"


write.xlsx(result_tableCotovelo.xlsx, nome_excel)


print(paste("O arquivo Excel foi criado em:", file.path(getwd(), nome_excel)))


# end -----------------------------------------------


