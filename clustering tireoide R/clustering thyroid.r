#Extraindo colunas relevantes (sem a coluna que nos diz a classe de tireóidismo: 1 = normal, 2 = hyper, 3 = hypo)
dados_clustering <- new.thyroid[, (1:5)]
 
#Kmeans com 3 clusters
kmeans_result <- kmeans(dados_clustering, centers = 3, nstart = 10)
 
# Adicionando rótulos de cluster para a tabela original
new.thyroid$cluster <- kmeans_result$cluster

# Criando tabela de contingência
confusion_table <- table(new.thyroid$thyroidism, new.thyroid$cluster)

# Extraindo os valores reais e do modelo de cluster
val_reais <- colSums(confusion_table)
val_modelo <- rowSums(confusion_table)

# Calculando a porcentagem de classificações corretas
accuracy <- sum(diag(confusion_table)) / sum(confusion_table) * 100

# Imprimindo a tabela de contingência
print("Tabela de Contingência:")
print(confusion_table)

# Imprimindo os valores reais e do modelo
print("Valores Reais:")
print(val_reais)
print("Valores Previstos pelo Modelo de Cluster:")
print(val_modelo)

# Imprimindo a porcentagem de classificações corretas
cat("Porcentagem de Classificações Corretas:", accuracy, "%\n")

