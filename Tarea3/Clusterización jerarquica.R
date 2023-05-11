#Segmentación de cliente
#Datos

#Cargar datos
Data_costumers <- read.delim("C:/Users/ASUS/Downloads/marketing_campaign.csv")

#Seleccionar variables a utilizar
data <- Data_costumers[, c("Income", "Recency", "MntWines")]
#Escalar datos
data_scaled <- scale(data)


#Clusterización jerarquica aglomerativa
hc <- hclust(dist(data_scaled), method = "ward.D2")

plot(hc)

clusters <- cutree(hc, k = 3)
library(ggplot2)

#Scatter plot
data_clustered <- cbind(data, clusters)
ggplot(data_clustered, aes(x = Income, y = MntWines, color = factor(clusters))) +
  geom_point() +
  labs(title = "Segmentación de clientes") +
  theme_minimal()



