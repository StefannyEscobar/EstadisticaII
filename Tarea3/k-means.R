# K- means

Data_costumers <- read.delim("C:/Users/ASUS/Downloads/marketing_campaign.csv")

#Seleccionar variables a utilizar
data <- Data_costumers[, c("Income", "Recency", "MntWines")]

#K-means
# Escalar los datos
scaled_data <- scale(data)
clean_data <- na.omit(scaled_data)
#clean_data <- clean_data[is.finite(clean_data)]

# Determinar el número óptimo de clusters usando el método del codo
wss <- (nrow(clean_data)-1)*sum(apply(clean_data,2,var))
wss <- numeric(length = 10)
for (i in 2:10) wss[i] <- sum(kmeans(clean_data, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

wss_df <- tibble(clusters = 1:10, wss = wss)
scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )

# Realizar el clustering con el número óptimo de clusters
k <- 4
kmeans_result <- kmeans(clean_data, centers = k, nstart = 25)
print(kmeans_result)

# Graficar los resultados
library(cluster)
clusplot(as.matrix(clean_data), kmeans_result$cluster, color = TRUE, shade =TRUE,
         labels = 2, lines=0, cex=.005 )