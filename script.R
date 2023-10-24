library(tidyverse)   # Incluye dplyr, ggplot2, y otras herramientas de manipulación y visualización de datos.
library(cluster)      # Proporciona funciones para realizar análisis de clustering, incluyendo K-Means.
library(factoextra)  # Ayuda a visualizar los resultados del clustering y obtener estadísticas adicionales.
library(NbClust)     # Se utiliza para determinar el número óptimo de clústeres.
library(tidyr)       # Para realizar manipulación de datos, especialmente si deseas realizar tareas de preprocesamiento.

data("iris")
datos<-iris
datos

#Estandarizar datos
datos[ ,1:4]<- scale(datos[,1:4])
datos

#Calcular distancia euclidiana
#se utiliza para determinar cuán "cercanas" o "lejanas" 
#están las observaciones en función de las características que se han estandarizado.
distancia_euclidiana<- dist(datos[ ,1:4])

#clasificar jerarquicamente la informacion de los datos 
#mediante las diastancias euclidianas calculadas
agrupamiento <- hclust(distancia_euclidiana)

#Calculamos los clusters optimos:

#Por método del codo
fviz_nbclust(datos[ ,1:4], hcut, method = "wss")

#Calculo de todos los clusters en conjunto
resnumclust <- NbClust(datos[, 1:4], distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index = "alllong")

#Dado este calculo, se definen 2 clusters

#Agrupas los datos por similitud en k clusters
#cutree realiza el corte del dendrograma en el nivel especificado
#creando grupos o clusters de observaciones.
(kgrupos<- cutree(agrupamiento, k=2))

#Graficamos en un dendograma nustros clusteres
plot(agrupamiento, hang=1, cex=0.5, labels = datos[,5], main="Análisis clúster")
rect.hclust(agrupamiento, k=2,border="green")

fviz_dend(agrupamiento, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))


#Ahora realizamos una interpretación de k-means :D#####################################################################

#Calculamos el número de cluster óptimo:

fviz_nbclust(datos[ ,1:4], kmeans, method = "wss")

resnumclust<-NbClust(datos[ ,1:4], distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")

# Realizar el análisis K-Means con 2 clústeres 
set.seed(123)
kmeans_result <- kmeans(datos[, 1:4], centers = 3, nstart = 25)

# Visualización de los resultados del K-Means
fviz_cluster(kmeans_result, data = datos[, 1:4], geom = "point", stand = FALSE)
fviz_cluster(kmeans_result, data = datos[, 1:4], ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(kmeans_result, data = datos[, 1:4], ellipse.type = "norm")
fviz_cluster(kmeans_result, data = datos[, 1:4], ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())
fviz_cluster(kmeans_result, data = datos[, 1:4], geom = "point", stand = FALSE, label = "auto")


cluster_data <- data.frame(x = datos[, 1], y = datos[, 2], Species = datos[, 5])

# Visualización de los resultados del K-Means con etiquetas
ggplot(cluster_data, aes(x = x, y = y, label = Species)) +
  geom_point(aes(color = as.factor(kmeans_result$cluster))) +
  geom_text() +
  theme_minimal()

