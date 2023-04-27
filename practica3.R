install.packages("dplyr")
install.packages("lubridate")
install.packages("stringr")
library(tidyr)
library(stringr)
library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(readr)
library(stringr)
library(plotrix)
library(ggplot2)


setwd("lab1")

#cargar datos (editar para la carga local de datos)
epa_http <- read_table("epa-http.csv", col_names = FALSE)

#nombre columnas
names(epa_http)<- c("URL", "Time", "Tipo", "Recurso", "Protocolo", "Post", " Bytes ")



## pregunta 1.2
epa_merge <- epa_http %>% distinct(URL, .keep_all = TRUE)

## buscando los tipo de errores en el dataframe unico por URL
epa_merge_agrupado <- epa_merge %>% group_by(Post) %>% summarise(n = n())



##pregunta 1.3


em_agroupado_imagenes_all <- filter(epa_http, grepl(pattern = "\\.jpg|\\.png|\\.gif|\\.ico", Recurso))

## Pregunta 1.4

ggplot(epa_merge_agrupado, aes(x = factor(Post), y = n,fill = factor(Post))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") + # Colores del paquete RColorBrewer
  xlab("Código de respuesta") +
  ylab("Cantidad de peticiones") +
  ggtitle("Distribución de peticiones según el código de respuesta")

### segundo cuadro


pie(epa_merge_agrupado$n, labels = paste("Código", epa_merge_agrupado$Post),
    main = "Distribución de peticiones según el código de respuesta",
    col = rainbow(length(epa_merge_agrupado$n))) # Colores del arco iris


## Pregunta 1.5

set.seed(123) # Establecer una semilla para reproducibilidad
k <- 2 # Número de clusters deseado
clustering <- kmeans(epa_http[, c("Post", "Bytes")], centers = k)


### Pregunta 1.5

# Generar columna con longitud de URL
epa_http$Longitud_URL <- nchar(epa_http$URL)

# Aplicar k-means con distintos valores de k
library(cluster)

epa_http <- epa_http[complete.cases(epa_http),]
epa_http$` Bytes `<- as.integer(epa_http$` Bytes `)



set.seed(123) # Fijar semilla para reproducibilidad
k <- 2
km.res1 <- kmeans(epa_http[,c(" Bytes ", "Longitud_URL")], centers = k)


set.seed(456) # Fijar semilla para reproducibilidad
k <- 3
km.res2 <- kmeans(epa_http[,c(" Bytes ", "Longitud_URL")], centers = k)

# Visualizar resultados
par(mfrow=c(1,2)) # Para ver ambos gráficos en la misma ventana
plot(epa_http$` Bytes `, epa_http$Longitud_URL, col=km.res1$cluster, main="k-means (k=2)")
points(km.res1$centers[,1], km.res1$centers[,2], col=1:2, pch=8, cex=2)
plot(epa_http$` Bytes `, epa_http$Longitud_URL, col=km.res2$cluster)


## pregunta 1.6

# Crear un vector de colores para representar los grupos
colores <- c("red", "blue")
grupo <- km.res1$cluster

## Bytes y Longitud URL
# Crear el scatter plot con las variables Bytes y Longitud_URL
plot(epa_http$` Bytes `, epa_http$Longitud_URL, col = colores[grupo], 
     xlab = " Bytes ", ylab = " Longitud URL ")


# Agregar los puntos correspondientes a cada grupo
points(epa_http$` Bytes `[grupo == 1], epa_http$Longitud_URL[grupo == 1], col = "red")
points(epa_http$` Bytes `[grupo == 2], epa_http$Longitud_URL[grupo == 2], col = "blue")

## Bytes y Tiempo

plot(epa_http$` Bytes `, epa_http$Time, col = colores[grupo], 
     xlab = "Bytes", ylab = "Time")
points(epa_http$` Bytes `[grupo == 1], epa_http$Time[grupo == 1], col = "red")
points(epa_http$` Bytes `[grupo == 2], epa_http$Time[grupo == 2], col = "blue")

     
