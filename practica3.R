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

