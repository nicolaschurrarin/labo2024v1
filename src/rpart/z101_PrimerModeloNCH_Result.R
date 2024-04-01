require("data.table")
require("rpart")
require("rpart.plot")
library(ggplot2)

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Nicolas\\OneDrive\\Ciencia de Datos\\Austral_2024_Laboratorio1") # Establezco el Working Directory


prueba<-"es una prueba para el comitt"
# cargo el dataset
# dataset <- fread("./exp/KA2001/K101_001.csv")

directorio<-("./exp/KA2001")

archivos <- list.files(directorio, pattern = "*csv", full.names = TRUE)


obtener_nombre_archivo <- function(campo) {
  nombre_archivo <- basename(campo)
  # Eliminamos la extensión si está presente
  nombre_archivo <- sub("\\.csv$", "", nombre_archivo)
  return(nombre_archivo)
}


dflista<- lapply(archivos, function(archivo){
  datos<-read.csv(archivo)
  datos$file <- obtener_nombre_archivo(archivo)
  return(datos)
})

dftotal<-do.call(rbind,dflista)


ggplot(dftotal, aes(x = file, fill = factor(Predicted))) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat='count', aes(x = file, label = ..count..), hjust = -0.1,vjust = -0.7) +
  labs(title = "Cantidad de Valores Predichos por Archivo",
       x = "Archivo", y = "Cantidad", fill = "Valor Predicho")

