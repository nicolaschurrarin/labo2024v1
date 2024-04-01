# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(ggplot2)
library(gridExtra)
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Nicolas\\OneDrive\\Ciencia de Datos\\Austral_2024_Laboratorio1") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")


dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

## VERIFICANDO MI DATASET dtrain

str(dtrain)
summary(dtrain$cliente_edad)
table(dtrain$cliente_edad)

lista_graficos <- list()

for (var in colnames(dtrain)[6]) {
  p <- ggplot(dtrain, aes_string(x = var, fill = "clase_ternaria")) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, binwidth = 15)+
    stat_density(geom = "line", position = "identity", color = "blue", size = 1) +
    theme_minimal() +
    labs(title = paste(var),
         x = var,
         y = "Densidad")+ 
    theme(text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5)  )          
  lista_graficos[[length(lista_graficos) + 1]] <- p
}

grid.arrange(grobs = lista_graficos)



#HISTOGRAMA

for (var in colnames(dtrain)[6]) {
  p <- ggplot(dtrain, aes_string(x = var, fill = "clase_ternaria")) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, binwidth = 15)+
    theme_minimal() +
    labs(title = paste("Histograma de", var),
         x = var,
         y = "Frecuencia") + 
    theme(text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5)  
    )                    
  
  lista_graficos[[length(lista_graficos) + 1]] <- p
}

grid.arrange(grobs = lista_graficos)




# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 200, # minima cantidad de registros para que se haga el split
        minbucket = 100, # tamaño minimo de una hoja
        maxdepth = 7 # profundidad maxima del arbol
) 


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_003.csv",
        sep = ","
)
