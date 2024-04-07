# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(ggplot2)
library(gridExtra)
# Aqui se debe poner la carpeta de la materia de SU computadora local
#setwd("C:\\Users\\Nicolas\\OneDrive\\Ciencia de Datos\\Austral_2024_Laboratorio1") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

#FEATURE ENGINEERING CREACION DE NUEVAS COLUMNAS

dataset$total_plazofijo = dataset$mplazo_fijo_dolares + dataset$mplazo_fijo_pesos  
dataset$cant_prestamos <-dataset$cprestamos_personales + dataset$cprestamos_prendarios+ dataset$cprestamos_hipotecarios  
dataset$monto_prestamos <-dataset$mprestamos_personales + dataset$mprestamos_prendarios+ dataset$mprestamos_hipotecarios  
dataset$tarjeta_financiacion_limite <-dataset$Visa_mfinanciacion_limite + dataset$Master_mfinanciacion_limite  
dataset$tarjeta_saldo_total <-dataset$Master_msaldototal + dataset$Visa_msaldototal  
dataset$tarjeta_limite_compra <-dataset$Master_mlimitecompra + dataset$Visa_mlimitecompra  
dataset$tarjeta_mpagado <-dataset$Master_mpagado + dataset$Visa_mpagado  
dataset$tarjeta_mconsumototal <-dataset$Visa_mconsumototal + dataset$Master_mconsumototal  
dataset$Monto_cajaahorro <-dataset$mcaja_ahorro + dataset$mcaja_ahorro_adicional+ dataset$mcaja_ahorro_dolares  
dataset$Monto_cuentacorriente <-dataset$mcuenta_corriente_adicional + dataset$mcuenta_corriente
dataset$cant_inversiones <-dataset$cinversion1 + dataset$cinversion2
dataset$Monto_inversiones <-dataset$minversion1_pesos + dataset$minversion1_dolares + dataset$minversion2
dataset$cant_seguros <-dataset$cseguro_vida + dataset$cseguro_auto + dataset$cseguro_vivienda+ dataset$cseguro_accidentes_personales


#CLASIFICACION DE EDADES CADA 10 anos

lim_inferior <- min(dataset$cliente_edad)
lim_superior <- max(dataset$cliente_edad) + 5
rangos_edad <- seq(lim_inferior, lim_superior, by = 10)  # Definir los límites de los rangos
dataset$rango_edad <- cut(dataset$cliente_edad, breaks = 10, labels = FALSE)

# COLUMNAS A QUITAR
columnasQuitar <-c("Visa_mpagominimo","Visa_cadelantosefectivo",
                   "Visa_cconsumos","Visa_fechaalta",
                   "Visa_mpagosdolares","Visa_mpagospesos",
                   "Visa_fultimo_cierre","Visa_madelantodolares",
                   "Visa_madelantopesos","Visa_mconsumosdolares",
                   "Visa_mconsumospesos","Visa_msaldodolares",
                   "Visa_msaldopesos","Visa_Finiciomora",
                   "Visa_Fvencimiento",
                   "Master_mpagominimo","Master_cadelantosefectivo",
                   "Master_cconsumos","Master_fechaalta",
                   "Master_mpagosdolares","Master_mpagospesos",
                   "Master_fultimo_cierre","Master_madelantodolares",
                   "Master_madelantopesos","Master_mconsumosdolares",
                   "Master_mconsumospesos","Master_msaldodolares",
                   "Master_msaldopesos","Master_Finiciomora",
                   "Master_Fvencimiento","mplazo_fijo_dolares","mplazo_fijo_pesos",
                   "cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                   "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                   "Visa_mfinanciacion_limite","Master_mfinanciacion_limite",
                   "Master_msaldototal","Visa_msaldototal",
                   "Master_mlimitecompra","Visa_mlimitecompra",
                   "Master_mpagado","Visa_mpagado",
                   "Visa_mconsumototal","Master_mconsumototal",
                   "mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares", 
                   "mcuenta_corriente_adicional","mcuenta_corriente","cinversion1","cinversion2",
                   "cseguro_vida","cseguro_auto","cseguro_vivienda","cseguro_accidentes_personales",
                   "cliente_edad")

#quito las columnas que no voy a usar
dataset <- subset(dataset, select = -which(names(dataset) %in% columnasQuitar))

# Convertir la columna 'rango_edad' a factor para que sea más fácil de interpretar
dtrain$rango_edad <- factor(dtrain$rango_edad)

# Convertir la columna 'clase_ternaria' a factor para que sea más fácil de interpretar
dtrain$clase_ternaria <- factor(dtrain$clase_ternaria)

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


cp <- -0.3
minsplit <- 650
maxdepht <- 10
minbucket <- 300
maxdepht <- 7 

modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = cp, # esto significa no limitar la complejidad de los splits
        minsplit = minsplit, # minima cantidad de registros para que se haga el split
        minbucket = minbucket, # tamaño minimo de una hoja
        maxdepth = maxdepht # profundidad maxima del arbol
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
       file = "~/buckets/b1/exp/KA2001/K101_003.csv",
        sep = ","
)
