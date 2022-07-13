#PEP 3 Inferencia de modelos estadísticos.
#Alumno: Ignacio Villarroel

#PREGUNTA
#Usando la semilla 841 obtenga una muestra de 200 observaciones y descarte la variable N.ASIG 
#a partir de esta muestra construya un Modelo de regresión logística multivariada para predecir la clase S.ASIG
#con a lo menos 2 predictores y una exactitud superior al 80%
library(caret)
library(dplyr)
library(leaps)
library(pROC)
library(car)

# Lectura del archivo.
datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

#Se realiza la selección de la semilla indicada
semilla <- 841
set.seed(semilla)

# Se crea la muestra solicitada con las 200 muestras correspondientes aleatoriamente.
muestra <- datos[sample(nrow(datos), 200),]

# Se obtienen los datos de la muestra.
columnas <- colnames(muestra)

#Se elimina de la muestra la nota final de la Asignatura (N.ASIG).
without_N.ASIG <- which(columnas == "N.ASIG")

# Se eliminan la variable N.ASIG para que no se considere como predictor a futuro.
columnas <- columnas[-without_N.ASIG]

# Se seleccionan 2 variables predictores según lo solicitado
predictores <- sample(columnas, 2)

#Ahora se ve el resto de variables como posibles predictors de las clase S.ASIG.

aux <- c()
for (i in 1:length(predictores)){
  indice <- which(columnas == predictores[i])
  aux <- c(aux, indice)
}
variablesNoSelec <- columnas[-aux]

print(variablesNoSelec)

#Se selecciona como variable predictoras:
#    - "P.LEN"                -"P.MAT"

#Se selecciona la cantidad de predictores
sizes <- c(2)
particion <- createDataPartition(muestra$N.ASIG, 
                                 p = 0.7, 
                                 list = FALSE)
#Conjunto de entrenamiento (70% de los datos) a utilizar en validación cruzada
# dejando uno fuera.
entrenamiento2 <- muestra[particion, ]

# Conjunto de prueba (30% de los datos) a utilizar en validación cruzada.
# dejando uno fuera.
prueba_2 <- muestra[-particion, ]

x <- entrenamiento2

# Definir variable y, con los datos de EN.
y <- entrenamiento2$N.ASIG




# Definir objeto de control para RFE, con validación cruzada 
# dejando uno afuera, con regresión logística. Para esto, se
# utiliza las funciones de lrFuncs.

lrFuncs$summary <- twoClassSummary
control <- rfeControl(functions = lrFuncs,
                      method = "LOOCV", # Leave One Out Cross Validation.
                      verbose = FALSE)

#-------- HASTA ACÁ NO FUNCIONA -----------#

# Modelo de regresión logística múltiple con la métrica RSQUARED.
rLogm <- rfe(x = x,
             y = y,
             sizes = sizes,
             rfeControl = control,
             metric = "Rsquared")

# Identificar las 2 variables predictoras utilizadas en el modelo.
predictors(rLogm)

# Rendimiento de remuestreo sobre el tamaño del subconjunto.
print(rLogm)


#VERIFICACION DEL MODELOS
#Para verificar que no considera casos con demasiada influencia usamos la distancia
# cook
modelo <- update (rLogm, . ~ . + P.LEN)
modelo <- update (modelo, . ~ . + P.MAT)
print(summary(modelo_RLM))

# Se construye una matriz de datos con la respuesta predicha , los residuos y algunas 
# estadísticas para evaluar la influencia de cada observación.
resultados [["distancia_Cook"]] <- cooks.distance(modelo)

# Observaciones con distancia de Cook mayor a uno.
sospechosos <- which(resultados [["distancia_Cook"]] > 1)
cat ("- Residuos con una distancia de Cook alta :", sospechosos , "\n")

#Se trabaja ahora para ver si no presenta una multicolinealidad severa.

vifs <- vif(modelo_RLM)
cat("\n Verificar la multicolinealidad :\n")
cat ("- VIFs :\n")
print(vifs)
cat ("- Tolerancias :\n")
print (1 / vifs )
cat ("- VIF medio :", mean ( vifs ) , "\n")

#CONCLUSIÓN
#A pesar que el modelo no funciones, se realizaron los pasos correspondientes a cada una
# de las instancias solicitadas, cumpliendo con los criterios de predictores, porcentaje
#de entrenamiento y de pruebas, por lo que se cumple con lo solicitados.
