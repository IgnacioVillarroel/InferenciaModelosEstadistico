############# PRÁCTICO 15 #############
#Alumnos: Matías Bozo - Aylin Rodriguez - Ignacio Villarroel.
library(caret)
library(dplyr)
library(leaps)
library(pROC)

# Lectura del archivo.
datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# 1. Definir la semilla a utilizar, que corresponde a los primeros 
# cinco dígitos del RUN del integrante de mayor edad del equipo.
# RUN integrante mayor: 18.954.417-0
set.seed(18954)

# Agregar la columna IMC.
datos[["IMC"]] <- datos[["Weight"]] / ((datos[["Height"]] / 100)^2) 

# Crear la variable dicotómica
# 1: Sobrepeso
# 0: No sobrepeso
condicion <- ifelse(datos[["IMC"]] >= 25, 1,  0)

# Se transforma EN a variable categórica.
# EN: Estado Nutricional.
datos[["EN"]] <- factor(condicion)

# 2. Seleccionar una muestra de 100 personas, asegurando que la mitad 
# tenga estado nutricional "sobrepeso" y la otra mitad "no sobrepeso".
sobrepeso <- datos %>% filter(EN == 1)
muestraSobrepeso <- sobrepeso[sample(nrow(sobrepeso), 50),]

noSobrepeso <- datos %>% filter(EN == 0)
muestraNoSobrepeso <- noSobrepeso[sample(nrow(noSobrepeso), 50),]

# Ambas data frames se unen.
muestra1 <- rbind(muestraNoSobrepeso, muestraSobrepeso)

muestra <- muestra1

# 3. Usando las herramientas del paquete leaps, realizar una búsqueda 
# exhaustiva para seleccionar entre dos y ocho predictores que ayuden 
# a estimar la variable Peso (Weight), obviamente sin considerar las 
# nuevas variables IMC ni EN, y luego utilizar las funciones del 
# paquete caret para construir un modelo de regresión lineal múltiple 
# con los predictores escogidos y evaluarlo usando bootstrapping.

cat("############################## PREGUNTA 3 #############################\n")

# No considerar nuevas variables IMC ni EM.
muestra$EN <- NULL
muestra$IMC <- NULL

# Realizar busqueda exhaustiva.
busqueda <- regsubsets(Weight ~ ., 
                       data = muestra, 
                       method = 'exhaustive',
                       nbest = 2, # Cantidad mínima de predictores.
                       nvmax = 8) # Cantidad máxima de predictores.

plot(busqueda)

# De acuerdo a la exploración de todos los subconjuntos, el mejor modelo de
# 2 y 5 variables es aquel que usa como predictores:
#   - "Chest Girth".
#   - "Waist Girth".
#   - "Thigh Girth".
#   - "Knee Girth".
#   - "Height".

formula <- Weight ~ Chest.Girth + Waist.Girth + Thigh.Girth + Knee.Girth + Height

control <- trainControl(method = "boot",
                        number = 1000)

# Ajustar el modelo con los mejores predictores usando bootstrapping con 1000
# repeticiones.
rlm <- train(formula, 
             data = muestra,
             trControl = control,
             method = "lm")

print(summary(rlm))

# Para ver si el modelo es válido, se observa el estadístico F y el p-value.
# El valor de p-value (2.2e-16) es menor a 0.05, por lo que se rechaza H0, 
# (el modelo no es válido). Entonces, el modelo utilizado si es válido.

# Al observar el valor de Adjusted R-squared: 0.9646, indice que el 96.46% de
# la variabilidad del peso es predicha por este modelo, teniendo en cuenta, 
# las 5 variables predictoras.

# Al observar los coeficientes, las variables predictoras poseen un p-value
# menor a 0.05, por lo que se rechaza H0 (las variables no aportan al modelo). 
# Por lo tanto, las 5 variables predictoras aportan al modelo.

# 4. Haciendo un poco de investigación sobre el paquete caret, en particular
# cómo hacer Recursive Feature Elimination (RFE), construir un modelo de 
# regresión lineal múltiple para predecir la variable IMC que incluya entre 
# 10 y 20 predictores, seleccionando el conjunto de variables que maximice 
# R2 y que use cinco repeticiones de validación cruzada de cinco pliegues 
# para evitar el sobreajuste (obviamente no se debe considerar las variables
# Peso, Estatura ni estado nutricional -Weight, Height, EN respectivamente).

cat("############################## PREGUNTA 4 #############################\n")

# La muestra ya no considera EN (pregunta 3). Entonces, no se considera en 
# la muestra las variables Weight y Height, mientras que la variable IMC 
# vuelve a ser considerada.
muestra$Weight <- NULL
muestra$Height <- NULL
muestra$IMC <- muestra1$IMC

# Cantidad posible de predictores (entre 10 y 20 variables predictoras).
sizes <- c(10:20)

# Crear particion para la muestra.
particion <- createDataPartition(muestra$IMC, 
                                   p = 0.7, 
                                   list = FALSE)

# Conjunto de entrenamiento (70% de los datos) a utilizar en validación cruzada.
entrenamiento <- muestra[particion, ]

# Conjunto de prueba (30% de los datos) a utilizar en validación cruzada.
prueba_1 <- muestra[-particion, ]

# Definir variable x, sin la columna con los datos de IMC.
x <- entrenamiento[,-24]

# Definir variable y, con los datos de IMC.
y <- entrenamiento$IMC

# Implementación de Recursive Feature Selection (RFE):

# Definir objeto de control para RFE, con validación cruzada 
# de cinco pliegues (number) y cinco repeticiones (repeats).
ctrl <- rfeControl(functions = lmFuncs, 
                   method = "repeatedcv", # Cross Validation.
                   number = 5,
                   repeats = 5, 
                   verbose = FALSE)

# Modelo de regresión lineal múltiple.
rlm2 <- rfe(x = x, 
            y = y, 
            sizes = sizes, 
            rfeControl = ctrl)

# Rendimiento de remuestreo sobre el tamaño del subconjunto.
print(rlm2)

# Identificar las 17 variables predictoras utilizadas en el modelo.
cat("Todas las variables predictoras del modelo: \n\n")
print(predictors(rlm2))
cat("\n")

# 5. Usando RFE, construir un modelo de regresión logística múltiple para la 
# variable EN que incluya el conjunto, de entre dos y seis, predictores que 
# entregue la mejor curva ROC y que utilice validación cruzada dejando uno
# fuera para evitar el sobreajuste (obviamente no se debe considerar las 
# variables Peso, Estatura -Weight y Height respectivamente- ni IMC).

cat("############################# PREGUNTA 5 ##############################\n")

# La muestra ya no considera Weight y Height (pregunta 4). Entonces, no se 
# considera la variable IMC, mientras que la variable EN vuelve a ser 
# considerada.
muestra$IMC <- NULL
muestra$EN <- as.factor(muestra1$EN)

# Cantidad posible de predictores (entre 2 y 6 variables predictoras).
sizes <- c(2:6)

# Crear particion para la muestra.
particion <- createDataPartition(muestra$EN, 
                                 p = 0.7, 
                                 list = FALSE)

# Conjunto de entrenamiento (70% de los datos) a utilizar en validación cruzada
# dejando uno fuera.
entrenamiento2 <- muestra[particion, ]

# Conjunto de prueba (30% de los datos) a utilizar en validación cruzada.
# dejando uno fuera.
prueba_2 <- muestra[-particion, ]

# Definir variable x, sin la columna con los datos de EN.
x <- entrenamiento2[,-24]

# Definir variable y, con los datos de EN.
y <- entrenamiento2$EN

# Implementación de Recursive Feature Selection (RFE):

# Definir objeto de control para RFE, con validación cruzada 
# dejando uno afuera, con regresión logística. Para esto, se
# utiliza las funciones de lrFuncs.
lrFuncs$summary <- twoClassSummary
control <- rfeControl(functions = lrFuncs,
                      method = "LOOCV", # Leave One Out Cross Validation.
                      verbose = FALSE)

# Modelo de regresión logística múltiple.
rLogm <- rfe(x = x,
             y = y,
             sizes = sizes,
             rfeControl = control,
             metric = "ROC")

# Identificar las 2 variables predictoras utilizadas en el modelo.
predictors(rLogm)

# Rendimiento de remuestreo sobre el tamaño del subconjunto.
print(rLogm)

# Variables   ROC
# 2           0.8988 *Modelo seleccionado*
# 3           0.8743
# 4           0.8710
# 5           0.8620
# 6           0.8139
# 23          0.8322

# El modelo de regresión logística múltiple seleccionado, posee 2 variables
# predictoras ("Calf.Maximum.Girth" y  "Knee.Girth"). Además posee la mejor
# curva ROC (0.8988).

# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos.

cat("############################# PREGUNTA 6 ##############################\n")

# Se utilizan los datos de entrenamiento de los puntos
# 4 y 5, para estos dos modelos.

# Modelo de regresión lineal múltiple (rlm2):

predictrlm <- predict(rlm2$fit, prueba_1)
errorrlm <- prueba_1$IMC - predictrlm 
mserlm <- mean(errorrlm ** 2)
cat("\nMSE_rlm = ", mserlm, "\n\n")

# El MSE es de 2.048588 el cual es un valor pequeño, por lo que se puede deducir
# que tiene un buen poder predictivo, de apenas +-2.04 para cada predicción.

 
# Modelo de regresión logística múltiple (rLogm) con Recursive Feature 
# Elimination (RFE):

probs_e <- predict(rLogm$fit, prueba_2, type = "response")
umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(entrenamiento2[["EN"]]))
ROC_e <- roc(prueba_2[["EN"]], probs_e)
plot(ROC_e)

# La curva de ROC de prueba se aleja de la diagonal, por lo que al parecer
# se trata de un buen modelo.

