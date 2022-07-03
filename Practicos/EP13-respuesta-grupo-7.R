############# PRÁCTICO 13 #############
#Alumnos: Matías Bozo - Aylin Rodriguez - Ignacio Villarroel.

# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres (Heinz et al., 2003). Estas
# mediciones están disponibles en el archivo "EP13 Datos.csv",, que acompaña a este enunciado. El estudio
# incluye nueve mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce
# mediciones de grosor (circunferencias) que incluyen el tejido. 

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso, de acuerdo con las
# siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el
# dígito verificador) del integrante de menor edad del equipo

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la
# semilla es impar)

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la
# variable Peso, justificando bien esta selección

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado
# en el paso anterior

library(dplyr)
library(ggpubr)
library (caret)
library(car)

datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# Se transforma la variable Género a factor 
#datos[["Gender"]] <- factor(datos[["Gender"]])


# RUT del integrante menor: 20442784-4
# Definir semilla a utilizar
semilla <- 2784 
set.seed(semilla)

# Al ser semilla un número par se consideran sólo los datos de las mujeres.
mujeres <- datos %>% filter(Gender == "0")

# Se selecciona la muestra de 50 mujeres.
muestra <- mujeres[sample(nrow(mujeres), 50),]

# Se obtienen las columnas del dataframe "mujeres".
columnas <- colnames(muestra)

without_gender <- which(columnas == "Gender")
without_weight <- which(columnas == "Weight")

# Se eliminan las variables Peso y Género del vector que contiene 
# los nombres de variables, esto para que no se consideren dentro de la
# selección aleatoria de las variables predictoras.
columnas <- columnas[-without_gender]
columnas <- columnas[-without_weight]

# print(columnas)
# Se seleccionan las 8 variables predictoras de forma aleatoria
variablesPredic <- sample(columnas, 8)

# Se obtienen las variables restantes para poder
# seleccionar una que podría ser útil para predecir la variable Peso.

aux <- c()
for (i in 1:length(variablesPredic)){
  indice <- which(columnas == variablesPredic[i])
  aux <- c(aux, indice)
}
variablesNoSelec <- columnas[-aux]

print(variablesNoSelec)

# La variable seleccionada es: 
# Chest.Girth (circunferencia del pecho), ya que al tener una circunferencia del pecho
# mayor o menor, se podría suponer que esto también influye en el peso de la persona,
# en otras palabras, al tener mayor o menor circunferencia, se tiene mayor o menor volumen
# corporal respectivamente, lo que influye en su peso.

# Tal como se solicita, se piensa usar un modelo de regresión lineal 
# simple, lo cual se hará mediante mínimos cuadrados.

# Cumplimiento de condiciones:
# 1. b Para medir la fuerza de la relación lineal entre la variable 
# Peso y el predictor, se debe calcular 
# el coeficiente de correlación que existe entre ellas

correlacion <- cor(muestra[["Chest.Girth"]], muestra[["Weight"]])
print(correlacion)

# De acuerdo al valor obtenido del coeficiente de correlación, el cual es 0.878273 
# (cercano a 1 y positivo), se puede afirmar que sí existe una relación lineal directa entre 
# la variable Peso y el predictor Circunferencia del pecho. Además en el gráfico de la relación 
# entre las variables, se observa la pendiente positiva rearfimando la relación directa.

# 2. La distribución de los residuos debe ser cercana a la normal:
# De acuerdo al gráfico Q-Q de los residuos y al valor 
# obtenido por la prueba de Shapiro, se puede decir que estos siguen una distribución 
# cercana a la normal.

# 3. La variabilidad de los residuos debe ser aproximadamente constante:
# En el gráfico de los residuos, se puede observar que la variabilidad de los puntos en torno a la 
# línea de los mínimos cuadrados es aproximadamente constante ya que los puntos 
# están distribuidos se manera aleatoria, sin seguir un patrón, y 
# se puede apreciar que no forman un "embudo".

# 4.- Se cumple que las observaciones son independientes entre sí ya que se elige una muestra al 
# azar de tamaño 50, por lo que no se usa regresión lineal con series de tiempo, ya que los datos 
# no dependen de situaciones o acciones del pasado.

modelo <- lm( Weight ~ Chest.Girth, data = muestra)
print(summary(modelo))
# Test de Sahpiro para los residuos
shapiro <- shapiro.test(modelo$residuals)
print(shapiro)

# Gráfico Q-Q para la distribución de los residuos
datosResiduos <- data.frame(modelo$residuals)
g <- ggqqplot(datosResiduos, x = "modelo.residuals", color = "Steelblue", xlab = "Teórico", ylab = "Residuos",
              title = "Gráfico Q-Q residuos v/s distr. normal")
print(g)

# Se grafica el modelo
p <- ggscatter(muestra, x = "Chest.diameter", y = "Weight",
               color = "blue", fill = "blue", xlab = "Diametro del pecho [cm]",
               ylab = "Peso [Kg]")
p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
print(p)

# Crear gráficos para evaluar el modelo
plot(modelo)

# CONCLUSIONES
# De acuerdo a los resultados obtenidos, se espera que al aumentar
# la circunferencia del pecho en 1 centímetro, el peso aumente en 1,23116 
# kilogramos, es decir, que los 1,23116 kilogramos se distribuirán en el cuerpo.

# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresión líneal simple obtenido en el paso 5

# 7. Evaluar los modelos y "arreglarlos" en caso de que tengan algún problema con las condiciones que
# deben cumplir

# 8. Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando
# validación cruzada)

# La variables seleccionadas son:
# - Thigh.Girth
# - Age
# - Shoulder.Girth

# Ahora se agrega cada una de las variables al modelo
modelo_RLM <- update (modelo, . ~ . + Thigh.Girth)
modelo_RLM <- update (modelo_RLM, . ~ . + Ankle.Minimum.Girth)
modelo_RLM <- update (modelo_RLM, . ~ . + Knee.Girth)
print(summary(modelo_RLM))

# Evaluación del RLS
modelo

############################ Evaluación del modelo RLM ############################
# Reducir matriz de datos para que solo contenga los predictores
# personas y la respuesta .
predictores <- names(coef (modelo_RLM))[ -1]
muestra2 <- muestra[ , c(predictores , "Weight") ]

# Se construye una matriz de datos con la respuesta predicha , los residuos y algunas 
# estadísticas para evaluar la influencia de cada observación.
resultados <- data.frame(respuesta_predicha = fitted(modelo_RLM))
resultados [["residuos_estandarizados"]] <- rstandard(modelo_RLM)
resultados [["residuos_estudiantizados"]] <- rstudent(modelo_RLM)
resultados [["distancia_Cook"]] <- cooks.distance(modelo_RLM)
resultados [["dfbeta"]] <- dfbeta(modelo_RLM)
resultados [["dffit"]] <- dffits(modelo_RLM)
resultados [["apalancamiento"]] <- hatvalues(modelo_RLM)
resultados [["covratio"]] <- covratio(modelo_RLM)


cat ("Identificación de valores atípicos :\n")
# Observaciones con residuos estandarizados fuera del 99 % esperado.
sospechosos1 <- which(abs(resultados [["residuos_estandarizados"]]) > 2.576)

cat ("- Residuos estandarizados fuera del 99 % esperado :", sospechosos1 , "\n")

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(resultados [["distancia_Cook"]] > 1)

cat ("- Residuos con una distancia de Cook alta :", sospechosos2 , "\n")

# Observaciones con apalancamiento mayor igual al doble del
# apalancamiento promedio.

apal_medio <- (ncol(muestra2) + 1) / nrow (muestra2)
sospechosos3 <- which(resultados [["apalancamiento"]] > 2 * apal_medio)

cat ("- Residuos con apalancamiento fuera de rango :",sospechosos3, "\n")

# Observaciones con DFBeta mayor o igual a 1.
sospechosos4 <- which(apply( resultados [["dfbeta"]] >= 1, 1 , any))
names(sospechosos4) <- NULL

cat ("- Residuos con DFBeta >= 1:",sospechosos4, "\n")

# Observaciones con razón de covarianza fuera de rango .
inferior <- 1 - 3 * apal_medio
superior <- 1 + 3 * apal_medio
sospechosos5 <- which (resultados [["covratio"]] < inferior | resultados [["covratio"]] > superior )

cat ("- Residuos con razón de covarianza fuera de rango :", sospechosos5 , "\n")
# Resumen de valores sospechosos .
sospechosos <- c( sospechosos1 , sospechosos2 , sospechosos3 ,sospechosos4 , sospechosos5 )

sospechosos <- sort(unique(sospechosos))

cat ("\n Resumen de valores sospechosos :\n")
cat (" Apalancamiento promedio :", apal_medio , "\n")
cat (" Intervalo razón de covarianza : [", inferior , "; ",superior , "]\n\n", sep = "")

print(round(resultados[sospechosos , c("distancia_Cook", "apalancamiento","covratio") ], 3) )

#-------------- Análisis de la evaluación del modelo RLM -----------------------
# La distancia de Cook estimada para todas las observaciones potencialmente influyentes 
# están lejos de sobrepasar el valor recomendado, por lo que en este caso no 
# parece ser necesario quitar observaciones, aún cuando algunas muestren
# valores un tanto alto de apalancamiento y covarianza.

# ------------------- Verificación de condiciones para modelo RLM -------------------
# Comprobar independencia de los residuos.
cat ("Prueba de Durbin - Watson para autocorrelaciones")
cat ("entre errores :\n")
print(durbinWatsonTest (modelo_RLM))


# Comprobar normalidad de los residuos para el modelo RLM.
cat ("\n Prueba de normalidad para los residuos :\n")
print(shapiro.test(modelo_RLM$residuals))

# Comprobar homocedasticidad de los residuos para el modelo RLM.
cat ("Prueba de homocedasticidad para los residuos :\n")
print(ncvTest(modelo_RLM))

# Comprobar la multicolinealidad.
vifs <- vif(modelo_RLM)
cat("\n Verificar la multicolinealidad :\n")
cat ("- VIFs :\n")
print(vifs)
cat ("- Tolerancias :\n")
print (1 / vifs )
cat ("- VIF medio :", mean ( vifs ) , "\n")

# ------ ANÁLISIS DE LA VERIFICACIÓN DE CONDICIONES ------
# 1. Al comprobar la independencia de los residuos se obtiene
# un p-valor = 0.196 mayor al alpha (0.01), por lo
# tanto podemos concluir que los residuos son independientes.

# 2.- Al aplicar la prueba de Shapiro a los residuos del modelo
# se obtiene un p-valor = 0.7913 > alfa = 0.01, por lo que
# se puede asumir que los residuos siguen una distribución normal.

# 3.- La prueba de homocedasticidad arroja un p-valor = 0.044062,
# el cual es mayor a alfa = 0.01, por lo que se cumple 
# el supuesto de homocedasticidad.

# 4.- Al comprobar la multicolinealidad, podemos observar
# los VIFS individuales y considerando VIF>= 5 como preocupante,
# obtenemos que todos son menores a dicho valor. Además las variables
# tienen valores de tolerancia mayores a 0.2, por lo que se cumple
# que se cumple la condición de multicolinealidad.



# ------------------- Evaluar poder predictivo de los modelos-------------------
# Se crean los conjuntos de entrenamiento y prueba
n <- nrow(muestra)
n_entrenamiento <- floor(0.85*n)
var <- sample.int (n=n , size = n_entrenamiento ,replace = FALSE )
entrenamiento <- muestra[var, ]
prueba <- muestra [ -var, ]



# Se calcula el poder predictivo del modelo RLM 

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_val_cruz <- train ( Weight ~ Chest.diameter + Knee.Girth + Thigh.Girth + Ankle.Minimum.Girth, data = muestra, method = "lm",
                           trControl = trainControl ( method = "cv", number = 5) )

print (summary(modelo_val_cruz))

# Obtener error cuadrado promedio para el conjunto de entrenamiento .
rmse_entrenamiento <- modelo_val_cruz$results$RMSE
cat (" MSE para el conjunto de entrenamiento modelo RLM:", rmse_entrenamiento , "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict(modelo_val_cruz,prueba)
# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba [["Weight"]] - predicciones
mse_prueba <- mean (error** 2)
rmse_prueba <- sqrt(mse_prueba)
cat (" MSE para el conjunto de prueba modelo RLM:", rmse_prueba)


# Se calcula el poder predictivo del modelo RLS
# Ajustar modelo usando validación cruzada de 5 pliegues.
modeloRLS_val_cruz <- train ( Weight ~ Chest.diameter, data = muestra, method = "lm",
                              trControl = trainControl ( method = "cv", number = 5) )

print (summary(modeloRLS_val_cruz))

# Obtener error cuadrado promedio para el conjunto de entrenamiento .
rmse_entrenamiento_RLS <- modeloRLS_val_cruz$results$RMSE
cat (" MSE para el conjunto de entrenamiento modelo RLS:", rmse_entrenamiento_RLS , "\n")

# Hacer predicciones para el conjunto de prueba .
prediccionesRLS <- predict(modeloRLS_val_cruz,prueba)
# Calcular error cuadrado promedio para el conjunto de prueba .
errorRLS <- prueba [["Weight"]] - prediccionesRLS
mse_prueba_RLS <- mean (errorRLS** 2)
rmse_prueba_RLS <- sqrt(mse_prueba_RLS)
cat (" MSE para el conjunto de prueba modelo RLS:", rmse_prueba_RLS)

# Para el caso del modelo de regresión lineal simple, como se puede observar, para el conjunto de entrenamiento
# la raíz del error cuadrático medio es RMSEe = 4.93107
# mientras que para el conjunto de prueba obtenemos RMSEp = 6.744698. 
# Existe diferencia entre ambos errores, por lo que se puede concluir
# que el modelo está sobreajustado, es decir, que se adapta bien a los datos del conjunto de
# entrenamiento pero no tanto al conjunto de prueba, por lo que podría ser imprudente suponer que puede ser
# generalizado.


# Para el caso del modelo de regresión lineal múltiple, como se puede notar, para el conjunto de entrenamiento, 
# la raíz del error cuadrático medio es RMSEe = 3.758793,
# mientras que para el conjunto de prueba obtenemos RMSEp = 3.542385.
# Como los errores son muy parecidos, se puede decir que el modelo no está sobreajustado porque
# los datos se adapatan bien tanto al conjunto de datos de entrenamiento como el de prueba,
# por lo que podemos suponer que este modelo podría ser generalizable.