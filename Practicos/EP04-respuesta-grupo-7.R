##########Ejercicio práctico 4##########
# Alumnos: Aylin Rodriguez
#          Ignacio Villarroel

library(tidyr)
library (TeachingDemos)
library (ggpubr)
library(ggplot2)
library (dplyr)
if (!require(nortest)){
  install.packages("nortest", dependencies = TRUE )
  require (nortest)
}


datos <- read.csv2("C:\\Users\\ignac\\OneDrive\\Escritorio\\datos4.csv")

### Pregunta 1 ####

# Grupo 7:
#   1. El Comité Olímpico cree que el mejor tiempo medio de los atletas blancos después de ingresar al programa
# de entrenamiento es superior a 11,78 segundos. ¿Soportan los datos esta afirmación?

#Hipotesis Nula: Tiempo medio = 11.78
#Hipotisis Alternativa: Tiempo medio <= 11.78

atletasbancos <- datos %>% filter(Raza == "Blanca") 
agruparatletas <- group_by(atletasbancos, Posterior)
alfa = 0.05
x <- agruparatletas$Posterior
# Realizar la prueba t para la muestra.
pruebat <- t.test(x, mu = 11.78, alternative = "two.sided", conf.level = 1 - alfa)
print(pruebat)

#  RESPUESTA: Se puede observar que el valor P es menor que el nivel de significancia (< 0,05), por lo tanto, se rechaza la hipotesis nula a
# favor de la alternativa, también se considera que la media de los datos se encuentra dentro del intervalo de confiantza
# [12,06377, 13,65833], Por lo tanto los datos si soportan esta afirmación puesto que se toma la hipotesis alternativa indicando que son
# superiores a 11,78 segundos.

### Pregunta 2 ####

#   2. ¿Sugieren los datos que la mejor marca de los atletas negros se reduce en 4,68 segundos tras el
# entrenamiento?

#HIPOTESIS NULA Tiempo Posterior Negros - Tiempo PRevio Negros = 4,68
#HIPOTESIS ALTERNATIVA Tiempo Posterior Negros - Tiempo PRevio Negros /= 4,68
atletasnegros <- datos %>% filter(Raza == "Negra") 
agruparatletas2 <- group_by(atletasnegros, Posterior)
alfa = 0.01
xantes <- agruparatletas2$Previo
xdespues <- agruparatletas2$Posterior
#promedio de antes = 13.68
#promedio de despues = 9.78
diferencia <-  xantes - xdespues
agruparatletas2 <- cbind(agruparatletas2, diferencia)
head(agruparatletas2,4)

# Realizar la prueba t para la muestra.
pruebat2 <- t.test(xantes,xdespues, paired = TRUE, mu = 4.68,
                   conf.level = 1 - alfa, alternative = "greater")
print(pruebat2)

# RESPUESTA: Se puede observar que el valor P es mayor que el nivel de significancia (> 0,05), por lo tanto, se acepta la hipotesis nula a
# favor de la alternativa. Por lo que la pregunta si el mejor tiempo de mejora es 4,68 segundos, es correcto puesto que es nuestra
# Hipotesis nula, la cual fue aceptada.


### Pregunta 3 ####

#   3. ¿Es posible afirmar que, en promedio, los atletas negros superan a los blancos por menos de 1,34 segundos
# antes del entrenamiento?

# #Hipotesis Nula: Tiempo Medio Blanco = Tiempo Medio Negros
# Hipotisis Alternativa: Tiempo medio Blanco /= Tiempo medio Negro
atletasblancos1 <- agruparatletas$Previo
atletasnegros1 <- agruparatletas2$Previo
normalidadA <- shapiro.test(atletasblancos1)
print(normalidadA)
normalidadB <- shapiro.test(atletasnegros1)
print(normalidadB)
#diferencia2 <- atletasblancos1 - atletasnegros1
#normalidad <- shapiro.test(diferencia)
#print (normalidad)
alfa <- 0.05


#pruebat3 <- t.test(diferencia, alternative = "two.sided", mu = 1.36, conf.level = 1 - alfa)
#print(pruebat3)

pruebat3 <- t.test(x = atletasblancos1, y = atletasnegros1, paired = FALSE, alternative = "greater", mu = 1.36, conf.level = 1 - alfa)
print(pruebat3)
mediaA <- mean(atletasblancos1)
mediaB <- mean(atletasnegros1)
diferencia <- mediaA - mediaB
cat(" Diferencia de las medias =", diferencia , "[seg]\n")

# RESPUESTA: Se puede observar que el valor P = 0.1487 es mayor que el nivel de significancia (> 0,05), por lo tanto, se acepta la hipotesis nula a
# favor de la alternativa. Por lo que previo a los entrenamientos, no es posible afirmar que los atletas negros superan en 1,34 segundos
# a los atletas blancos