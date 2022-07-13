# Pep 2 Inferencia de modelos estadísticos
# Alumno: Ignacio Villarroel.


require(ez)
library(ggpubr)
library(tidyr)
library(dplyr)
library(boot)
library(simpleboot)
library(bootES)

#Se verá si existe una diferencia en la estatura promedio entre niños que se han alimentado con 
# leche materna y otros niños que han sido alimentados con la fórmula de una prestigiosa empresa.

# Se realiza la hipotesis para el desarrollo.

#H0: La estatura promedio entre niños alimentados con leche materna y leeche de una empresa es la misma.
#H1: La estatura promedio entre niños alimentados con leche materna y leeche de una empresa no es la misma.

# Se cargan los datos.

materna <- c(87.9 , 92.5, 88.1, 89.9, 87.8, 92.6, 88.0, 91.2, 86.8, 87.0, 88.0, 91.3, 89.2, 88.2)
formula <- c(91.7, 87.9, 87.6, 88.1, 89.2, 87.4, 93.8, 89.1, 88.4, 89.4, 91.8, 87.0, 87.7, 89.5, 87.9, 88.2)
# Creamos una tabla de datos combinada para analizar los intervalos de confianza.
combinada <- c(87.9 , 92.5, 88.1, 89.9, 87.8, 92.6, 88.0, 91.2, 86.8, 87.0, 88.0, 91.3, 89.2, 88.2, 91.7, 87.9, 87.6, 88.1, 89.2, 87.4, 93.8, 89.1, 88.4, 89.4, 91.8, 87.0, 87.7, 89.5, 87.9, 88.2)
# Se obtiene el largo de la muestra por cada una.

largoMaterna <- c(length(materna))
largoFormula <- c(length(formula))

#Ahora se debe comprobar la normalidad de los datos si es que se 
# desea implementar bootstrapping.

print(shapiro.test(materna))
print(shapiro.test(formula))
#Se observa que los valores de p en las muestras normalizadas se encuentran en 
# los valores de normalidad esperado, por lo que se cumple este criterio.


#Se establece que es un alfa (nivel de significación) además de la cantidad de
# repeticiones (remuestreos a usar).
alfa <- 0.05
B <- 3999

media <- function(valores, i){
  mean(valores[i])
}

# Se construye la distribución bootstrap usando el paquete "boot"
#acá se implementan todos los valores numericos.
distribucionTotal <- boot(combinada, statistic = media, R = B)
print(distribucionTotal)
# Se grafica la distribución 
print(plot(distribucionTotal))
# Se obtienen las medias
mediaMaterna <- mean(materna)
mediaFormula <- mean(formula)
# Se obtiene la diferencia de media 
diferencia <- mediaMaterna - mediaFormula
valorNulo <- 0.5

# Se crea la distribución Boots
distribucionBoots <- two.boot(materna,
                              formula,
                              FUN = mean,
                              R = B)
# Se ve la distribución con el valor nulo
distribucionNula <- distribucionBoots[["t"]] - valorNulo

# Se calcula el valor de p.
p <- (sum(abs(distribucionNula) > abs(diferencia)) + 1) / (B + 1)
cat ("Valor p:", p)

#RESPUESTA 
# Con un valor p = 0.8685 se observa que el valor es superior al nivel de
# significancia alfa = 0.05, por lo que se acepta la hipotesis nula, dando a 
# entender de que no existen diferencias en la estatura promedio.