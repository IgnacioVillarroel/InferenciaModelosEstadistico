############# PRÁCTICO 5 #############
#Alumnos: Aylin Rodriguez - Ignacio Villarroel.

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}
if (!require(pwr)){
  install.packages("pwr", dependencies = TRUE )
  require (pwr)
}
#############    Enunciado    ############# 
# Se sabe que una máquina que envasa detergentes industriales llena bidones con un volumen 
# de producto que sigue una distribución normal con desviación estándar de 1 litro. 
# Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de la planta 
# requiere determinar si la máquina está llenando los bidones con una media de
# 10 litros

#Desviación estandar
s <- 1

#Tamaño de la muestra
t <- 100

mu <- 10
#Error Estándar
SE = s / sqrt (t)

#############    Pregunta 1    ############# 

# Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente una media 
# menor a 9,8 litros o mayor a 10,2 litros, ¿cuál es la probabilidad de que cometa un 
# error de tipo I?

#La probabilidad de cometer un error tipo I corresponde a alfa (nivel de significativa).

#Se establecen las medias para las pruebas de hipótesis.
medianula1 <- 9.8
medianula2 <- 10.2

#Se genera un modelo normal

x <- seq(medianula1 - 5.2 * SE, medianula2 + 5.2 * SE, 0.01) 
y <- dnorm(x, mean = mu, sd = SE)
g <- ggplot (data = data.frame(x, y), aes(x))
g <- g + stat_function(fun = dnorm,
                       args = list(mean = mu, sd = SE),
                       colour = "red", size = 1)
g <- g + ylab ("")
g <- g + scale_y_continuous (breaks = NULL)
g <- g + scale_x_continuous (name = "Ejercicio 1",
                             breaks = seq (medianula1 - 5.2 * SE, medianula2 + 5.2 * SE, 0.2))
g <- g + theme_pubr()

#Se identifican los valores menores a 9.8
g <- g + geom_area(data = subset(g$data, x < medianula1),
                   aes (y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)

#Se identifican los valores mayores a 10.2
g <- g + geom_area(data = subset(g$data, x > medianula2),
                   aes (y = y),
                   colour = "red",
                   fill = "green",
                   alpha = 0.5)

#Linea divisoria
g <- g + geom_vline ( aes(xintercept = mu), color = "red", linetype = 1)

print(g)

#Cantidad de observaciones bajo la medianula1 (9.8)
lowMedia1 <- subset(g$data, x < medianula1)

#Cantidad de observaciones sobre la medianula2 (10.2)
overMedia2 <- subset(g$data, x > medianula2)

#Se realiza una regla de 3 para calcular la probabilidad de estar en los sectores indicados
probLow <- length(lowMedia1$x)/length(x)
probOver <- length(overMedia2$x)/length(x)
#Se calcula la probabilidad total (ambos por separado corresponden a alfa/2 al ser una prueba bilateral)
probTotal <- probLow + probOver


#RESPUESTA: La probabilidad de cometer un error tipo I cuando la muestra presenta una media 
# menor a 9,8 litros o mayor a 10,2 litros es de 0.7241. 


#############    Pregunta 2    ############# 

# Si el verdadero volumen medio de los bidones fuera de 10,1 litros, ¿cuál sería la 
# probabilidad de que el  ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?
#Se calcula el poder

mediaEfecto <- 10.1
poder <- pnorm(medianula2,
               mean = mediaEfecto,
               sd = SE,
               lower.tail = FALSE) + pnorm(medianula1,
                                           mean = mediaEfecto,
                                           sd = SE,
                                           lower.tail = TRUE)

#Se calcula el valor beta (probabilidad de cometer error tipo II)
beta <- 1 - poder

#RESPUESTA: La probabilidad de cometer un error tipo II si el verdadero volumen medio fuera de 10.1 litros, es de
#           0.8399 aproximadamente.

#########ANEXO############
#Se tiene el gráfico de area para error tipo 2. 

#Gráfico área error tipo 2
# y2 <- dnorm(x, mean = 10.1 , sd = SE)
# g1<- ggplot(data = data.frame(x,y), aes(x))
# g1 <- g1 + stat_function(fun = dnorm, n = t, args = list(mean = medianula1, sd = SE), colour = "black", size = 1)
# g2 <- ggplot(data = data.frame(x,y2), aes(x))
# g2 <- g1 + stat_function(fun = dnorm, n = t, args = list(mean = medianula2, sd = SE), colour = "blue", size = 1)
# g2 <- g2 + ylab("Densidad")
# g2 <-g2 + xlab("Dureza")
# g2 <-g2 + labs(title = Área Error tipo 2")
# g2 <- g2 + theme_pubr()
# Zcritico <- qnorm ( 0.05/2 , mean = 10.1 , sd = SE , lower.tail = FALSE )
# qcriticoinferior <- medianula1 - Zcritico
# qcriticosuperior <- medianula2 + Zcritico
# 
# print(g2)




#############    Pregunta 3    #############

# Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico
# con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podría 
# variar de 9,6 a 10,4 litros.

#Se hacen los mismos gráficos que se utilizaron en el ejercicio 1, pero con las nuevas medias.
x <- seq (medianula1 - 5.2 * SE, medianula2 + 5.2 *SE, 0.01)
y <- dnorm (x, mean = mu, sd = SE)
g3 <- ggplot (data = data.frame(x, y), aes(x))
g3 <- g3 + stat_function(fun = dnorm,
                         args = list(mean = mu, sd = SE),
                         colour = "red", size = 1)
g3 <- g3 + ylab ("")
g3 <- g3 + scale_y_continuous (breaks = NULL)
g3 <- g3 + scale_x_continuous (name = "Ejercicio 3",
                               breaks = seq (medianula1 - 5.2 * SE, medianula2 + 5.2 *SE, 0.2))
g3 <- g3 + theme_pubr()

#Se identifican los valores menores a 9.6
g3 <- g3 + geom_area(data = subset(g3$data, x < medianula1),
                     aes (y = y),
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)

#Se identifican los valores mayores a 10.4
g3 <- g3 + geom_area(data = subset(g3$data, x > medianula2),
                     aes (y = y),
                     colour = "red",
                     fill = "green",
                     alpha = 0.5)

medianula3 <- 9.6
medianula4 <- 10.4

#Para 9.6
g3 <- g3 + stat_function(fun = dnorm,
                         args = list (mean = medianula3, sd = SE),
                         colour = "blue", size = 1)
x1 <- seq (medianula1 - 5.2 * SE, medianula2 + 5.2*SE, 0.01)
y1 <- dnorm (x, mean = medianula3 , sd = SE)

g3 <- g3 + geom_area (data = subset (data.frame (x1, y1),
                                     x < medianula3),
                      aes ( x = x1 , y = y1 ),
                      colour = " blue ",
                      fill = " blue ",
                      alpha = 0.5)
g3 <- g3 + geom_area (data = subset (data.frame (x1, y1) ,
                                     x > medianula4 ),
                      aes(x = x1 , y = y1) ,
                      colour = " blue ",
                      fill = " blue ",
                      alpha = 0.5)

#Para 10.4
g3 <- g3 + stat_function(fun = dnorm,
                         args = list (mean = medianula4, sd = SE),
                         colour = "blue", size = 1)
x2 <- seq (medianula1 - 5.2 * SE, medianula2 + 5.2 *SE, 0.01)
y2 <- dnorm (x, mean = medianula4 , sd = SE)
g3 <- g3 + geom_area (data = subset (data.frame (x2, y2),
                                     x < medianula3),
                      aes ( x = x2 , y = y2 ),
                      colour = " blue ",
                      fill = " blue ",
                      alpha = 0.5)
g3 <- g3 + geom_area (data = subset (data.frame (x2, y2) ,
                                     x > medianula4),
                      aes(x = x2 , y = y2) ,
                      colour = " blue ",
                      fill = " blue ",
                      alpha = 0.5)

print(g3)


#Se crea un gráfico donde se observa que mientras mayor sea el tamaño del efecto, 
# mayor será el poder estadístico sobre la muestra.
efecto <- seq(-1, 1, 0.01)
powerEfect <- power.t.test ( n = t,                             
                             delta = efecto,                            
                             sd = s,                             
                             sig.level = probTotal,   
                             power = NULL,
                             type = "one.sample",                            
                             alternative = "two.sided")

datos <- data.frame(efecto, powerEfect$power)
g3_alt <- ggplot(datos, aes(efecto, powerEfect$power))
g3_alt <- g3_alt + geom_line ()
g3_alt <- g3_alt + ylab ("Poder estadístico ")
g3_alt <- g3_alt + xlab ("Tamaño del efecto ")
g3_alt <- g3_alt + theme_pubr()
g3_alt <- g3_alt + ggtitle("Curva de poder para prueba bilateral")
g3_alt <- g3_alt + geom_vline (xintercept = 0, linetype = "dashed")
#se imprime el gráfico
print(g3_alt)

#############    Pregunta 4    ############# 

# Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para 
# conseguir un poder estadístico de 0,7 y un nivel de significación de 0,05?

#Se calcula el tamaño de la muestra utilizando la función power.t.test.

t4 <- power.t.test ( n = NULL,                             
                      delta = mediaEfecto - mu,                            
                      sd = s,                             
                      sig.level = 0.05,                             
                      power = 0.7,                             
                      type = "one.sample",                            
                      alternative = "two.sided") 

#RESPUESTA: Por medio de la aplicación de power.t.test, para conseguir un poder estadístico de 0,7
#            y un nivel de significancia de 0.05, los bidones que deberían revisarse son 620, (se aproximo de 619.12).


#############    Pregunta 5    ############# 

# ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer 
# un error de tipo I a un 1% solamente?

#Se calculará el tamaño de la muestra utilizando la función power.t.test.

t5 <- power.t.test ( n = NULL ,                             
                      delta = mediaEfecto - mu,                            
                      sd = s,                             
                      sig.level = 0.01,                             
                      power = 0.7,                             
                      type = "one.sample",                            
                      alternative = "two.sided")

#RESPUESTA: Por medio de la aplicación de power.t.test, para conseguir un poder estadístico de 0,7
#            y un nivel de significancia de 0.01, los bidones que deberían revisarse 965 (se aproximo de 964.46).
