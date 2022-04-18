############# PRÁCTICO 6 #############
#Alumnos: Matías Bozo - Aylin Rodriguez - Ignacio Villarroel.

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)


if (!require(Hmisc)){
  install.packages("Hmisc", dependencies = TRUE )
  require (Hmisc)
}

############################# PREGUNTA 1 #######################################
# Estudios previos habían determinado que la proporción de autoras en la 
# especialidad de neurología era de 32%. ¿Respaldan estos datos tal estimación?
################################################################################
#                 PRUEBA DE WILSON PARA UNA PROPORCIÓN
################################################################################

# R: se plantean las hipótesis:

# H0: p = 0.32
# H1: p <> 0.32

# Fijar valores conocidos.
autoresNeurologia <- 44 + 62
probExito <- 44/autoresNeurologia
valorNulo <- 0.32
alfa <- 0.05
fracasos <- (1 - probExito) * autoresNeurologia
# Calcular cantidad de éxitos.
exitos <- probExito * autoresNeurologia

# Realizar prueba de Wilson.
pruebaWilson <- prop.test(exitos, 
                       n = autoresNeurologia, 
                       p = valorNulo,
                       alternative = "greater",
                       conf.level = 1 - alfa)
print(pruebaWilson)

# Por lo tanto, a partir del p obtenido [0,02304] se puede ver que es menor al 
# valor de significación [0,05], entonces se puede rechazar la hipótesis nula 
# a favor de la alternativa. Los datos no respaldan tal afirmación.


############################# PREGUNTA 2 #######################################
# Según estos datos, ¿es igual la proporción de autoras en las áreas de
# anestesiología y obstetricia?
################################################################################
#          MÉTODO DE WALD PARA LA DIFERENCIA ENTRE DOS PROPORCIONES
################################################################################

# R: se plantean las hipótesis:

# H0: pAnest - pObs = 0
# H1: pAnest - pObs <> 0

#Fijar valores conocidos.
autoresObs <- 71 + 66
autoresAnest <- 21 + 40
exitoObs <- 71
exitoAnest <- 21
alfa <- 0.05
valor_nulo <- 0

# Calcular probabilidades.
probexitoObs <- exitoObs / autoresObs
probexitoAnest <- exitoAnest / autoresAnest

# Estimar diferencia.
diferencia <- abs(probexitoObs - probexitoAnest)

# Construcción de intervalo de confianza.
errorObs <- (probexitoObs * (1- probexitoObs)) / autoresObs
errorAnest <- (probexitoAnest * (1- probexitoAnest)) / autoresAnest
error_est <- sqrt(errorObs + errorAnest)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba de Hipótesis.
p_agrupada <- (exitoObs + exitoAnest) / (autoresAnest + autoresObs)
errorObs <- (p_agrupada * (1 - p_agrupada)) / autoresObs
errorAnest <- (p_agrupada * (1 - p_agrupada)) / autoresAnest
error_est_hip <- sqrt( errorObs + errorAnest )
Z <- (diferencia - valor_nulo) / error_est_hip
p <- 2 * pnorm(Z, lower.tail = FALSE)
cat ("HipÃ³tesis alternativa bilateral \n")
cat ("Z =", Z, "\n")
cat ("p =", p)

# Por lo tanto, a partir del p obtenido [0,02343] se puede ver que es menor al 
# valor de significación [0,05], entonces se puede rechazar la hipótesis nula 
# a favor de la alternativa. La proporción de autoras en las áreas de
# anestesiología y obstetricia no es igual.

############################# PREGUNTA 3 #######################################
# Suponiendo que la diferencia en la proporción de autoras en la especialidad 
# de psiquiatría y la de medicina interna es de 0,23. ¿A cuántos autores 
# deberíamos monitorear para obtener un intervalo de confianza del 97,5% y 
# poder estadístico de 90%, si se intenta mantener aproximadamente la misma 
# proporción de gente estudiada en cada caso?
################################################################################

# Fijar valores conocidos.
diferencia <- 0.23
int_conf <- 0.975
poder <- 0.9
autoresPsiq <- 72
autoresMed <- 45 + 65
exitoPsiq <- 30
exitoMed <- 45

# Calcular probabilidades.
probExitoPsiq <- exitoPsiq / autoresPsiq
probExitoMed <- exitoMed / autoresMed

# Calcular los tamaños de cada grupo.
cant_gente <- bsamsize(p1 = probExitoPsiq,
                       p2 = probExitoMed,
                       fraction = autoresPsiq / (autoresPsiq + autoresMed),
                       alpha = 0.025,
                       power = 0.9)
print(cant_gente)

# Se debería monitorear 86718 personas en la especialidad de Psiquiatría 
# y 132487 personas en la especialidad de Medicina para obtener un  intervalo 
# de confianza del 97,5% y poder estadístico de 90%, intentando mantener 
# aproximadamente la misma proporción de gente estudiada en cada caso.