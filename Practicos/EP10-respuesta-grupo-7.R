############# PRÁCTICO 10 #############
#Alumnos: Matías Bozo - Aylin Rodríguez - Ignacio Villarroel.
library(tidyverse)
library (ggplot2)
library (ggpubr)
library (ez)
library(emmeans)
library(nlme)

# Ñami-Ñam, compañía dedicada a la elaboración y comercialización de golosinas, se prepara para lanzar 
# una nueva línea de productos al mercado. Para asegurar el éxito comercial, ha solicitado a varias 
# empresas de diseño la creación de un empaque para cada uno de los nuevos productos. A fin de decidir 
# qué envase es mejor para cada producto y evaluar un contrato permanente con una de las empresas de 
# diseño, Ñami-Ñam ha reclutado a 2.000 voluntarios de todo el país, seleccionados aleatoriamente entre 
# los participantes de un concurso efectuado por Ñami-Ñam el año anterior. Cada participante debe puntuar 
# las distintas alternativas de envase para un producto (seleccionado al azar) mediante una escala Likert 
# de 7 puntos, donde: 1: el envase es muy poco atractivo y 7: el envase es muy atractivo.

######################## PREGUNTA 1 #####################
# ¿Existe diferencia en la puntuación obtenida por los envases diseñados por PackPro según las 
# evaluaciones realizadas por niños y adultos?

# Dado que se tienen más de 2 muestras correlacionadas y se pide determinar si 
# existe alguna diferencia en la puntuación obtenida por envases diseñados por PackPro, 
# por esto se utilizará la prueba de Kruskal-Wallis.

# Formulación de hipótesis:
# H0: Las puntuaciones obtenidas por los envases diseñados por PackPro según las evaluaciones 
# realizadas por niños y adultos son iguales.
# HA: Las puntuaciones obtenidas por los envases diseñados por PackPro según las evaluaciones 
# realizadas por niños y adultos no son iguales.

# Cumplimento de condiciones para usar la prueba de Kruskal-Wallis:
# 1. La variable independiente corresponde al producto, y es categórica porque puede 
# tomar como valores cualidades o categorías, que en este caso son "Alfajor", "Caramelo",
# "Chocolate", "Cuchufli", "Galleta" y "Queque". Con lo anterior, se cumple que debe tener a 
# 2 niveles (k = 6), por lo que se cumple la primera condición. 

# 2.- La escala de la variable dependiente (Puntaje)
# es ordinal, ya que, corresponde a una escala Likert del 1 al 7, por lo que 
# sería posible ordenar los valores de mayor a menor. 

# 3.- Dado que las muestras provienen de un estudio confiable, se puede 
# asumir que la muestra es aleatoria e independiente de la población.

# Como se cumplen las condiciones, se procede a usar la prueba de 
# Kruskal-Wallis. 

# Se obtienen los datos
datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

packPro <- datos %>% filter(Diseno == "PackPro")
adult <- packPro %>% filter(Edad == "Adulto")
ninio <- packPro %>% filter(Edad == "Nino")

edad <- rbind(adult, ninio)

#Comprobación de la normalidad.
g <- ggqqplot(edad, x = "Puntaje", y = "Producto", color = "Producto")
g <- g + facet_wrap(~ Producto)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Se fija el Valor del nivel de significación.
alfa <- 0.05

# Se comprueba que no existe la normalidad.
prueba <- kruskal.test(Puntaje ~ Producto, data = edad)
print(prueba)

# Procedimientos Post Hoc de Holm en caso de que p-value sea menor que 
# el valor de alfa
if(prueba$p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(edad$Puntaje,
                                   edad$Producto,
                                   p.adjust.method = "holm",
                                   paired = FALSE)
  
  print(post_hoc)
}

# CONCLUSIONES
# El valor p = 0.8047 es mayor al nivel de significación alfa = 0.01,
# por lo que la hipótesis nula es verdadera y se podría decir con un 99% 
# de confianza que no existen diferencia en la puntuación obtenida por los envases diseñados por 
# PackPro según las evaluaciones realizadas por niños y adultos. 
# Al ser p > alfa, entonces no se realiza el análisis post-hoc.

######################## PREGUNTA 2 #####################
# ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes 
# envases de caramelos? De ser así,¿cuál(es) envase(s) se diferencia(n) de 
# los demás?

# Dado que se tienen 4 muestras correlacionadas y se pide determinar si 
# existen diferencias entre los envases de caramelos, se utilizará
# la prueba de Friedman.

# Formulación de hipótesis:
# H0: Los puntajes obtenidos son similares para todos los envases de caramelos.
# HA: Al menos uno de los envases de caramelos obtiene un puntaje distinto a
# los demás.

# Cumplimento de condiciones para usar la prueba de Friedman:
# 1.- La variable independiente corresponde al algoritmo, la cual
# es categórica porque puede tomar como valores cualidades o categorías, que
# en este caso son "DisenoColor", "KoolDesign", "LaKajita" y "PackPro". Se puede decir además que tiene
# 4 niveles (k >= 3), por lo que se cumple la primera condición. 

# 2.- La escala de la variable dependiente (Puntaje)
# es ordinal, ya que, corresponde a una escala Likert del 1 al 7, por lo que 
# sería posible ordenar los valores de mayor a menor. 

# 3.- Dado que las muestras provienen de un estudio confiable, se puede 
# asumir que la muestra es aleatoria e independiente de la población.

# Como se cumplen las condiciones, se procede a usar la prueba de 
# Friedman. 

# Se reorganizan los datos para poder trabajarlos
caramel <- datos %>% filter(Producto == "Caramelos")
disenocolor <- caramel %>% filter(Diseno == "DisenoColor")
koolDesign <- caramel %>% filter(Diseno == "KoolDesign")
lakajita <- caramel %>% filter(Diseno == "LaKajita")
packpro <- caramel %>% filter(Diseno == "PackPro")
usuario <- factor(1:313)
muestradisenocolor <- disenocolor[["Puntaje"]]
muestrakooldesign <- koolDesign[["Puntaje"]]
muestralakajita <- lakajita[["Puntaje"]]
muestrapackpro <- packpro[["Puntaje"]]

datos <- data.frame(instancia, muestradisenocolor, muestrakooldesign, muestralakajita, muestrapackpro)

# Se llevan los datos obtenidos a formato largo
datos <- datos %>% pivot_longer(c("muestradisenocolor", "muestrakooldesign", "muestralakajita", "muestrapackpro"),
                                names_to = "diseno", values_to = "puntaje")

datos[["diseno"]] <- factor(datos[["diseno"]])
datos[["instancia"]] <- factor(datos[["instancia"]])

# Gráfico del tamaño del efecto
g2 <- ezPlot(data =datos, dv = puntaje, wid = instancia, between = diseno, y_lab = "Puntaje obtenido",
             x = diseno)
print(g2)

pruebaf <- friedman.test(puntaje ~ diseno | instancia, data = datos)
print(pruebaf)

# Se fija el nivel de significación
alfa <- 0.05

# Procedimientos Post Hoc de Holm en caso de que p-value sea menor que 
# el valor de alfa
if (prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$puntaje,
                                   datos$diseno,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  print(post_hoc)
}

# CONCLUSIONES
# El valor p = 0.116 es mayor al nivel de significación alfa = 0.05,
# por lo que la hipótesis nula es verdadera y se podría decir con un 95% 
# de confianza que no existen diferencia en la puntuación obtenida por los envases de caramelos
# según las puntajes obtenidos. 
# Al ser p > alfa, entonces no se realiza el análisis post-hoc.