############# PR?CTICO 12 #############
#Alumnos: Matias Bozo - Aylin Rodriguez - Ignacio Villarroel.
library(ggpubr)
library(DescTools)
require(ez)
library(ggpubr)
library(tidyr)
library(dplyr)
library(boot)
library(simpleboot)
library(bootES)
library(WRS2)

######################## PREGUNTA 1 ##########################

# Cargar datos
instanciaA <- c(41, 192, 138, 85, 87, 134, 8, 57, 79, 17, 117, 51, 121, 30, 113, 146, 106, 153, 175, 139)

tiempoA <- c(48705, 251843, 210041, 8576989, 842079, 1510394, 4428151, 834565, 37449, 885722,6701654, 1252837, 6568968, 6684497, 92932, 994830, 120276, 180141, 5743260, 1174562 )

datosA <- data.frame(instanciaA, tiempoA)

# Transformaci贸n de Box-cox
box_cox <- function(x, lambda) {
  if(lambda == 0) {
    return(log(x)) }
  
  resultado <- (x ** lambda -1) / lambda
 
  return(resultado)
}

# Transformaciones de la poblacion
lambda_menos_dos <- box_cox(datosA$tiempoA, -2)
lambda_menos_uno <- box_cox(datosA$tiempoA, -1)
lambda_cero <- box_cox(datosA$tiempoA, 0)
lambda_uno <- box_cox(datosA$tiempoA, 1)
lambda_dos <- box_cox(datosA$tiempoA, 2)

transformaciones <- data.frame(datosA, lambda_menos_dos, lambda_menos_uno,
                               lambda_cero, lambda_uno, lambda_dos)

# Gr谩ficos de dispersion para la transformacion de Box-Cox
gt1 <- ggscatter(transformaciones, x = "tiempoA", y = "lambda_menos_dos",
                 color = "purple", xlab = "tiempo de ejecuci髇",
                 ylab = "lambda = -2") + rotate_x_text(45)

gt2 <- ggscatter(transformaciones, x = "tiempoA", y = "lambda_menos_uno",
                 color = "purple", xlab = "tiempo de ejecuci髇",
                 ylab = "lambda = -1") + rotate_x_text(45)

gt3 <- ggscatter(transformaciones, x = "tiempoA", y = "lambda_cero",
                 color = "purple", xlab = "tiempo de ejecuci髇",
                 ylab = "lambda = 0") + rotate_x_text(45)

gt4 <- ggscatter(transformaciones, x = "tiempoA", y = "lambda_uno",
                 color = "purple", xlab = "tiempo de ejecuci髇",
                 ylab = "lambda = 1") + rotate_x_text(45)

gt5 <- ggscatter(transformaciones, x = "tiempoA", y = "lambda_dos",
                 color = "purple", xlab = "tiempo de ejecuci髇",
                 ylab = "lambda = 2") + rotate_x_text(45)

# Crear una 鷑ica figura con todos los graficos de dispersion.
dispersion  <- ggarrange(gt1, gt2, gt3, gt4, gt5, ncol = 3, nrow = 2)
texto <- "Poblacion transformada por anio"
titulo  <- text_grob(texto, face = "bold", size = 14)
dispersion <- annotate_figure(dispersion, top = titulo)
print(dispersion)

# Buscar la mejor transformacion Box-Cox usando funciones de R.
lambda <- BoxCoxLambda(datosA$tiempoA, lower = -4, upper = 4)
cat("Lambda optimo:", lambda)


transformacion <- BoxCox(datosA$tiempoA, lambda)
datosA <- data.frame(datosA, transformacion)

# Graficar los datos transformados.
g1 <- ggqqplot(transformacion, color = "purple")
print(g1)

g2 <- gghistogram(datosA, bins = 10, x = "transformacion", color = "purple",
                  fill = "purple", xlab = "Poblacion (Box-Cox)",
                  ylab = "Frecuencia") + rotate_x_text(45)

print(g2)

# Grafico de dispersion para la transformacion de Box-Cox
g3 <- ggscatter(datosA, x = "tiempoA", y = "transformacion", color = "purple",
                xlab = "A帽o", ylab = "lambda optimo") + rotate_x_text(45)

print(g3)

# Nuevos datos
datosTA <- datosA[1:10,]
datosTB <- datosA[11:20,]

# Como la muestra es peque馻 (menos de 30 observaciones), ser韆 adecuado usar
# la prueba t de Student para una muestra. Pero antes debemos verificar las
# condiciones.

# Como se trata de instancias diferentes y la muestra representa menos del 10% de
# la poblaci贸n, podemos asumir que las observaciones son independientes entre si.


# Verificar si las muestras se distribuyen de manera cercana
# a la normal.
normalidad_A <- shapiro.test(datosTA$transformacion)
print(normalidad_A)
normalidad_B <- shapiro.test(datosTB$transformacion)
print(normalidad_B)

# Fijando un nivel de significaci贸n de 0.05
# Podemos ver que, la muestra A, obtiene un valor p bastante alto, por 
# otro lado, en la muestra B, el valor p es 0.05533, lo que es mayor al alfa
# fijado por lo que es razonable suponer que ambas muestas provienen de una
# distribuci贸n cercana a la normal.
# De todas formas se procede con cautela y se fija un nivel de significaci贸n en 0.01
# Fijar un nivel de significaci贸n.

alfa <- 0.01

# Las hip髏esis a formular en este caso son:
# H0: no hay diferencia entre ambos algoritmos
# HA: uno de los algoritmos es m醩 r醦ido que otro.
# Aplicar la prueba t para dos muestras independientes.

prueba <- t.test(x = datosTA$transformacion,
                 y = datosTB$transformacion,
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba)

##################### CONCLUSIONES PREGUNTA 1 #####################

# El valor p obtenido es mayor que el nivel de significacion (0,8173 < 0,01),
# por lo que fallamos en rechazar la hipotesis nula.

# En consecuencia, podemos concluir con 95% de confianza que, en promedio, la
# no hay diferencia entre los algoritmos

# Calcular la diferencia entre las medias.
media_A <- mean(datosTA$transformacion)
media_B <- mean(datosTB$transformacion)
diferencia <- media_A - media_B
cat("Diferencia de las medias =", diferencia, "[mg/ml]\n")


############################ PREGUNTA 2 ############################

#Lectura de archivo de entrada.
csv <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# Analice la primera pregunta abordada en el ejercicio 
# pr醕tico 11, con los mismos datos, utilizando un m閠odo 
# robusto adecuado.

# Enunciado:
# Un estudiante del curso de Inferencia y Modelos Estadist韈os 
# de la Universidad de Santiago de Chile desea saber si la 
# edad media de las personas heterosexuales que viven en la 
# regi髇 de Valpara韘o es la misma para las persona heterosexuales 
# que viven en la regi髇 del Biob韔.

# Para resolver este problema utiliza la prueba de Yuen para dos 
# muestras independientes (M閠odo robusto), con un nivel de 
# significaci髇  de 0.05.

# Estad韘tico de inter閟: la media de la edad de las personas.

# Hip髏esis a constrastar.
# H0: La edad media de las personas heterosexuales que viven 
# en la Regi髇 de Valpara韘o y en la Regi髇 del Biob韔 es la misma.

# HA: La edad media de las personas heterosexuales que viven 
# en la Regi髇 de Valpara韘o y en la Regi髇 del Biob韔 es distinta.

# Denotando como uA al promedio de las edades de las personas 
# heterosexuales en la Regi髇 de Valpara韘o, y uB al promedio 
# de las edades de las personas heteroexuales en la Regi髇 del Biob韔,
# entonces, las hip髏esis quedan expresadas matem醫icamente como:
# H0: uA - uB = 0 
# HA: uA - uB =/ 0 


#Fijar semilla.
set.seed(524)

# Filtrar para obtener los datos de las personas de que viven en la 
# Regi髇 de Valpara韘o y la Regi髇 del Biob韔.
valparaiso <- csv %>% filter(region == "Regi髇 de Valpara韘o")
biobio <- csv %>% filter(region == "Regi髇 del Biob韔")

# Filtrar para obtener los datos de las personas heterosexuales en la Regi髇 de Valpara韘o.
heteroValparaiso <- valparaiso %>% filter(r23 == "Heterosexual (Atracci髇 hacia el sexo opuesto)")

# Filtrar para obtener los datos de las personas heterosexuales en la Regi髇 del Biob韔.
heteroBiobio <- biobio %>% filter(r23 == "Heterosexual (Atracci髇 hacia el sexo opuesto)")

# Tabular para ver la edad de las personas que se necesita.
edadHeteroValparaiso <- heteroValparaiso[["edad"]]
edadHeteroBiobio <- heteroBiobio[["edad"]]

# Crear tablas de las muestras para trabajar.
muestraHeteroValparaiso <- sample(edadHeteroValparaiso, 260)
muestraHeteroBiobio<- sample(edadHeteroBiobio, 300)

edad <- c(muestraHeteroValparaiso,muestraHeteroBiobio)
region <- c(rep("Valpara韘o", length(muestraHeteroValparaiso)), rep("BioBio", length(muestraHeteroBiobio)))
datosp2 <- data.frame(edad,region)

# Comprobar normalidad.
g <- ggqqplot(datosp2, x = "edad", facet.by = "region",
              palette = c("blue", "red"), color = "region")
print(g)

# Establecer nivel de significaci髇.
alfa <- 0.05

# Ver poda del 20%.
gamma <- 0.2
n_valparaiso <- length(muestraHeteroValparaiso)
n_biobio <- length(muestraHeteroBiobio)

#Obtener datos truncados.
poda_valparaiso <- n_valparaiso * gamma
poda_biobio <- n_biobio * gamma

muestraHeteroValparaiso_truncada <- muestraHeteroValparaiso[poda_valparaiso:(n_valparaiso - poda_valparaiso)]
muestraHeteroBiobio_truncada <- muestraHeteroBiobio[poda_biobio:(n_biobio - poda_biobio)]

edad <- c(muestraHeteroValparaiso_truncada, muestraHeteroBiobio_truncada)
region <- c(rep("Valparaiso", length(muestraHeteroValparaiso_truncada)), rep("Biobio", length(muestraHeteroBiobio_truncada)))
datos_truncados <- data.frame(edad, region)

# Comprobar normalidad con datos truncados.
g <- ggqqplot(datos_truncados, x = "edad", facet.by = "region",
              palette = c("blue", "red"), color = "region")

print(g)

# Aplicar prueba de Yuen.
prueba <- yuen(edad ~ region, data = datosp2, tr = gamma)
print(prueba)

##################### CONCLUSIONES PREGUNTA 2 #####################
# Despu閟 de truncar ambas muestras con gamma = 0.2 y de construir
# los gr醘icos Q-Q para las muestras podadas, podemos apreciar que, 
# tras la poda, la distribuci髇 de los datos se aproxima m醩 a la 
# normal.
#
# La prueba de Yuen entrega como resultado una diferencia entre las
# medias truncadas de -0.44615, con un intervalo de 95% de confianza
# (-4,1028; 3,2105) y tama駉 del efecto de 0,020. El valor p obtenido,
# p = 0.81045, no es significativo al nivel de significaci髇 definido,
# por lo que concluimos con un 95% de confianza que la edad de 
# las personas heterosexuales que viven en la Regi髇 de Valpara韘o y 
# en la Regi髇 del Biob韔, en promedio, es la misma.



############################ PREGUNTA 3 ############################

# Analice la segunda pregunta abordada en el ejercicio pr醕tico 11, 
# con los mismos datos, utilizando un m閠odo robusto adecuado.

# Enunciado:
# El investigador Tom York desea saber si la edad media de las 
# personas de la Regi髇 del Maule es la misma para aquellos que 
# est醤 casados, solteros, viudos o separados.

#usando alfa = 0.05

# Hip髏esis a constrastar.
# H0: La edad media de las personas de la Regi髇 del Maule es la 
# misma para aquellos que est醤 casados, solteros, viudos o separados.

# HA: La edad media de las personas de la Regi髇 del Maule es
# diferente al menos en un grupo (aquellos que est醤 casados, 
# solteros, viudos o separados).

# Fijar valor para semilla, distinta a la anterior.
set.seed(523)

# Filtro para obtener las personas que viven en la Regi髇 del Maule.
maule <- csv %>% filter(region == "Regi髇 del Maule")

#Obtener muestra de tama駉 500.
tamano <- 500

# Construir data frame.
muestra <- maule[sample(nrow(maule), tamano),]
edad <- muestra[["edad"]]
e_civil <- factor(muestra[["ecivil"]])
instancia <- factor(1:tamano)
datos <- data.frame(instancia, edad, e_civil)

casado <- datos %>% filter(e_civil == "Casado(a)")
edadCasado <- as.vector(t(casado %>% select("edad")))
mediaedadCasado <- mean(edadCasado)

viudo <- datos %>% filter(e_civil == "Viudo(a)")
edadViudo <- as.vector(t(viudo %>% select("edad")))
mediaedadViudo <- mean(edadViudo)

soltero <- datos %>% filter(e_civil == "Soltero(a)")
edadSoltero <- as.vector(t(soltero %>% select("edad")))
mediaedadSoltero <- mean(edadSoltero)

separado <- datos %>% filter(e_civil == "Separado(a)")
edadSeparado <- as.vector(t(separado %>% select("edad")))
mediaedadSeparado <- mean(edadSeparado)

edad <- c(edadCasado,edadViudo,edadSoltero,edadSeparado)
ecivil <- c(rep("casado", length(edadCasado)), rep("viudo", length(edadViudo)), rep("soltero", length(edadSoltero)), rep("separado", length(edadSeparado)))
datosp3 <- data.frame(edad, ecivil)

# Fijar nivel de significaci髇.
alfa <- 0.05

# Comparar las diferentes edades usando medias truncadas.
cat("Comparaci髇 entre grupos usando medias truncadas\n\n")
gamma <- 0.2

medias_truncadas <- t1way(edad ~ ecivil, data = datosp3, tr = gamma,
                          alpha = alfa)

print(medias_truncadas)

if(medias_truncadas$p.value < alfa) {
  cat("\nProcedimiento post-hoc\n\n")
  
  set.seed(523)
  
  post_hoc <- lincon(edad ~ ecivil, data = datosp3, tr = gamma,
                     alpha = alfa)
  
  print(post_hoc)
}


# Comparaci髇 de las diferentes edades usando bootstrap.
cat("Comparaci髇 entre grupos usando bootstrap\n\n")
muestras <- 999

bootstrap <- t1waybt(edad ~ ecivil, data = datosp3, tr = gamma,
                     nboot = muestras)

print(bootstrap)

if(medias_truncadas$p.value < alfa) {
  cat("\nProcedimiento post-hoc\n\n")
  
  set.seed(666)
  
  post_hoc <- mcppb20(edad ~ ecivil, data = datosp3, tr = gamma,
                      nboot = muestras)
  
  print(post_hoc)
}

##################### CONCLUSIONES PREGUNTA 3 #####################
# Deseamos comparar la edad promedio de cuatro grupos de personas:
#   - Personas casadas, con 194 observaciones para el grupo.
#   - Personas viudas, con 52 observaciones para el grupo.
#   - Personas solteras, con 106 observaciones para el grupo.
#   - Personas separadas, con 64 observaciones para el grupo.
# Se ha establecido para este estudio un nivel de significaci髇
# de 0, 05.

# Podemos ver que las funciones t1way() y t1waybt() arrojan el 
# mismo resultado. Puesto que el valor p obtenido, p = 0, menor que 
# el nivel de significaci髇, por lo que rechazamos la hip髏esis 
# nula en favor de la hip髏esis alternativa. Concluimos, entonces,
# con 95% de confianza, que la edad media de las personas de la 
# Regi髇 del Maule es diferente al menos en un grupo de personas
# (casados, solteros, viudos y/o separados).

# Al efectuar los procedimientos post-hoc respectivos, los valores 
# p obtenidos son casi todos iguales para ambos m閠odos. 

# No obstante, en ambos casos podemos concluir que los grupos de 
# personas estudiadas, presentan una edad promedio diferente.

# Si examinamos las medias de cada grupo, tenemos que 
#   - xCasado = 57.92 a駉s
#   - xSeparado = 52.35 a駉s
#   - xSoltero = 42.75 a駉s
#   - xViudo = 71.84 a駉s
# Entonces, el grupo de personas viudas tienen una 
# edad mucho mayor que los otros grupos, que tambi閚 poseen 
# edades distintas (en promedio).
