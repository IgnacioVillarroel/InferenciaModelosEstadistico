############# PR?CTICO 11 #############
#Alumnos: Matias Bozo - Aylin Rodriguez - Ignacio Villarroel.
require(ez)
library(ggpubr)
library(tidyr)
library(dplyr)
library(boot)
library(simpleboot)
library(bootES)
# Como hab韆mos visto a comienzos del semestre, la Encuesta de Caracterizaci髇 Socioecon髆ica
# Nacional, Casen, es realizada por el Ministerio de Desarrollo Social de forma peri骴ica para conocer la
# situaci髇 de los hogares chilenos con relaci?n a aspectos demogr?ficos, de educaci?n, salud, vivienda,
# trabajo e ingresos. Es la principal fuente de informaci?n para estimar la magnitud de la pobreza y la
# distribuci髇 del ingreso en el pa韘.

#ENTRADA: int (muestraA) + int (muestraB) + int (repeticiones) + estad?stico (FUN) + plot...
#SALIDA: gr醘ico de distribuci髇.

contrastar_hipotesis_permutaciones <- function(muestraA, muestraB, repeticiones, FUN, alternative, plot, ...){
  cat("Prueba a partir de permutaciones\n")
  cat("Elecci?n para prueba de Hipotesis alternativa", alternative, "\n")
  observado <- calcular_diferencia(muestraA, muestraB, FUN)
  cat("Valor observado:", observado, "\n")
  distribucion <- rep(NA, repeticiones)
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestraA, muestraB, FUN)}
  if(plot){
    graficar_distribucion(distribucion, ...)}
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  cat("Valor p: ", valor_p, "\n\n")}


#DESCRIPCI?N: Funci髇 que obtienes la diferencia de promedios.
#ENTRADA: int (muestraA) + int (muestraB) + estad?stico (FUN)
#SALIDA: int (diferencia)
calcular_diferencia <- function(muestraA, muestraB, FUN){
  diferencia <- FUN(muestraA) - FUN(muestraB)
  return(diferencia)}

#DESCRIPCI?N: Funci髇 que realiza el c?lculo de la permutaci?n y obtiene el estad?stico.
#ENTRADA: int (muestraA) + int (muestraB) + estad?stico (FUN)
#SALIDA: int (estad?stico)
permutar <- function(muestraA, muestraB, FUN){
  A <- length(muestraA)
  B <- length(muestraB)
  
  permutacion <- sample(c(muestraA, muestraB), size = A + B, replace = FALSE)
  permutacionA <- permutacion[1 : A]
  permutacionB <- permutacion[A + 1 : B]
  return(calcular_diferencia(permutacionA, permutacionB, FUN))}

# DESCRIPCI?N: Funci髇 que calcula el valor de p a partir del estad韘tico y su distribuci髇.
# ENTRADA: int (distribuci髇) + int(valorObs) + int (repeticiones) + graph (alternative)
# SALIDA: int (p)

calcular_valor_p <- function(distribucion, valorObs, repeticiones, alternative){
  if (alternative == "two.sided"){
    numerador <- sum (abs(distribucion) > abs(valorObs)) + 1
    denominador <- repeticiones + 1
    p <- numerador / denominador }
  else if(alternative == "greater"){
    numerador <- sum(abs(distribucion) > valorObs) + 1
    denominador <- repeticiones
    p <- numerador / denominador}
  else{
    numerador <- sum(distribucion < valorObs) + 1
    denominador <- repeticiones + 1
    p <- numerador / denominador}
  return(p)}


# Funci髇 para graficar una distribuci?n.
# Argumentos :
# - distribucion : distribuci?n nula del estad韘tico de inter閟.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.

graficar_distribucion <- function(distribucion, ...){
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion", xlab = "Estad?stico de inter?s", ylab = "Frecuencia", ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...) 
  
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}


# Funci髇 para hacer emuestreo usando bootstrapping
my_boot <- function(x){
  # Se toma una muestra con reemplazo para cada grupo
  i_casado <- sample(1:n_casado, replace = TRUE) 
  i_viudo <- sample(1:n_viudo, replace = TRUE)
  i_soltero <- sample(1:n_soltero, replace = TRUE)
  i_separado <- sample(1:n_separado, replace = TRUE)
  rbind(casado[i_casado,], viudo[i_viudo,], soltero[i_soltero,], separado[i_separado,])
}

# Funci髇 para obtener el estad韘tico F 
my_F <- function(frame){
  anova <- ezANOVA(frame, dv = edad, between = ecivil, 
                   wid = instancia, return_aov = FALSE)
  invisible(anova$ANOVA$F)
}

# Funci髇 para generar la distribuciones de la diferencia de medias a
# partir de las permutaciones.
distribucion_diferencias <- function(permutaciones, columna_1, columna_2){
  R <- length(permutaciones)
  distribucion <- c()
  for(i in 1:R){
    datos <- as.data.frame(permutaciones[i])
    muestra_1 <- datos %>% filter(ecivil == columna_1)
    muestra_2 <- datos %>% filter(ecivil == columna_2)
    diferencia <- calcular_diferencia(muestra_1[["edad"]], muestra_2[["edad"]], mean)
    distribucion <- c(distribucion, diferencia)
  }
  return(distribucion)}


######################## PREGUNTA 1 ########################

# Propongan una pregunta de investigaci?n original, que involucre la comparaci?n de las medias de dos
# grupos independientes (m?s abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulaci?n
# Monte Carlo.

# Enunciado:
# Un estudiante del curso de Inferencia de modelos estadisticos de la Universidad de Santiago
# de Chile desea saber si la edad media de las personas heterosexuales que viven en la regi贸n de Valpara韘o
# es la misma para aquellas heterosexuales que viven en la regi髇 del Bio B韔.

# Para resolver este problema se piensa utilizar prueba de permutaciones para 
# comparar una variable continua de dos muestras independientes con P = 1999
# y un alfa = 0.05.

# Estad韘tico de inter閟: la media de la edad de las personas
# Hip?tesis a contrastar
# H0: La edad media de las personas heterosexuales que viven en la Regi髇 de Valpara韘o y en la Regi贸n
# del Biob铆o es la misma.

# HA: La edad media de las personas heterosexuales que viven en la Regi贸n de Valpara韘o y en la Regi贸n
# del Biob铆o es distinta.

# Denotando como uA al promedio de las edades de las personas heterosexuales en la Regi贸n
# de Valpara铆so, y uB al promedio de las edades de las personas heteroexuales en la Regi贸n del Biob铆o,
# entonces matem谩ticamente las hip贸tesis quedan expresadas como:
# H0: uA - uB = 0 
# HA: uA - uB =/ 0 

set.seed(524)

# Fijamos un nivel de significaci?n.
alpha <- 0.05

datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# Se filtra para obtener los datos de la personas de que viven en la 
# Regi贸n de Valpara铆so y la Regi贸n del Biob铆o.
valpo <- datos %>% filter(region == "Regi贸n de Valpara铆so")
biobio <- datos %>% filter(region == "Regi贸n del Biob铆o")
# Se filtra para obtener los datos de las personas heterosexuales en la 
# Regi贸n de Valpara铆so.
heteroValpo <- valpo %>% filter(r23 == "Heterosexual (Atracci贸n hacia el sexo opuesto)")

# Se filtra para obtener los datos de las personas heterosexuales en la 
# Regi贸n del Biob铆o.
heteroBiobio <- biobio %>% filter(r23 == "Heterosexual (Atracci贸n hacia el sexo opuesto)")

# Se tabula que se vea la edad de las personas que se necesita.
edadheteroValpo <- heteroValpo[["edad"]]
edadheteroBiobio <- heteroBiobio[["edad"]]

# Se crean las tablas de las muestras para trabajar.
muestraheteroValpo <- sample(edadheteroValpo, 260)
muestraheteroBiobio<- sample(edadheteroBiobio, 300)

R =  1999

#Se ve la permutaci?n para graficar su normalidad.
contrastar_hipotesis_permutaciones(muestraheteroValpo, 
                                   muestraheteroBiobio, 
                                   repeticiones = R, 
                                   FUN = mean, 
                                   alternative = "two.sided", 
                                   plot = TRUE, 
                                   color = "red", 
                                   fill = "red")

##################### CONCLUSIONES PREGUNTA 1 #####################

# Dado que se obtiene un valor p = 0.5475 > alfa = 0.05, se acepta la hip贸tesis
# nula. Por lo tanto, se puede concluir con un 95% de confianza que la edad media de las personas 
# heterosexuales en la regi贸n de Valpara铆so y la regi贸n del Biob铆o es la misma.

# Tambi茅n se puede apreciar en los gr?ficos, formados a partir de permutaciones, que los datos tienen 
# una distribuci贸n normal.



# ------------------ Pregunta 2 ------------------
# Propongan una pregunta de investigaci?n original, que involucre la comparaci?n de las medias de m?s de
# dos grupos independientes (m?s abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio acad?mico, aplique un an?lisis post-hoc con bootstrapping
# aunque este no sea necesario.

# Enunciado:
# El investigador Tom York desea saber si la edad media de las personas de la Regi贸n del Maule
# es la misma para aquellos que est谩n casados, solteros, viudos o separados.

# Para este caso se piensa utilizar la t?cnica de remuestreo de
# bootstrapping para m?s de dos muestras independientes, creando una funci?n que haga esto,
# usando alfa = 0.05 y una cantidad de remuestreos P = 1000.

# HIP?TESIS A CONTRASTAR
# H0: La edad media de las personas de la Regi?n del Maule es la misma para aquellos
#    que est谩n casados, solteros, viudos o separados.

# HA: La edad media de las personas de la Regi?n del Maule es diferente al menos en un grupo (aquellos
#    que est谩n casados, solteros, viudos o separados).


library(readxl)
library(dplyr)
library(ggpubr)
library(boot)
library(ez)

datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# Se obtienen las personas que viven en la Regi?n del Maule.
maule <- datos %>% filter(region == "Regi贸n del Maule")

set.seed(523)
# Se obtiene la muestra de tama?o 500
tamano <- 500
muestra <- maule[sample(nrow(maule), tamano),]
edad <- muestra[["edad"]]
ecivil <- factor(muestra[["ecivil"]])
instancia <- factor(1:tamano)
datos2 <- data.frame(instancia, edad, ecivil)

casado <- datos2 %>% filter(ecivil == "Casado(a)")
n_casado <- nrow(casado)

viudo <- datos2 %>% filter(ecivil == "Viudo(a)")
n_viudo <- nrow(viudo)

soltero <- datos2 %>% filter(ecivil == "Soltero(a)")
n_soltero <- nrow(soltero)

separado <- datos2 %>% filter(ecivil == "Separado(a)")
n_separado <- nrow(separado)


# Se obtiene el estad?stico F original
anova_original <- ezANOVA(datos2, dv = edad, between = ecivil,
                          wid = instancia, return_aov = FALSE)
print(anova_original)

# Establecer cantidad de remuestreos
R <- 1000
set.seed(456) # segunda semilla


# lapply guarda en lista
distribucion1  <- lapply(1:R, my_boot)

# sapply guarda en un vector
suppressMessages(suppressWarnings(Fs <- sapply(distribucion1, my_F))) # evitar los warnings


p <- calcular_valor_p(Fs, anova_original$ANOVA$F, R, "two.sided")
print(p)

# CONCLUSIONES

# Usando un alpha de 0.05, en consecuencia se obtiene un valor p = 0.5864136 , p > alpha,
# entonces se acepta la hip?tesis nula. Por lo tanto se puede afirmar con un 95% de confianza
# que la edad media de las personas de la Regi贸n del Maule es igual para todos los grupos
# (aquellos casados, viudos, solteros y separados).

# Independiente del resultado anterior, se calcula igualemente un an谩lisis Post Hoc
# An?lisis Post Hoc


# Se calculan las diferencias observadas entre cada par de muestras
dif_obs_casado_soltero <- calcular_diferencia(casado[["edad"]], soltero[["edad"]], mean)
dif_obs_casado_viudo <- calcular_diferencia(casado[["edad"]], viudo[["edad"]], mean)
dif_obs_casado_separado <- calcular_diferencia(separado[["edad"]], casado[["edad"]], mean)
dif_obs_soltero_viudo <- calcular_diferencia(soltero[["edad"]], viudo[["edad"]], mean)
dif_obs_soltero_separado <- calcular_diferencia(soltero[["edad"]], separado[["edad"]], mean)
dif_obs_viudo_separado <- calcular_diferencia(viudo[["edad"]], separado[["edad"]], mean)

# Generar distribuciones para diferencias entre pares a partir de las
# permutaciones
dif_casado_soltero <- distribucion_diferencias(distribucion1, "Casado(a)", "Soltero(a)")
dif_casado_viudo <- distribucion_diferencias(distribucion1, "Casado(a)", "Viudo(a)")
dif_casado_separado <- distribucion_diferencias(distribucion1, "Separado", "Casado(a)")
dif_soltero_viudo <- distribucion_diferencias(distribucion1, "Soltero(a)", "Viudo(a)")
dif_soltero_separado <- distribucion_diferencias(distribucion1, "Soltero(a)", "Sepadado(a)")
dif_viudo_separado <- distribucion_diferencias(distribucion1, "Viudo(a)", "Sepadado(a)")

# Se calculan el valor de p
num1 <- sum(abs(dif_casado_soltero) > abs(dif_obs_casado_soltero) + 1)
den1 <- R + 1
p_casado_soltero <- num1/den1

num2 <- sum(abs(dif_casado_viudo) > abs(dif_obs_casado_viudo) + 1)
den2 <- R + 1
p_casado_viudo <- num2/den2

num3 <- sum(abs(dif_casado_separado) > abs(dif_obs_casado_separado) + 1)
den3 <- R + 1
p_casado_separado <- num3/den3

num4 <- sum(abs(dif_soltero_viudo) > abs(dif_obs_soltero_viudo) + 1)
den4 <- R + 1
p_soltero_viudo <- num4/den4

num5 <- sum(abs(dif_soltero_separado) > abs(dif_obs_soltero_separado) + 1)
den5 <- R + 1
p_soltero_separado <- num5/den5

num6 <- sum(abs(dif_viudo_separado) > abs(dif_obs_viudo_separado) + 1)
den6 <- R + 1
p_viudo_separado <- num6/den6

# Gr?fico del tama?o del efecto para observar las medias de las edades
g2 <- ezPlot(data =datos2, dv = edad, wid = instancia, between = ecivil, y_lab = "Media de las edades",
             x = ecivil)
print(g2)


# CONCLUSIONES FINALES
# Al realizar el an?lisis Post Hoc, podemos notar que todos los grupos presentan diferencias
# significativas, obteniendo los siguientes valores p:
# p valor entre casado con separado = NA_real_
# p valor entre casado con soltero = 0.3076
# p valor entre casado con viudo = 0.3316
# p valor entre soltero con separado = NA_real_
# p valor entre soltero con viudo = 0.3576
# p valor entre viudo con separado = NA_real_

# Por lo tanto, se puede concluir con un 95% de confianza que la edad media
# es distinta para aquellas personas casadas, viudas, solteras y separadas.

# Adem?s, si se observa el gr?fico del tama?o del efecto, es posible notar
# la gran diferencia que hay entre cada par de grupo, destacando que aquellas personas
# que tienen alrededor de 70 a?os tienden estar viudos siendo esta mayor
# que la edad media de las personas casadas, separads y solteras.