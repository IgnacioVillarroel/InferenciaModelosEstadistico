######Ejercicio Practico 3#######
# Grupo 7
# Alumnos: Aylin Rodriguez - Ignacio Villarroel

library(dplyr)
library(tidyr)
library(readr)
library(ggpubr)

poblacion <- read.csv2(file.choose(), stringsAsFactors = FALSE, encoding = "UTF-8")
#basename <- "Casen 2017.csv"
#file <- file.path(dir, basename)\\casen.csv
#poblacion <- read.csv(file = file)

# ACTIVIDAD 1
tamanio <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamanio.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamanio.podado )
set.seed(5000)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# Distribución Z
distribucionZ <- (ingreso.normal - media.ingreso) / sd.ingreso
plot(density(distribucionZ), main = "Distribucion Z")


# Distribución Chi Cuadrado
# 3 Grados de libertad.
grados1 <- 3
chi.cuadrado1 <- c(1:5000)
for (i in 1:5000) {
  chi.cuadrado1[i] <- distribucionZ[i]^2 + sum((distribucionZ[1:grados1]^2), na.rm = TRUE)
}
plot(density(chi.cuadrado1), main = "Distribucion chi-cuadrado con 3 grados de libertad")

# 14 grado de libertad.
grados2 <- 14
chi.cuadrado2 <- c(1:5000)
for (i in 1:5000) {
  chi.cuadrado2[i] <- distribucionZ[i]^2 + sum((distribucionZ[1:grados2]^2), na.rm = TRUE)
}
plot(density(chi.cuadrado1), main = "Distribucion chi-cuadrado con 14 grados de libertad")

# Distribución F
distribucionF <- (((chi.cuadrado1) / grados1) / ((chi.cuadrado2) / grados2))
plot(density(distribucionF), main = "Distribucion F")

#############################################################################

#ACTIVIDAD 2
set.seed(5000)
n.repeticiones <- 20
ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)
veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)
exitos <- sum(veinte.repeticiones)
probExito <- exitos / n.repeticiones #(p)
probFracaso <- 1 - probExito #(1-p)

# DISTRIBUCIÓN BINOMIAL
distribucionBin <- 1:n.repeticiones
for (i in 1:n.repeticiones) {
  #n! / (k!(n-k )!)
  combinatoria <- factorial(n.repeticiones) / (factorial(i) * factorial(n.repeticiones - i)) 
  distribucionBin[i] <- (combinatoria*(probExito)^i)*((probFracaso)^(n.repeticiones - i))
}

names(distribucionBin) <- 1:n.repeticiones
barplot(distribucionBin, main = "Distribucion Binomial")

# DISTRIBUCIÓN GEOMÉTRICA
distribucionGeo <- 1:n.repeticiones
for (i in 1:n.repeticiones) {
  #f(x)=((1-p)^(x-1))*p
  distribucionGeo[i] <- ((probFracaso)^(i-1))*probExito
}
names(distribucionGeo) <- 1:n.repeticiones
barplot(distribucionGeo, main = "Distribucion Geometrica")

# DISTRIBUCIÓN BINOMIAL NEGATIVA
distribucionBinNeg <- 1:n.repeticiones
for (i in 1:n.repeticiones) {
  combinatoriaNeg <- factorial(n.repeticiones - 1)/(factorial(i-1) * factorial(n.repeticiones - i))
  distribucionBinNeg[i] <- (combinatoriaNeg*(probExito)^i)*((probFracaso)^(n.repeticiones - i))
}
names(distribucionBinNeg) <- 1:n.repeticiones
barplot(distribucionBinNeg, main = "Distribucion binomial negativa")
