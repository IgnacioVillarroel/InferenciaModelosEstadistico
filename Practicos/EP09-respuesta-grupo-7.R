############# PR�CTICO 9 #############
#Alumnos: Mat�as Bozo - Aylin Rodriguez - Ignacio Villarroel.
library(tidyverse)
library (ggplot2)
library (ggpubr)
library (ez)
library(emmeans)
library(nlme)

# Un equipo de investigadores del �rea de interacci�n humano-informaci�n est� estudiando si el �rea 
# tem�tica y el nivel de dificultad del problema de informaci�n influyen en el tiempo (en segundos) 
# que toma un usuario en formular una consulta de b�squeda para resolver dicho problema. Para ello, 
# han reclutado a un grupo de participantes voluntarios, asignados aleatoriamente a distintos grupos. 
# Cada participante debe resolver tres problemas de informaci�n con diferentes niveles de dificultad: 
# baja, media y alta. A su vez, cada grupo debe resolver problemas relacionados a una tem�tica diferente

# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que 
# tardan los usuarios en formular consultas para problemas con diferente nivel de dificultad en el 
# �rea de literatura.

# Se cargan los datos.
datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# Dado que son muestras correlacionadas, se piensa utilizar ANOVA de una v�a para
# muestras correlacionadas.

# Formulaci�n de hip�tesis:
# H0: El tiempo que tardan los usuarios en formular consultas en el �rea de literatura, es igual para todos los niveles de dificultad.
# HA: El tiempo que tardan los usuarios en formular consultas en el �rea de literatura, es diferente para al menos un nivel de dificultad.

# Verificaci�n de cumplimiento de condiciones para usar ANOVA de una v�a para
# muestras correlacionadas.

# 1. Se verifica la priemera condici�n, puesto que el tiempo, como toda magnitud 
# f�sica, tiene una escala de intervalos iguales.

# 2. Dado que las muestras provienen de un equipo de investigadores y estas representan
# menos del 10% de la poblaci�n, por lo que se puede decir que las muestras
# son obtenidas de manera aleatoria e independiente desde la poblaci�n de origen.

# 3.- Se debe comprobar que las poblaciones provienen desde una distribuci�n
# normal, para ello se realiza un gr�fico Q-Q, donde se pueden observar 
# valores at�picos, por lo tanto se debe ser cauteloso y es por ello que se
# un alfa de 0.01.

# 4.- Al realizar la prueba de ANOVA (ezANOVA) y analizando los resultados
# de la prueba de esfericidad de la prueba de Mauchly, se obtiene un p-valor
# mucho menor que cualquier nivel de significaci�n, por lo que no se cumple
# que la matriz de varianza-covarianza es esf�rica, es decir, existen diferencias
# significativas entre las varianzas.

datos[["area"]] <- factor(datos[["area"]])
datos[["id"]] <- factor(1:nrow(datos))
liter <- datos %>% filter(area == "Literatura")
instancia <- factor(1:200)
dificultadBaja <- liter %>% filter (dificultad == "Baja")
dificultadMedia <- liter %>% filter (dificultad == "Media")
dificultadAlta <- liter %>% filter (dificultad == "Alta")

muestraalta <- dificultadAlta[["tiempo"]]
muestramedia <- dificultadMedia[["tiempo"]]
muestrabaja <- dificultadBaja[["tiempo"]]

datos <- data.frame(instancia, muestrabaja, muestramedia, muestraalta)

# Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("muestraalta", "muestramedia", "muestrabaja"),
                                names_to = "dificultad", values_to = "tiempo")

#Comprobaci�n de la normalidad.
g <- ggqqplot(datos, x = "tiempo", y = "dificultad", color = "dificultad")
g <- g + facet_wrap(~ dificultad)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

#Procedimiento ANOVA con aov.
cat("Procedimiento ANOVA usando aov\n\n")

prueba <- aov(tiempo ~ dificultad + Error(instancia/(dificultad)),
              data = datos)

print(summary(prueba))

# Procedimiento ANOVA con ezANOVA().
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")

prueba2 <- ezANOVA(data = datos, dv = tiempo, within = dificultad, 
                   wid = instancia, return_aov = TRUE)

print(summary(prueba2$aov))
cat("\n\nPero ezANOVA entrega más información.\n")
cat("El resultado de la prueba de esfericidad de Mauchly:\n\n")
print(prueba2[["Mauchly's Test for Sphericity"]])

cat("\n\nY factores de corrección para cuando no se cumple la\n")
cat("condición de esfericidad:\n\n")
print(prueba2$`Sphericity Corrections`)

# Gr�fico del tama�o del efecto.
g2 <- ezPlot(data = datos, dv = tiempo, wid = instancia, within = dificultad,
             y_lab = "Tiempo promedio de ejecuci�n [ms]", x = dificultad)

print(g2)

# Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["dificultad"]],
                              p.adj = "bonferroni", paired = TRUE)

cat("Correcci�n de Bonferroni\n")
print(bonferroni)

# Procedimiento post-hoc de Holm.
holm <- pairwise.t.test(datos[["tiempo"]], datos[["dificultad"]],
                        p.adj = "holm", paired = TRUE)

cat("\n\nCorrecci�n de Holm\n")
print(holm)

# Procedimiento post-hoc HSD de Tukey.
mixto <- lme(tiempo ~ dificultad, data = datos, random = ~1|instancia)
medias <- emmeans(mixto, "dificultad")
tukey <- pairs(medias, adjust = "tukey")
plot(tukey, col="red", las=1,cex.axis=0.5,cex.lab=0.5,cex=0,5)

cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)

# Procedimiento post-hoc de Scheff�
cat("\n\nComparaci�n de Scheff�\n")
scheffe <- pairs(medias, adjust = "scheffe")
print(scheffe)

# Considerando el ajuste para m�ltiples pruebas de Tukey, el p-valor para todas las
# comparaciones (0.0001), son inferiores al nivel de significaci�n definido (0.01), 
# por lo que se rechaza la hip�tesis nula en favor de la hip�tesis alternativa. 
# Podemos concluir  con un 99% de confianza, que el tiempo que tardan los usuarios
# en formular consultas en el �rea de literatura, es diferente para todos los niveles 
# de dificultad.