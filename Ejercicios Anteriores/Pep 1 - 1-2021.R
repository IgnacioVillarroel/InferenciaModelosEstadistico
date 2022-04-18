
library(ggpubr)


# Se carga la semilla la cual fue pedida en el enunciado.
set.seed(523)

# Se cargan los datos a la base de conocimientos, los cuales serán usados para las pruebas pertinentes y una inminente
# inminente inferencia.

## COLOCAR DIRECCION

# A breves cuentas, se desea verificar si el indicie de masa muscular de reclutas se ha reducido ¡En PROMEDIO!
# 2,5[kg/m^2], como han de ser los mismos reclutas puestos a prueba de las observaciones, se van a tratar de datos
# relacionados o dependientes.

# Para esto y como se desea disernir o afirmar este baja PROMEDIO se hará uso de la prueba T para muestras pareadas.
# Ya que serán los mismos reclutas en dos instancias distintas.

# Por tanto las hipotesis a contrastar serán:
# HIPOTESIS:
#
# H0 : Hubo un cambio promedio igual a 2.5 [Kg/m^2]
# Ha : Hubo un cambio promedio distinto a 2.5 [Kg/m^2]
#
# Lo cual se traduce a
#
# H0 : El promedio de las diferencias = 2.5
# Ha : El promedio de las diferencias != 2.5
#
# Para empezar se revisa si se cumple que las diferencias de ambas muestras pertenecen a una distribución normal.

alfa <- 0.05
valor_nulo <- 2.5

antes <- datos[["imc_1"]]
despues <- datos[["imc_2"]]
diferencia <- antes - despues

tamano_muestra <- 40
muestra_diferencias<- sample(diferencia, tamano_muestra)

normalidad <- shapiro.test(muestra_diferencias)
print(normalidad)

# Como la prueba de shapiro para lograr saber si las diferencias tienen una forma aproximada a la muestral se logró
# encontrar que obtenemos un p mucho mayor al nivel de signicación por lo que se falla al rechazar la hipotesis nula y se afirma
# con un 95% de seguridad que las diferencias tienen una distribución cercana a la normal.

prueba_t <- t.test(muestra_diferencias,
                   mu = valor_nulo,
                   alternative = "less",
                   conf.level = 1 - alfa
)

# Se realiza una prueba solo con la cota más baja ya que despues de un entrenamiento es logico que los reclutas y se
# comprueba que en la muestra ocurra eso.
print(prueba_t)
print(muestra_diferencias)

# Luego de haber realizado la prueba t, para la cota inferior, se obtiene un p valor de 0.894, mucho mayor al valor de
# significación de 0.05, por lo cual se falla al rechazar la hipotesis nula y se asegura con un 95% de confianza que 
# luego del periodo de entrenamiento, en promedio se ha reducido un 2.5 [Kg/m^2] de su indice de masa muscular.