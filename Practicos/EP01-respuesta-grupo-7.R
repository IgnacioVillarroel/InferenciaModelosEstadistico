#ACTIVIDADES.
#                        ¿Qué variables se han cargado?
#RESPUESTA: Las varaibles que se cargaron fueron las siguientes: 
#     Region: que corresponde al nombre de la region,
#     Casos: corresponde a la cantidad de contagiados con sintomas
#     Fecha: es la fecha donde se registrÃ³ el numeros de Casos en el dia.


#                   ¿Qué tipo tiene cada una de estas variables?
#RESPUESTA: La variable Region es del tipo Categórica nominal,
#           La variable Casos es del tipo Numérica discreta y
#           La variable Fecha es del tipo Categórica ordinal.

#                  ¿Qué escala parecen tener estas variables?
#RESPUESTA:Region tiene una escala nominal, Casos tiene una escala de razón y Fecha tiene una escala ordinal.
library(dplyr)
library(tidyr)
library(readr)

datos <- read.csv2("C:\\Users\\ignac\\OneDrive\\Escritorio\\casen.csv")

losrios <- filter(datos, Region == "Los Ríos")

cabeza <- names(losrios)[-1]

datosLargos <- losrios %>% pivot_longer(cabeza, names_to = "Fechas", values_to = "Casos", values_transform  = as.integer)

rangoFechas <- "X01.06.2020" <= cabeza & "X31.12.2020" >= cabeza

final_fechas <- cabeza[rangoFechas]

final_casos <- datosLargos[rangoFechas,"Casos"]

maximoContagios <- which.max(final_casos[["Casos"]])

print("El día con mas contagios es:")
print(final_fechas[maximoContagios])




