#Importa los datos de soccer de la temporada 2019/2020 de la 
#primera división de la liga española a R, los datos los 
#puedes encontrar en el siguiente enlace: 
#https://www.football-data.co.uk/spainm.php

getwd()
setwd("C:/Users/Bego Montes/Documents/decimo semestre/BEDU/Fase2")
liga <-read.csv("liga2019.csv")

#Del data frame que resulta de importar los datos a R, extrae las columnas que
#contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y 
#los goles anotados por los equipos que jugaron como visitante (FTAG)

FTHG <- liga$FTHG
FTHG

FTAG <- liga$FTAG
FTAG

#Consulta cómo funciona la función 
#table en R al ejecutar en la consola ?table
#regresa la frecunecia de cada uno de los valores en formato de tabla
?table

#Posteriormente elabora tablas de frecuencias relativas 
total <- dim(liga)[1]
total
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles
table(FTHG)/total

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
table(FTAG)/total

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
tabla<-table(FTAG,FTHG)/total
tabla


