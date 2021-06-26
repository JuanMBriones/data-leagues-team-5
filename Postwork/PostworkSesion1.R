library(dplyr)
# 1- Importa los datos de soccer de la temporada 2019/2020 de la primera 
#    división de la liga española a R, los datos los puedes encontrar en el  
#    siguiente enlace: https://www.football-data.co.uk/spainm.php

partidos_liga_española_df <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")


# 2- Del data frame que resulta de importar los datos a R, extrae las columnas 
#    que contienen los números de goles anotados por los equipos que jugaron en 
#    casa (FTHG) y los goles anotados por los equipos que jugaron como visitante 
#    (FTAG)

casa <- partidos_liga_española_df$FTHG
(visitante <- partidos_liga_española_df$FTAG)
(goles <- as.data.frame(cbind(casa, visitante)))

# 3- Consulta cómo funciona la función table en R al ejecutar en la consola 
#   ?table

?table


# 3- Posteriormente elabora tablas de frecuencias relativas para estimar las 
#    siguientes probabilidades:

# 3.1- La probabilidad (marginal) de que el equipo que juega en casa anote x 
#      goles (x = 0, 1, 2, ...)

total_de_partidos <- length(casa)
matriz_goles <- table(goles)
frecuencia_goles_casa <- rowSums(matriz_goles)
probabilidad_goles_casa <- c(frecuencia_goles_casa / total_de_partidos)
tabla_casa <- as.data.frame(cbind(names(frecuencia_goles_casa), frecuencia_goles_casa, probabilidad_goles_casa))


# 3.2- La probabilidad (marginal) de que el equipo que juega como visitante 
#      anote y goles (y = 0, 1, 2, ...)

frecuencia_goles_visitante <- colSums(matriz_goles)
probabilidad_goles_visitante <- c(frecuencia_goles_visitante / total_de_partidos)
tabla_visitante <- as.data.frame(cbind(names(frecuencia_goles_visitante), frecuencia_goles_visitante, probabilidad_goles_visitante))


# 3.3- La probabilidad (conjunta) de que el equipo que juega en casa anote x 
#      goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, 
#      ..., y = 0, 1, 2, ...)

(probabilidad_conjunta <- as.data.frame(matriz_goles / total_de_partidos))




