#' Se importanron los datos de la temporada 2019/2020 y se guardaron en la carpeta de Clase1
#' Se almacena la información del CSV en la variable df_soccer.

df_soccer <- read.csv("c:/Users/gabri/Documents/BEDU/FASE II/Clase/Clase1/SP1.csv")

#' Ahora guardamos unicamente las columnas que queremos y las almacenamos en un nuevo data frame
df_goles <- data.frame(FTHG = df_soccer$FTHG, FTAG= df_soccer$FTAG)

#' Por medio de prop, obtenemos la tabla de frecuencias del equipo que juega en casa
tabla.casa <- prop.table(table(goles = df_goles$FTHG))
tabla.casa

#' Hacemos lo mismo pero ahora para la tabla de equipo visitante
tabla.visit <- prop.table(table(goles = df_goles$FTAG))
tabla.visit

#' Finalmente creamos una tabla conjunta que contiene las frecuencias de las tablas anteriores
table_goles <- prop.table(table(FTHG = df_goles$FTHG, FTAG = df_goles$FTAG))
table_goles
