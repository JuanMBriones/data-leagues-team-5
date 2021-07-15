setwd("Files/")

# Se importan las librerías necesarias.

library(dplyr)
library(fbRanks)

# A partir del conjunto de datos de soccer de la liga española de las temporadas 
# 2017/2018, 2018/2019 y 2019/2020, crea el data frame SmallData, que contenga 
# las columnas date, home.team, home.score, away.team y away.score; esto lo 
# puede hacer con ayuda de la función select del paquete dplyr. Luego establece 
# undirectorio de trabajo y con ayuda de la función write.csv guarda el data 
# frame como un archivo csv con nombre soccer.csv. Puedes colocar como argumento 
# row.names = FALSE en write.csv.


# Con la función read.csv() se guardan los campos y registros de los archivos 
# .csv seleccionados y los almacena en objetos de tipo data.frame.
partidos_17 <- read.csv("esp18.csv")
partidos_18 <- read.csv("esp19.csv")
partidos_19 <- read.csv("esp20.csv")

# La función select() permite seleccionar los campos deseados de un data.frame.
partidos_17 <- select(partidos_17, 
                      c("Date", "HomeTeam", "FTHG", "AwayTeam", "FTAG"))
partidos_18 <- select(partidos_18,
                      c("Date", "HomeTeam", "FTHG", "AwayTeam", "FTAG"))
partidos_19 <- select(partidos_19, 
                      c("Date", "HomeTeam", "FTHG", "AwayTeam", "FTAG"))

# La función mutate cambia el formato de dato del campo que se deseé. En este
# caso el campo "date".
partidos_17 <- mutate(partidos_17,
                      Date=as.Date(Date, "%d/%m/%y"))
partidos_18 <- mutate(partidos_18, 
                      Date=as.Date(Date, "%d/%m/%y"))
partidos_19 <- mutate(partidos_19, 
                      Date=as.Date(Date, "%d/%m/%y"))

# La función do.call() compuesta con las funciones rbind() y list(), une los 
# tres dataframes por campo y en orden ascendente en en campo de la fecha.
SmallData <- do.call(rbind, list(partidos_17, 
                                 partidos_18, 
                                 partidos_19))

# La función rename() permite cambiar el nombre de los campos seleccionados.
SmallData <- rename(SmallData, c("date"="Date", 
                                 "home.team" = "HomeTeam", 
                                 "home.score"="FTHG", 
                                 "away.team"="AwayTeam", 
                                 "away.score"="FTAG"))

# Se guarda un archivo .csv del dataframe "SmallData"
write.csv(SmallData, "soccer.csv", row.names = FALSE)


# Con la función create.fbRanks.dataframes del paquete fbRanks importe el 
# archivo soccer.csv a R y al mismo tiempo asignelo a una variable llamada 
# listasoccer. Se creará una lista con los elementos scores y teams que son data
# frames listos para la función rank.teams. Asigna estos data frames a variables
# llamadas anotaciones y equipos.

# Se crean los dataframes requeridos con la función create.fbRanks.dataframes()
listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")

# Se guarda el campo "scores" dentro de la variable anotaciones.
anotaciones <- listasoccer[["scores"]]

# La función rank.teams() clasifica a los equipos según tres parámetros, total,
# ataque y defensa
anotaciones <- rank.teams(anotaciones)

# Se guarda el campo "teams" dentro de la variable equipos.
equipos <- listasoccer[["teams"]]
# La función rank.teams() clasifica a los equipos según tres parámetros, total,
# ataque y defensa
equipos <- rank.teams(scores = listasoccer[["scores"]], 
                      teams = equipos)


# Con ayuda de la función unique crea un vector de fechas (fecha) que no se 
# repitan y que correspondan a las fechas en las que se jugaron partidos. Crea 
# una variable llamada n que contenga el número de fechas diferentes. 
# Posteriormente, con la función rank.teams y usando como argumentos los data 
# frames anotaciones y equipos, crea un ranking de equipos usando únicamente 
# datos desde la fecha inicial y hasta la penúltima fecha en la que se jugaron 
# partidos, estas fechas las deberá especificar en max.date y min.date. Guarda 
# los resultados con el nombre ranking.

# La función unique nos permite obtener los datos no repetidos de un campo, en 
# este caso "date" dentro de la 
fecha <- unique(as.Date(listasoccer[["scores"]]$date))
fecha

# Se obtiene el número de datos en "fecha".
n <- length(fecha)
n

# Se obtiene el ranking de las fechas del dataframe que inicialmente se tenía 
# menos la última, es decir: 2017-08-18 al 2020-12-22.
ranking <- rank.teams(scores = listasoccer[["scores"]], 
                      teams = listasoccer[["teams"]],
                      min.date = fecha[1],
                      max.date = fecha[n - 1])


# Finalmente estima las probabilidades de los eventos, el equipo de casa gana, 
# el equipo visitante gana o el resultado es un empate para los partidos que se 
# jugaron en la última fecha del vector de fechas fecha. Esto lo puedes hacer 
# con ayuda de la función predict y usando como argumentos ranking y fecha[n] 
# que deberá especificar en date.

# Se pone a prueba el modelo basado en los datos de "ranking" con la última 
# fecha del dataset, es decir, la fecha número n. Esto
predict(object = ranking, date = fecha[n])

# Como se puede observar, para los partidos Leganes vs Sevilla y Valencia vs 
# Huesca las predicciones fueron acertadas (0.8-1.4 y 1.8-1 respectivamente) 
# pues coinciden con el resultado del partido (1-1 y 2-1 respectivamente). Sin 
# embargo, en la predicción del partido Vallecano vs Levante este falló pues en 
# realidad el marcador quedó 2-1 a favor del Vallecano y el porcentaje de gol 
# predicho fué de 1.3-1.9 favor Levante.

# Observación: los antes mencionados "porcentajes de gol" en realidad representan
# la fracción de goles predicha por el modelo, sin embargo en la realidad no 
# tiene sentido hablar de fracciones de gol. Entonces, si el modelo predice un 
# 0.8 quiere decir 1 en la realidad.
