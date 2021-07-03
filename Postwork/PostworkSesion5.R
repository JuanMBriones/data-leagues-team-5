setwd("BEDU/ProgramacionYEstadisticaConR/Sesion2/Files/PostworkSesion2/")

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

partidos_17 <- read.csv("esp18.csv")
partidos_18 <- read.csv("esp19.csv")
partidos_19 <- read.csv("esp20.csv")

partidos_17 <- select(partidos_17, 
                      c("Date", "HomeTeam", "FTHG", "AwayTeam", "FTAG"))
partidos_18 <- select(partidos_18,
                      c("Date", "HomeTeam", "FTHG", "AwayTeam", "FTAG"))
partidos_19 <- select(partidos_19, 
                      c("Date", "HomeTeam", "FTHG", "AwayTeam", "FTAG"))

partidos_17 <- mutate(partidos_17,
                      Date=as.Date(Date, "%d/%m/%y"))
partidos_18 <- mutate(partidos_18, 
                      Date=as.Date(Date, "%d/%m/%y"))
partidos_19 <- mutate(partidos_19, 
                      Date=as.Date(Date, "%d/%m/%y"))

SmallData <- do.call(rbind, list(partidos_17, 
                                 partidos_18, 
                                 partidos_19))

SmallData <- rename(SmallData, c("date"="Date", 
                                 "home.team" = "HomeTeam", 
                                 "home.score"="FTHG", 
                                 "away.team"="AwayTeam", 
                                 "away.score"="FTAG"))

write.csv(SmallData, "soccer.csv", row.names = FALSE)


# Con la función create.fbRanks.dataframes del paquete fbRanks importe el 
# archivo soccer.csv a R y al mismo tiempo asignelo a una variable llamada 
# listasoccer. Se creará una lista con los elementos scores y teams que son data
# frames listos para la función rank.teams. Asigna estos data frames a variables
# llamadas anotaciones y equipos.

listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")

anotaciones <- listasoccer[["scores"]]
anotaciones <- rank.teams(anotaciones)

equipos <- listasoccer[["teams"]]
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

fecha <- unique(as.Date(listasoccer[["scores"]]$date))
fecha

n <- length(fecha)
n

ranking <- rank.teams(scores = listasoccer[["scores"]], 
                      teams = listasoccer[["teams"]],
                      max.date = fecha[n - 1],
                      min.date = fecha[1])


# Finalmente estima las probabilidades de los eventos, el equipo de casa gana, 
# el equipo visitante gana o el resultado es un empate para los partidos que se 
# jugaron en la última fecha del vector de fechas fecha. Esto lo puedes hacer 
# con ayuda de la función predict y usando como argumentos ranking y fecha[n] 
# que deberá especificar en date.

predict(object = ranking, date = fecha[n])
