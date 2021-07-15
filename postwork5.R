library(dplyr)
library(fbRanks)
setwd("c:/users/gabri/Documents/BEDU/FASE II/Clase/Clase2/foot")

#1
#' Primero creamos un data frame a partir de todos los archivos en la carpeta foot.
#' Seguido de esto filtramos por medio de select, las columnas que necesitamos.
#' Hecho esto juntamos todos los archivos en el mismo data frame por medio de rbind
#' y cambiamos las etiquetas de nuestras columnas a las correspondientes.
#' Finalmente, creamos un archivo csv que contiene lso datos anteriores en nuestro directorio de Postwork.

SmallData <- lapply(dir(), read.csv)
head(SmallData)
SmallData <- lapply(SmallData, select, Date,HomeTeam, AwayTeam,FTHG,FTAG)

SmallData <- do.call(rbind, SmallData)
SmallData <- mutate(SmallData, Date = as.Date(Date, "%d/%m/%Y"))
names(SmallData)[1] <- "date"
names(SmallData)[2] <- "home.team"
names(SmallData)[3] <- "away.team"
names(SmallData)[4] <- "home.score"
names(SmallData)[5] <- "away.score"
str(SmallData)


setwd("c:/users/gabri/Documents/BEDU/FASE II/Postwork/")

write.csv(SmallData,file = "soccer.csv", row.names = F)


#2
#' Usando la paqueteria de fbRanks creamos un data frame importando el  csv
#' que previamente creamos.
#' Extraemos la lista de equipos en una nueva variable y de igual manera para los puntajes.

listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")
equipos <- listasoccer$teams
anotaciones <- listasoccer$scores

#3
#' Por medio de la funcion unique creamos un vector de las fechas y lo almacenamos en una nueva
#' variable. Hacemos otra variable llamada ranking que por medio de la función rank.teams nos permite
#' pasar el puntaje, los equipos y las fechas deseadas para obtener los puntajes correspondeintes de cada equipo.

fecha <- unique(SmallData$date, incomparables = F, MARGIN = 1, fromLast = FALSE)
n <- length(fecha)
ranking <- rank.teams(scores = anotaciones, teams = equipos, max.date = fecha[382], min.date = fecha[1])


#4
#' Finalmente, creamos una variable de resultado que pormedio de predict nos permite estimar las probabilidades
#' de cada equipo para futuras anotaciones.

resultado <- predict(object = ranking, date = fecha[n])

#resultados <- predict.fbRanks(ranking, max.date = fecha[n], min.date = fecha[1])
