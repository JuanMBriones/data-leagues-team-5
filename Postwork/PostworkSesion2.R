setwd("c:/Users/eherr/OneDrive/Documents/BEDU/ProgramacionYEstadisticaConR/Sesion2/Files/PostworkSesion2")
suppressMessages(suppressWarnings(library(dplyr)))

# 1- Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 
#    2019/2020 de la primera división de la liga española a R, los datos los 
#    puedes encontrar en el siguiente enlace: 
#    https://www.football-data.co.uk/spainm.php

u1011 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"

download.file(url = u1011, destfile = "esp20.csv", mode = "wb")
download.file(url = u1112, destfile = "esp19.csv", mode = "wb")
download.file(url = u1213, destfile = "esp18.csv", mode = "wb")

(lista <- lapply(dir(), read.csv))


# 2- Revisa la estructura de de los data frames al usar las funciones: str, 
#    head, View y summary

head(lista[[1]]); head(lista[[2]]); head(lista[[3]])
str(lista[[1]]); str(lista[[2]]); str(lista[[3]])
View(lista[[1]]); View(lista[[2]]); View(lista[[3]])
summary(lista[[1]]); summary(lista[[2]]); summary(lista[[3]])


# 3- Con la función select del paquete dplyr selecciona únicamente las columnas 
#    Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data 
#    frames. (Hint: también puedes usar lapply).

nueva_lista <- lapply(lista, select, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR"))
nueva_lista <- lapply(nueva_lista, mutate, Date = as.Date(Date, "%d/%m/%Y"))


# 4- Asegúrate de que los elementos de las columnas correspondientes de los 
#    nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para 
#    arreglar las fechas). Con ayuda de la función rbind forma un único data 
#    frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la 
#    función do.call podría ser utilizada).

data_partidos_18_21 <- do.call(rbind, nueva_lista)

write.csv(data_partidos_18_21, "Partidos_18_21.csv", row.names = FALSE)

