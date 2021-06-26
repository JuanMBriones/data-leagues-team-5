#Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R

liga2017 <- read.csv("C:/Users/Bego Montes/Documents/decimo semestre/BEDU/Fase2/liga2017.csv")
liga2018 <- read.csv("C:/Users/Bego Montes/Documents/decimo semestre/BEDU/Fase2/liga2018.csv")
liga2019 <- read.csv("C:/Users/Bego Montes/Documents/decimo semestre/BEDU/Fase2/liga2019.csv")

#str 
str(liga2017)
str(liga2018)
str(liga2019)

#head
head(liga2017)
head(liga2018)
head(liga2019)

#view
View(liga2017)
View(liga2018)
View(liga2019)

#summary 
summary(liga2017)
summary(liga2018)
summary(liga2019)



#Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).

library(dplyr)

liga2017 <-select(liga2017, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
liga2018 <-select(liga2018, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
liga2019 <-select(liga2019, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) 

liga2017
liga2018
liga2019

#Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).
liga2017Date <- mutate(liga2017, Date = as.Date(Date, "%d/%m/%y"))
liga2018Date <- mutate(liga2018, Date = as.Date(Date, "%d/%m/%Y"))
liga2019Date <- mutate(liga2019, Date = as.Date(Date, "%d/%m/%Y"))

liga <- list(liga2017Date, liga2018Date, liga2019Date)
liga
ligaUltima <- do.call(rbind, liga)
ligaUltima
