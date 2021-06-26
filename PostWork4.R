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

lista <- list(liga2017Date, liga2018Date, liga2019Date)
lista
liga <- do.call(rbind, lista)
liga

#Termina postwork2 comienza postwork3
FTHG <- liga$FTHG
FTHG

FTAG <- liga$FTAG
FTAG

total <- dim(liga)[1]
total

tablaCasa<-(table(FTHG)/total)
tablaCasa
golesCasa<-data.frame(tablaCasa)
golesCasa

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
tablaVisitante<-(table(FTAG)/total)
golesVisitante<-data.frame(tablaVisitante)
golesVisitante

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
tablaConjunto<-(table(FTAG,FTHG)/total)
golesJuntos<-data.frame(tablaConjunto)
golesJuntos
tablaConjunto

library(ggplot2)

golesCasa
#Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa
ggplot(golesCasa, aes(x=FTHG, y=Freq)) + 
  geom_bar(stat = "identity") +
  ggtitle("Probabilidad marginal de goles en casa")

#Un gráfico de barras para las probabilidades marginales
#estimadas del número de goles que anota el equipo visitante.
ggplot(golesVisitante, aes(x=FTAG, y=Freq)) + 
  geom_bar(stat = "identity")+
  ggtitle("Probabilidad marginal de goles como visitante")

#Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el 
#equipo de casa y el equipo visitante en un partido.
ggplot(golesJuntos, aes(x = FTHG, y = FTAG, fill = Freq)) + geom_tile()

#Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto de las probabilidades marginales correspondientes.
tablaCocientes <- (tablaConjunto/outer(tablaVisitante,tablaCasa,'*'))
tablaCocientes

install.packages('rsample')
library(rsample)

set.seed(839287482)
computos_boot <- bootstraps(tablaCocientes, times = 100)
computos_boot

