setwd("c:/Users/eherr/OneDrive/Documents/BEDU/ProgramacionYEstadisticaConR/Sesion2/Files/PostworkSesion2")

library(dplyr)
library(tensor)
library(ggplot2)
library(plotly)
library(hrbrthemes)

# 1- Con el último data frame obtenido en el postwork de la sesión 2, elabora 
#    tablas de frecuencias relativas para estimar las siguientes probabilidades:

# 1.1- La probabilidad (marginal) de que el equipo que juega en casa anote x 
#      goles (x=0,1,2,)

data_partidos_18_21 <- read.csv("Partidos_18_21.csv")
df_goles <- select(data_partidos_18_21, FTHG, FTAG)

prob_casa <- as.data.frame(prop.table(table(goles = df_goles$FTHG)))


# 1.2- La probabilidad (marginal) de que el equipo que juega como visitante 
#      anote y goles (y=0,1,2,)

prob_visitante <- as.data.frame(prop.table(table(goles = df_goles$FTAG)))


# 1.3- La probabilidad (conjunta) de que el equipo que juega en casa anote x 
#      goles y el equipo que juega como visitante anote y goles 
#      (x=0,1,2,, y=0,1,2,)

prob_conjunta <- as.data.frame(prop.table(table(goles_casa = df_goles$FTHG,
                                                goles_visitante = df_goles$FTAG)))


# 2.1- Un gráfico de barras para las probabilidades marginales estimadas del 
#      número de goles que anota el equipo de casa.

bar_casa <- ggplot(prob_casa, aes(x=goles, y=Freq)) +
  geom_bar(stat='identity') +
  ylab("Frecuencia") +
  theme_dark() +
  ggtitle("Frecuencia de goles jugando en casa")

# 2.2- Un gráfico de barras para las probabilidades marginales estimadas del 
#    número de goles que anota el equipo visitante.

bar_visitante <- ggplot(prob_visitante, aes(x=goles, y=Freq)) +
  geom_bar(stat='identity') +
  ylab("Frecuencia") +
  theme_dark() +
  ggtitle("Frecuencia de goles jugando como visitante")

# 2.3- Un HeatMap para las probabilidades conjuntas estimadas de los números de 
#      goles que anotan el equipo de casa y el equipo visitante en un partido.

heat_conjunta <- ggplot(prob_conjunta, aes(x=goles_casa, y=goles_visitante, fill=Freq)) +
  geom_tile() +
  ggtitle("Heatmap goles") +
  scale_fill_gradient(low="pink", high="blue") +
  theme_ft_rc()
heat_conjunta
