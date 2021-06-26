library(dplyr)
library(ggplot2)

# Fijamos nuestra área de trabajo en la carpeta donde se encuentra el archivo
# y lo almacenamos en un data frame

setwd("c:/users/gabri/Documents/BEDU/FASE II/Postwork/")

data <- read.csv("tabla1.csv")

# Quitamos la primer columna ya que esta repetida
data <- data[,-1]

str(data)

# Cambiamos el formato de Date para que corresponda a un valor de fecha.
data <- mutate(data, Date = as.Date(Date, "%Y-%m-%d"))

#1.1 Obtenemos la tabla de frecuencia para los goles del equipo de casa.
t1 <- prop.table(table(goles = data$FTHG))
t1 <- as.data.frame(t1)
t1


#1.2 Obtenemos la tabla de frecuencia para los goles del equipo visitante.
t2 <- prop.table(table(goles = data$FTAG))
t2 <- as.data.frame(t2)
t2



#1.3 Generamos una tabla conjunta que nos indique las probabilidades del equipo local y el visitante.
t.conjunta <- prop.table(table(FTHG = data$FTHG, FTAG = data$FTAG))
t.conjunta <- as.data.frame(t.conjunta)
t.conjunta


#2.1 Generamos una tabla de frecuencia para los goles del equipo local.
b1 <- ggplot(data, aes(x=FTHG, y= (..count..)/sum(..count..))) +
  geom_bar() +
  ylab("Frecuencia") +
  theme_dark()
b1

#2.2 Generamos una tabla de frecuencia para los goles del equipo visitante.
b2 <- ggplot(data, aes(x=FTAG, y= (..count..)/sum(..count..)))+
  geom_bar()+
  ylab("Frecuencia") +
  theme_dark()
b2

#2.3 Finalmente, utilizamos nuestra tabla conjunta para crear un heatmap uqe contiene las
#   probabilidades de ambos equipos respecto a los goles que pueden anotar.
t6 <- ggplot(t.conjunta, aes(x=FTHG, y=FTAG, fill=Freq)) +
  geom_tile()
t6

# Por ultimo utilizamos la libreria de plotly para checar nuestros datos de una manera más
# sencilla.
library(plotly)

ggplotly(t6)
