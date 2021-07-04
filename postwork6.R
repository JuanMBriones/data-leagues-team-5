library(dplyr)
library(lubridate)

#' Exportamos los datos de la carpeta y lo almacenamos en un
#' Data frame para poder cambiar el formato de fecha con mutate

setwd("c:/Users/gabri/Documents/BEDU/FASE II/Postwork/")

datos <- read.csv("match.data.csv")

head(datos)
dim(datos)
str(datos)
datos <- mutate(datos, date = as.Date(date, "%Y-%m-%d"))


#1
#' Creamos un vector suma con el numero total de filas de nuestro
#' Data frame y por medio de un for creamos la suma de cada fila de goles.
#'
#' Seguido de esto usamos cbind para unir la nueva columna de suma a nuestro DF.

suma <- as.data.frame(c(1:3800))
tail(suma)

for (i in 1:3800) {
  suma[i,1] <- datos[i,3] + datos[i,5]

}
head(suma)
names(suma) <- "sumagoles"

datos <- cbind(datos,suma)


#2
#' Ahora utilizamos la paqueteria de Lubridate para agrupar las fechas por meses
#' y por medio de su funcion summarise nos permite sacar el promedio para cada agrupación.

promedios <- datos %>%
  group_by(fecha = floor_date(datos$date, unit = "month")) %>%
  summarise(prom_goles = mean(sumagoles))

#3
#' Creamos un serie de tiempo que vaya del primer valor al valor 96 que equivale a
#' la fecha final 2019-12-01

promedios.ts <- ts(promedios$prom_goles,start = 1,end = 96)
tail(promedios.ts)

#4
#' Finalmente, graficamos nuestro resultado, donde x es igual a las fechas y Y a los promedios.

plot(promedios$fecha[1:96],promedios.ts, type = "l",
     xlab = "Fechas", ylab = "Promedios", main = "Gráfica de Promedios")

