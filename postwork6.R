#' En este postwork hacemos uso de las series de tiempo, debido a esto
#' nos cuestionamos si es posible utilizar R para generar algún método
#' para pronosticar los resultados. Por ello más adelante, generaremos
#' una hipótesis que nos permita resolver esta duda.


#' Comenzamos exportando las librerias necesarias.
library(dplyr)
library(lubridate)


#' Exportamos los datos de la carpeta y lo almacenamos en un
#' Data frame para poder cambiar el formato de fecha con mutate
datos <- read.csv("https://raw.githubusercontent.com/JuanMBriones/data-leagues-team-5/main/match.data.csv")

head(datos)
dim(datos)
str(datos)
datos <- mutate(datos, date = as.Date(date, "%Y-%m-%d"))


#1
#' Creamos un vector suma con el número total de filas de nuestro
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
#' Ahora utilizamos la paquetería de Lubridate para agrupar las fechas por meses
#' y por medio de su función summarise nos permite sacar el promedio para cada agrupación.
#' en este caso los agrupamos por mes, por lo que nos devuelve los promedios de goles por mes.

promedios <- datos %>%
  group_by(fecha = floor_date(datos$date, unit = "month")) %>%
  summarise(prom_goles = mean(sumagoles))

#3
#' Creamos una serie de tiempo que vaya del primer valor al valor 96 que equivale a
#' la fecha final 2019-12-01

promedios.ts <- ts(promedios$prom_goles,start = 1,end = 96)
tail(promedios.ts)

#4
#' Finalmente, graficamos nuestro resultado, donde x es igual a las fechas y Y a los promedios.

plot(promedios$fecha[1:96],promedios.ts, type = "l",
     xlab = "Fechas", ylab = "Promedios", main = "Gráfica de Promedios")



#' HIPOTESIS:
#' Por medio de la utilización de Promedios móviles ponderados deseamos realizar los pronósticos
#' de los partidos a partir de los promedios móviles de 3 meses.
#' Como hipótesis planteamos que el mínimo error al cuadrado de los pronósticos será menor a 0.5
#' H0: mínimo error de los cuadrados < 0.5
#' H1: H0 no es verdad


#' Para realizar los pronósticos creamos un data frame llamada prom.mov el cual por medio de una
#' función for generará los pronósticos con n = 3meses y los almacenará dentro de la posición
#' correspondiente de nuestro data frame, saltándose los primeros 3 valores debido a que son los que
#' se utilizaron para el pronóstico.
prom.mov <- as.data.frame(c(1:96))
for (j in 1:93) {
  prom.mov[j+3,1] <- (promedios[j,2] + promedios[j+1,2] + promedios[j+2,2])/3

}
names(prom.mov) <- "pronostico"

#' Hecho esto guardamos los datos de nuestros pronósticos en un nuevo data frame que además almacene
#' la suma de los goles por meses que utilizamos previamente para poder hacer la comparación.
promedios.nuevos <- cbind(promedios[1:96,],prom.mov)


#' Una vez que tenemos este nuevo data frame, obtendremos el valor de los errores de cada fila, para ello
#' debemos restar el valor de la suma de goles reales al pronóstico que generamos en esa fila y lo elevamos
#' al cuadrado. Para realizar esto utilizamos la función for nuevamente, la cual almacena estos valores
#' en un nuevo data frame llamada error.
error <- as.data.frame(c(1:96))
error[4,1] <- promedios.nuevos$prom_goles[4]
for (k in 4:95) {
  error[k+1,1] <- round((promedios.nuevos$prom_goles[k] - promedios.nuevos$pronostico[k])^2, 3)

}
names(error) <- "error"

#' Finalmente, obtenemos el promedio de los errores al cuadrado y verificamos que su valor es de 0.1486
#' lo que implica que el error mínimo de nuestros pronósticos es bastante pequeño y por ende aceptamos
#' H0 como verdadera.
error.prom <- mean(error[4:96,1])

#' Guardamos todos los valores dentro de un solo data frame
promedios.nuevos <- cbind(promedios.nuevos, error)

#' Creamos las gráficas del pronóstico y la de los valores reales para observar su comportamiento.
plot(promedios$fecha[4:96],promedios.nuevos[4:96,3], type = "l",
     xlab = "Fechas", ylab = "Promedios", main = "Gráfica de Promedios", ylim = c(1.75,4))

plot(promedios$fecha[4:96],promedios.nuevos[4:96,2], type = "l",
     xlab = "Fechas", ylab = "Promedios", main = "Gráfica de Promedios", ylim = c(1.75,4), col = "red")



#' CONCLUSIONES
#' Por medio del uso de series de tiempo podemos utilizar diferentes métodos de pronóstico empleados en
#' la actualidad para empresas que requieren reabastecer su almacén de productos y necesitan estimar
#' la cantidad a comprar de cada producto. Al utilizar R para obtener las series de tiempo podemos ahorrar
#' mucho tiempo para realizar los cálculos, además de que podemos cambiar rápidamente el método de
#' pronósticos utilizados, para ver cuál es el que mejor se adapta a nuestro modelo de datos, dando el
#' menor error posible.
