library(dplyr)
library(lubridate)

# Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
# Agrega una nueva columna sumagoles que contenga la suma de goles por partido.

nueva_data <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")
nueva_data <- mutate(nueva_data,
                     date = as.Date(date, "%Y-%m-%d"))

# Obtén el promedio por mes de la suma de goles.

nueva_data$sumgoles <- nueva_data$home.score + nueva_data$away.score


# Crea la serie de tiempo del promedio por mes de la suma de goles hasta 
# diciembre de 2019.

data_por_mes <- nueva_data %>% 
  group_by(fecha_por_mes= ceiling_date(date, "month")) %>% 
  summarise(promedio_goles=mean(sumgoles))


# Grafica la serie de tiempo.

meses <- data_por_mes$fecha_por_mes

fecha_final <- which(meses == "2019-12-01")

meses[fecha_final]

serie <- ts(data_por_mes$promedio_goles, start=1, end=fecha_final)
plot(serie)

