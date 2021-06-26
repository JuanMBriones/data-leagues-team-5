library(dplyr)

#' Se importaron los datos de las temporadas 2017/2018 2018/2019 y 2019/2020.
#' Y se almacenaron en la carpeta foot.
#' Ahora establecemos esta nueva ruta como nuestra área de trabajo.
setwd("c:/users/gabri/Documents/BEDU/FASE II/Clase/Clase2/foot")

#' Creamos una lista y por medio de lapply leemos todos los csv que se encuentren en la carpeta
lista <- lapply(dir(), read.csv)

#' Checamos su estructura
str(lista)

#' Verificamos los primeros datos de cada archivo para ver sus variables
head(lista)

View(lista)

summary(lista)

#' Aplicamos lapply para seleccionar de cada archivo únicamente las variables que nos interesan.
lista <- lapply(lista, select, Date,HomeTeam, AwayTeam,FTHG,FTAG,FTR) 
head(lista[[1]]); head(lista[[2]]); head(lista[[3]])

#' En un nuevo data frame guardamos toda la colección de datos utilizando do.call y uniendo los 
#' archivos por filas con el comando rbind.
df_foot <- do.call(rbind, lista)

#' Verificamos nuevamente nuestros datos y vemos que es una sola lista ahora.
head(df_foot)

#' Verificamos la estructura de nuestras variables y vemos que coincidan con los valores que maneja
#' cada variable.
str(df_foot)

#' Ahora cambiamos nuestra variable Date para que realmente tenga valores de tipo fecha
#' Utilizando mutate, establecemos el formato que tiene y que previamente verificamos.
df_foot <- mutate(df_foot, Date = as.Date(Date, "%d/%m/%Y"))

#' Checamos la dimensión de nuestro data frame.
dim(df_foot)

#' Finalmente, creamos un archivo CSV que contenga nuestros resultados para su posterior análisis.
write.csv(df_foot,file = "tabla1.csv")

