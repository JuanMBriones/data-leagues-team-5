# PROYECTO

# Postwork 1 --------------------------------------------------------------

# 1. Importa los datos de soccer de la temporada 2019/2020 de la primera división
# de la liga española a R, los datos los puedes encontrar en el siguiente 
# enlace: https://www.football-data.co.uk/spainm.php
path <- "C:/Users/ferro/Desktop/Bedu/2. Programación y Estadística con R/Proyecto(Postwork)/Datos"
file <- "SP1.csv"
soccer <- read.csv(paste0(path, "/", file))

# 2. Del data frame que resulta de importar los datos a R, extrae las columnas 
# que contienen los números de goles anotados por los equipos que jugaron en 
# casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
goles_casa <- soccer$FTHG
goles_vis <- soccer$FTAG

# 3. Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

# 4. La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
table(goles_casa)/length(goles_casa)

# 5. La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
table(goles_vis)/length(goles_vis)

# 6. La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
goles_casa_vis <- paste(goles_casa, goles_vis) # Obs del Vec. aleatorio (X,Y)
table(goles_casa_vis)/length(goles_casa_vis)

table(goles_casa, goles_vis)/length(goles_casa) # Otra Forma

# Postwork 2 --------------------------------------------------------------
library(dplyr)

# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php 
setwd("C:/Users/ferro/Desktop/Bedu/2. Programación y Estadística con R/Proyecto(Postwork)/Datos")

url_2017 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url_2018 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url_2019 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = url_2017, destfile = "temp_2017.csv", mode = "wb")
download.file(url = url_2018, destfile = "temp_2018.csv", mode = "wb")
download.file(url = url_2019, destfile = "temp_2019.csv", mode = "wb")

archivos <- c("temp_2017.csv", "temp_2018.csv", "temp_2019.csv") 
lista <- lapply(archivos, read.csv)

# 2. Revisa la estructura de de los data frames al usar las funciones: str, head, View y summary
str(lista)
head(lista[[1]])
View(lista[[1]])
lapply(lista, dim)
lapply(lista, summary)

# 3. Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).
lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# 4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada). 
str(lista)
lista <- lapply(lista, mutate, Date = as.Date(Date, format = "%d/%m/%y"))
datos <- do.call(rbind, lista)

# Postwork 3 --------------------------------------------------------------
library(ggplot2)
library(dplyr)
# Centrar titulos de las gráficas
theme_update(plot.title = element_text(hjust = 0.5))

# 1. Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

    # 1.1 La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
marginal_golescasa <- as.data.frame(table(datos$FTHG)/length(datos$FTHG))
names(marginal_golescasa) <- c("Goles", "Probabilidad")

    # 1.2 La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
marginal_golesvis <- as.data.frame(table(datos$FTAG)/length(datos$FTAG))
names(marginal_golesvis) <- c("Goles", "Probabilidad")

    # 1.3 La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
conjunta_goles <- as.data.frame(table(datos$FTHG, datos$FTAG)/length(datos$FTHG)) # Otra Forma
names(conjunta_goles) <- c("Goles_Casa", "Goles_Vis", "Probabilidad")

# 2. Realiza lo siguiente:

    # 2.1 Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa.
marginal_golescasa %>% ggplot() +
    aes(x = Goles, y = Probabilidad) +
    geom_bar(stat = "identity") +
    labs(title = "Marginal Goles del Equipo de Casa.")

    # 2.2 Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante.
marginal_golesvis %>% ggplot() +
    aes(x = Goles, y = Probabilidad) +
    geom_bar(stat = "identity") +
    labs(title = "Marginal Goles del Equipo Visitante.")
    
    # 2.3 Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo visitante en un partido.
conjunta_goles %>% ggplot() +
    aes(x = Goles_Casa, y = Goles_Vis, fill = Probabilidad) +
    geom_tile() +
    labs(title = "Conjunta Goles del Equipo de Casa y Visitante.")

# Postwork 4 --------------------------------------------------------------
# 1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa 
# anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y goles 
# (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas 
# probabilidades conjuntas por el producto de las probabilidades marginales 
# correspondientes.
conjunta_goles <- merge(x = conjunta_goles, y = marginal_golescasa, by.x = "Goles_Casa", 
      by.y = "Goles", all.x = TRUE, suffixes = c("",".Casa"))
conjunta_goles <- merge(x = conjunta_goles, y = marginal_golesvis, by.x = "Goles_Vis", 
      by.y = "Goles", all.x = TRUE, suffixes = c("",".Vis"))
conjunta_goles["Producto_Marginales"] =  conjunta_goles["Probabilidad.Casa"]*conjunta_goles["Probabilidad.Vis"]
conjunta_goles["Cociente"] = conjunta_goles["Probabilidad"]/conjunta_goles["Producto_Marginales"]

# 2. Mediante un procedimiento de boostrap, obtén más cocientes similares a los 
# obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
# distribuciones de la cual vienen los cocientes en la tabla anterior. 
# Menciona en cuáles casos le parece razonable suponer que los cocientes de 
# la tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia 
# de las variables aleatorias X y Y).
# Sea X el número de goles de Casa y Y el número de goles de Vis.
# Debemos ver si P(X = x, Y = y)/(P(X = x)P(Y = y)) = 1 para cada x,y así
# Se debe hacer una prueba para cada par (x,y)
library(rsample)
library(comprehenr)
library(data.table)
set.seed(25)

str(datos)
n_muestras = 10000
# Bootstrap con 10000 muestras 
(datos_boot <- bootstraps(datos, times = n_muestras))

# Una muestra del bootstrap
as.data.frame(datos_boot$splits[[1]])
str(as.data.frame(datos_boot$splits[[1]]))
str(datos)

# Función para calcular el cociente de interés
cociente <- function(df){
    marginal_golescasa <- as.data.frame(table(df$FTHG)/length(df$FTHG))
    names(marginal_golescasa) <- c("Goles", "Probabilidad")
    
    marginal_golesvis <- as.data.frame(table(df$FTAG)/length(df$FTAG))
    names(marginal_golesvis) <- c("Goles", "Probabilidad")
    
    conjunta_goles <- as.data.frame(table(df$FTHG, df$FTAG)/length(df$FTHG)) # Otra Forma
    names(conjunta_goles) <- c("Goles_Casa", "Goles_Vis", "Probabilidad")

    # Cociente
    conjunta_goles <- merge(x = conjunta_goles, y = marginal_golescasa, by.x = "Goles_Casa", 
          by.y = "Goles", all.x = TRUE, suffixes = c("",".Casa"))
    conjunta_goles <- merge(x = conjunta_goles, y = marginal_golesvis, by.x = "Goles_Vis", 
          by.y = "Goles", all.x = TRUE, suffixes = c("",".Vis"))
    conjunta_goles["Producto_Marginales"] =  conjunta_goles["Probabilidad.Casa"]*conjunta_goles["Probabilidad.Vis"]
    conjunta_goles["Cociente"] = conjunta_goles["Probabilidad"]/conjunta_goles["Producto_Marginales"]
    conjunta_goles["id_goles"] = paste0(conjunta_goles[["Goles_Casa"]], "_", conjunta_goles[["Goles_Vis"]])
    
    return(conjunta_goles[, c("id_goles", "Cociente")])
} 

# Lista con los cocientes de cada muestra
(muestras <- lapply(datos_boot$splits, function(x) cociente(as.data.frame(x)) ) )

# Juntando en un solo DF
(muestras <- Reduce(function(x,y) merge(x, y, by = "id_goles", all = TRUE), muestras))

# Nombres de las columnas
names(muestras) <- c("id_goles", to_vec(for (i in 1:n_muestras) paste0("Muestra_", i)))
muestras

# Columnas del nuevo data frame 
(nuevas_col <- to_vec(for(id in muestras$id_goles) paste0("Cociente_", id)))

# Transponemos
(muestras <- transpose(select(muestras, -id_goles)))
names(muestras) <- nuevas_col

# Suponiendo que la normalización es una buena aproximación y como n > 30 haremos
# una prueba de dos colas con:
# H_0: mu = 1, H_1: mu != 1
alpha <- 0.05

prueba_dos_colas <- function(ma_cociente, mu = 1){
    n <- length(ma_cociente)
    
    # Prueba
    estadistico <- (mean(ma_cociente, na.rm = TRUE) - mu)/(sd(ma_cociente, na.rm = TRUE)/n**(1/2))
    pvalue <- pnorm(abs(estadistico), lower.tail = FALSE)*2    
    
    return(pvalue)
}

(pruebas <- sapply(muestras, prueba_dos_colas))
(no_rechazo <- names(which(pruebas > alpha))) # Podría valer 1 el cociente


# FALTA GRAFICAS
par(par(mfrow=c(2,3)))
hist(c(1,2))

library(grDevices)
graphics.out()

