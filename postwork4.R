library(dplyr)
library(roxygen2)

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

# *******************Postwork 4**********************
#1 Generamos una tabla apartir de las frecuencias del equipo local tA y otra para el equipo visitante TH
tH <- t1[,2]
tA <- t2[,2]

#Creamos un vector que contenga el numero total de los valores que tendra nuestra matriz
vector <- c(1:63)

#Generamos una matriz que contenga el vector anterior junto con sus dimensiones.
matriz <- matrix(vector, nrow = 9, ncol = 7)


# Por medio de un for anidado realizamos el producto tensorial al multiplicar las probabilidades margiales
# y con ello obtenemos una matriz
for (i in 1:9) {
  for (j in 1:7) {
    data <- tH[i] * tA[j]
    matriz[i,j] <- data
  }

}

# Ahora, creamos una matriz por medio de los valores de la probabilidad conjunta y los dividimos entre nuestra
# matriz resultante para obtener la tabla de cocientes.
tc <- matrix(t.conjunta$Freq, nrow = 9, ncol = 7)
(resultado <- tc/matriz)

#2 Finalmente, almacenamos el resultado anterior en un vector y lo convertimos en un data frame
resultado <- as.vector(resultado)
df_resultado <- as.data.frame(resultado)
mean(resultado)

# Por medio de replicate y sample creamos una muestra de 5000 valores generada a partir de nuestra tabla de cocientes
a <- replicate(5000, mean(sample(resultado,50,replace = TRUE)))
mean(a)
hist(a)
