setwd("c:/Users/eherr/OneDrive/Documents/BEDU/ProgramacionYEstadisticaConR/Sesion2/Files/PostworkSesion2")

library(dplyr)
library(tensor)
library(ggplot2)
library(plotly)

data_partidos_18_21 <- read.csv("Partidos_18_21.csv")

# 1- Ya hemos estimado las probabilidades conjuntas de que el equipo de casa 
#    anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y 
#    goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir 
#    estas probabilidades conjuntas por el producto de las probabilidades 
#    marginales correspondientes.

probRatio <- function(md) {
  library(tensor)
  t1 <- prop.table(table(md$FTHG))
  t2 <- prop.table(table(md$FTAG))
  t3 <- prop.table(table(md$FTHG, md$FTAG))
  # 1- Crear una matriz con los productos de cada elemento del vector con la 
  #    probabilidad de los goles de casa "probabilidad_goles_casa" con cada uno de
  #    los elementos del vector que almacena la probabilidad de goles visitante 
  #   "probabilidad_goles_visitante"
  ext_prod <- as.data.frame(tensor::tensor(t1, t2))
  # 2- Obtener el inverso multiplicativo de cada elemento de la matriz anterior "prod"
  ext_prod_inv <- apply(ext_prod, c(1, 2), '^', -1)
  # 3- Obtener la matriz resultante del producto elemento a elemento de la matriz 
  #    anterior con la matriz de probabilidad de goles "matriz_prob_goles"
  res <- as.data.frame(ext_prod_inv * t3)
  res <- rename(res, c(Gols_casa=Var1, Gols_vis=Var2, cociente=Freq))
  return(res)
}

df_cociente <- probRatio(data_partidos_18_21)
df_cociente

# 2- Mediante un procedimiento de boostrap, obtén más cocientes similares a los 
#    obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#    distribuciones de la cual vienen los cocientes en la tabla anterior. 
#    Menciona en cuáles casos le parece razonable suponer que los cocientes de 
#    la tabla en el punto 1, son iguales a 1 (en tal caso tendríamos 
#    independencia de las variables aleatorias X y Y).

media_muestral <- mean(df_cociente$cociente)
bootstrap <- replicate(n=10000, sample(df_cociente$cociente, replace = TRUE))
medias <- apply(bootstrap, MARGIN = 2, FUN = mean)
desv_est <- sqrt(sum((medias - media_muestral) ** 2) / length(medias))

bootstrap <- replicate(n = 1000, 
                       sample(df_cociente$Gols_casa, 
                              df_cociente$Gols_vis,
                              replace = TRUE))


hist_bootstrap <- ggplot() + 
  geom_histogram(aes(x = medias), color="black") + 
  geom_vline(xintercept = media_muestral, size=1, color="darkred") +
  ylab("Frecuencia") +
  ggtitle("Histograma de las medias")
hist_bootstrap

t.test(medias)


# resulta que el p-value < 2.2e-16, por lo que se rechaza la hipótesis de que el promedio es 1


