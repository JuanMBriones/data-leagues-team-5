# Carga de datos
url_2017 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url_2018 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url_2019 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
urls <- c(url_2017, url_2018, url_2019) 
lista <- lapply(urls, read.csv)

# Manipulando datos
lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) # Variables de interés
lista <- lapply(lista, mutate, Date = as.Date(Date, format = "%d/%m/%y")) # Formato Date
datos <- do.call(rbind, lista) # Unimos los data frames

# 1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa 
# anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y goles 
# (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas 
# probabilidades conjuntas por el producto de las probabilidades marginales 
# correspondientes.

cociente <- function(df){
    marginal_golescasa <- as.data.frame(table(df$FTHG)/length(df$FTHG)) # Marginal Casa
    names(marginal_golescasa) <- c("Goles", "Probabilidad")
    
    marginal_golesvis <- as.data.frame(table(df$FTAG)/length(df$FTAG)) # Marginal Visitante
    names(marginal_golesvis) <- c("Goles", "Probabilidad")
    
    conjunta_goles <- as.data.frame(table(df$FTHG, df$FTAG)/length(df$FTHG)) # Conjunta
    names(conjunta_goles) <- c("Goles_Casa", "Goles_Vis", "Probabilidad")
    
    # Cociente
    conjunta_goles <- merge(x = conjunta_goles, y = marginal_golescasa, by.x = "Goles_Casa", 
                            by.y = "Goles", all.x = TRUE, suffixes = c("",".Casa")) 
    conjunta_goles <- merge(x = conjunta_goles, y = marginal_golesvis, by.x = "Goles_Vis", 
                            by.y = "Goles", all.x = TRUE, suffixes = c("",".Vis"))
    conjunta_goles["Producto_Marginales"] =  conjunta_goles["Probabilidad.Casa"]*conjunta_goles["Probabilidad.Vis"] # Producto
    conjunta_goles["Cociente"] = conjunta_goles["Probabilidad"]/conjunta_goles["Producto_Marginales"] # Cociente
    conjunta_goles["id_goles"] = paste0(conjunta_goles[["Goles_Casa"]], "_", conjunta_goles[["Goles_Vis"]]) # (x,y)
    
    return(conjunta_goles[, c("id_goles", "Cociente")])
} 


head(cociente(datos), 5)

str(cociente(datos))

summary(cociente(datos)$Cociente)

boxplot(cociente(datos)$Cociente)


# 2. Mediante un procedimiento de boostrap, obtén más cocientes similares a los
# obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
# distribuciones de la cual vienen los cocientes en la tabla anterior. 
# Menciona en cuáles casos le parece razonable suponer que los cocientes de la 
# tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia de 
# las variables aleatorias X y Y).

# Bootstrap con 1000 muestras
set.seed(25)
n_muestras = 1000
datos_boot <- bootstraps(datos, times = n_muestras)

# Mostramos la primera muestra para ejemplificar.
head(as.data.frame(datos_boot$splits[[1]]))

# Lista con los cocientes de cada muestra
muestras <- lapply(datos_boot$splits, function(x) cociente(as.data.frame(x))) 

# Juntando en un solo DF
muestras <- Reduce(function(x,y) merge(x, y, by = "id_goles", all = TRUE), muestras)

# Nombres de las columnas
names(muestras) <- c("id_goles", to_vec(for (i in 1:n_muestras) paste0("Muestra_", i)))

# Columnas del nuevo data frame 
nuevas_col <- to_vec(for(id in muestras$id_goles) paste0("Cociente_", id))

# Transponemos
muestras <- transpose(select(muestras, -id_goles))
names(muestras) <- nuevas_col

# Llenamos con cero los NA's
muestras[is.na(muestras)] <- 0 # Los eventos que no sucedieron tienen probabilidad cero
head(muestras)

par(mfrow=c(2,5))
for(i in 1:10) hist(muestras[ , i], main = names(muestras)[i])

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

pruebas <- sapply(muestras, prueba_dos_colas)
(no_rechazo <- names(which(pruebas > alpha))) # Cocientes dónde no rechazamos

par(mfrow=c(1,2))
for(i in 1:2) hist(muestras[no_rechazo][ , i], main = names(muestras[no_rechazo])[i])
