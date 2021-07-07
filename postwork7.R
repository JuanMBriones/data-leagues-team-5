library(mongolite)

#' Almacenamos nuestras credenciales para acceder a nuestra DB de Mongo en una variable.
url_path <- "mongodb+srv://gryphathie:Fate%2Fstaynight20@cluster0.qjtc3.mongodb.net/test"


#' Creamos la conexión especificando la colección que queremos y la base de datos en donde se encuentra.
#' Pasamos nuestras credenciales a la función de url para autenticar nuestra conexión.

mongodb <- mongo(collection = "match", db= "Rstudio",
                 url = url_path,
                 verbose = TRUE)

print(mongodb)

#' Contamos por medio de la función count el número de registros en nuestra colección y verificamos que
#' corresponda con el número de valores que cargamos a esta colección.
mongodb$count()


#' Creamos un filtro para obtener el registro donde el Real Madrid jugo en casa el 20 de diciembre 2015.
golesRM <- mongodb$find('{"home.team":"Real Madrid","date":"2015-12-20"}')

#Jugo contra Vallecano y gano 10 a 2

#' Finalmente, cerramos nuestra conexión con la base de datos.
mongodb$disconnect(gc=TRUE)
