#' Comenzamos cargando la librería necesaria para la conexión con MongoDB
library(mongolite)

#' Almacenamos nuestras credenciales para acceder a nuestra DB de Mongo en una variable.
url_path <- "mongodb+srv://gryphathie:Fate%2Fstaynight20@cluster0.qjtc3.mongodb.net/test"


#' Creamos la conexión especificando la colección que queremos y la base de datos en donde se encuentra.
#' Pasamos nuestras credenciales a la función de url para autenticar nuestra conexión.

mongodb <- mongo(collection = "match",
                 db= "Rstudio",
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

#' En este ejercicio realizamos una conexión entre RStudio y nuestra base de datos en MongoDB, este tipo
#' de funciones son sumamente útiles cuando queremos hacer uso de los comandos establecidos en cada base de datos.
#' De realizar esto con una base SQL podríamos utilizar las QUERYS básicas para filtrar los datos que necesitamos
#' y poder utilizarlos en una plataforma de estadística como es RStudio ya que actualmente, los gestores de DB
#' nos limitan mucho al momento de querer realizar operaciones algebraicas. Al traer estos datos a R podemos obtener
#' todo tipo de probabilidades y gráficas que nos permita entender e interpretar los datos de nuestra DB de una manera
#' más sencilla y completa de lo que podríamos hacer utilizando únicamente el gestor de base de datos.
