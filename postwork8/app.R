#install.packages("shinydashboard")
#install.packages("dplyr")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("shiny")

library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(shiny)

data <-  read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/match.data.csv", header = T)

ui <- fluidPage(
  
  dashboardPage(skin = "green",
    
    dashboardHeader(title = "Postwork 8 - equipo 5"),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Goles por equipo", tabName = "Dashboard", icon = icon("bar-chart-o")),
        menuItem("Factor de Ganancia", tabName = "imagenes", icon = icon("usd", lib = "glyphicon")),
        menuItem("Probabilidad Goles", tabName = "imag", icon = icon("flash", lib = "glyphicon")),
        menuItem("Tabla", tabName = "DATATABLE", icon = icon("table"))
      )
      
    ),
    
    dashboardBody(
      
      tabItems( 
        tabItem(tabName = "Dashboard",
                fluidRow(
                  titlePanel("Grafico de barras"),
                  selectInput("x", "Seleccione equipo local o visitante",
                              choices = c("Goles Local"= "home.score","Goles Visitante" = "away.score")),
                  box(plotOutput("output_plot"))
                )
        ),
        tabItem(tabName = "imag",
                fluidRow(
                  titlePanel(h3("Probabilidad de Goles")),
                  img( src = "https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/Sesion-03-plt-1.png?raw=true"),
                  img( src = "https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/Sesion-03-plt-2.png?raw=true"),
                  img(src="https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/Sesion-03-plt-3.png?raw=true")
                  
                )
        ),
        tabItem(tabName = "imagenes",
                fluidRow(
                  titlePanel(h3("Factor de Ganancia")),
                  img( src = "https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/momio_maximo.png?raw=true"),
                  img( src = "https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/momio_promedio.png?raw=true"),
                  img( src = "https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/histograma_momio_maximo.png?raw=true"),
                  img( src = "https://github.com/JuanMBriones/data-leagues-team-5/blob/fred/PostWork-8/www/histograma_momio_minimo.png?raw=true")
                  
                )
        ),
        tabItem(tabName = "DATATABLE",
                fluidRow(        
                  titlePanel(h3("Tabla")),
                  dataTableOutput ("tabla")
                )
        )
        
      )
    )
  )
)


server<-function(input, output){
  
  datos<-eventReactive(input$equipo,{
    return(data)
  })
  
  output$tabla<-renderDataTable({
    data.frame(data)
  })
  
  output$output_plot <- renderPlot({
    x <- data[,input$x]
    if(input$x=="home.score"){
      data %>% ggplot(aes(x, fill = home.team)) + 
        geom_bar() + 
        facet_wrap("home.team", scales = "free") +
        labs(x =input$x, y = "Goles Anotados") + 
        ylim(0,76)
    }else{
      data %>% ggplot(aes(x, fill = away.team)) + 
        geom_bar() + 
        facet_wrap("away.team", scales = "free") +
        labs(x =input$x, y = "Goles Anotados") + 
        ylim(0,76)
    }
  })
  
  
}


shinyApp(ui=ui,server = server)            



