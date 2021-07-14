# Postwork 8 - Equipo 5

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  

  titlePanel("Postwork 8 - Equipo 5"),
  

  sidebarPanel(
    p("Goles por equipo"), 
    selectInput("x", "Seleccione equipo local o visitante",
                choices = c("Goles Local"= "home.score","Goles Visitante" = "away.score")),
  ),
  
  
  mainPanel(tabsetPanel(              
    tabPanel("Goles",   
             plotOutput("output_plot"),
    ),
    
    tabPanel("Factor de Ganancia",  
             h3("Factor de ganancia Maximo"),
             img(src = "momio_maximo.png", 
                 height = 350, width = 550),
             h3("Factor de ganancia Promedio"),
             img( src = "momio_promedio.png", 
                  height = 350, width = 550)
             
    ),
    
    tabPanel("Probabilidad Goles",  
             h3("Probabilidad goles equipo local"),
             img(src = "Sesion-03-plt-1.png", 
                 height = 450, width = 550),
             h3("Probabilidad goles equipo visitante"),
             img( src = "Sesion-03-plt-2.png", 
                  height = 450, width = 550),
             h3("Probabilidades conjuntas"),
             img( src = "Sesion-03-plt-3.png", 
                  height = 450, width = 550)
    ), 
    
    tabPanel("Table Match", dataTableOutput("data_table"))          
  )
  )
) 

server <- function(input, output) {
  
  datasetImput <- reactive(
    switch(input$dataset, 
           "away.team" = away.team, 
           "home.team" = home.team)
  )

  output$output_plot <- renderPlot({
    data <-  read.csv("match.data.csv", header = T)
    x <- data[,input$x]
    if(input$x=="home.score"){
      data %>% ggplot(aes(x, fill = home.team)) + 
        geom_bar() + 
        facet_wrap("away.team", scales = "free") +
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
  
  output$table <- renderTable({ 
   data.frame(data)
  })
  
  output$data_table <- renderDataTable({data}, 
                                       options = list(aLengthMenu = c(5,25,50),
                                                      iDisplayLength = 5))
  
  
} 

shinyApp(ui = ui, server = server)

