## app.R ##

## Dash board para el data set 'mtcars'

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)

ui <-

    fluidPage(
        #theme = shinytheme("united"),
        #shinythemes::themeSelector(),
        shinyDashboardThemes(theme = "blue_gradient"),

        dashboardPage(
            #theme = "united",
            dashboardHeader(title = "Proyecto Final"),

            dashboardSidebar(

                sidebarMenu(
                    menuItem("Histograma", tabName = "Dashboard", icon = icon("dashboard")),
                    menuItem("Gráficos", tabName = "graphics", icon = icon("area-chart")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                    menuItem("Imágenes", tabName = "img", icon = icon("file-picture-o"))
                )

            ),

            dashboardBody(

                tabItems(

                    # Histograma
                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                titlePanel(h2("Histograma para los goles locales y foráneos", align = "center", style = "font-weight: bold")),
                                selectInput("x", "Seleccione el valor de X",
                                            choices = c("home.score","away.score")),

                                selectInput("zz", "Selecciona la variable del grid",

                                            choices = c("away.score")),
                                box(plotOutput("plot1", height = 250)),

                                box(
                                    title = "Slider",
                                    sliderInput("bins", "Number of observations:", 1, 10, 5)
                                )
                            )
                    ),

                    # Graficas
                    tabItem(tabName = "graphics",
                            fluidRow(
                                titlePanel(h2("Gráficas FTHG, FTAG y Heatmap", align = "center", style = "font-weight: bold")),
                                column(3,
                                    img( src = "fthg.png",
                                         height = 350, width = 350)),
                                column(3,
                                    img( src= "ftag.png",
                                         height = 350, width = 350),
                                    offset = "3 offset 3"
                                    )

                            ),
                            fluidRow(
                                column(3,
                                       img(src = "heat.png",
                                           height = 350, width = 350),
                                       offset = "3 offset 3"

                                )
                            )
                    ),



                    tabItem(tabName = "data_table",
                            fluidRow(
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    ),

                    tabItem(tabName = "img",
                            fluidRow(
                                titlePanel(h2("Factores de ganancia mínima y máxima", align = "center", style = "font-weight: bold")),
                                column(3,
                                       img( src = "maximos.png",
                                            height = 350, width = 350),
                                       titlePanel(h3("-Máximos-", align = "center", style = "color: grey")),
                                       offset = "1 offset 3"),
                                column(3,
                                       img( src = "promedios.png",
                                            height = 350, width = 350),
                                       titlePanel(h3("-Promedios-", align = "center", style = "color: grey")),
                                       offset = "3 offset 3")
                            )
                    )

                )
            )
        )
    )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)

    match <- read.csv("c:/Users/gabri/Documents/BEDU/FASE II/Postwork/postwork8/post8/match.data.csv")
    #summary(match)
    #Gráfico de Histograma
    output$plot1 <- renderPlot({

        x <- match[,input$x]
        bin <- seq(min(x), max(x), length.out = input$bins + 1)

        ggplot(match, aes(x)) +
            geom_histogram( breaks = bin) +
            labs( xlim = c(0, max(x))) +
            theme_dark() +
            xlab(input$x) + ylab("Frecuencia") +
            facet_grid(input$zz)



    })

    #Data Table
    output$data_table <- renderDataTable( {match},
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )

}


shinyApp(ui, server)
