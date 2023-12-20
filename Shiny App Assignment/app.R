# import libraries
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Iris Data Exploration"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Correlation Plot", tabName = "corrPlot", icon = icon("bolt")),
      menuItem("Data Table", tabName = "dataT", icon = icon("database")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("corrPlot",
              box(plotOutput("correlation_plot"), width = 8),
              box(
                selectInput("features", "Features:",
                            c("Sepal.Width", "Petal.Length",
                              "Petal.Width")), width = 4
              )
    ),
   
    tabItem("dataT",
            fluidPage(
              DT::dataTableOutput("data1")
            )
            
    ),
    
    tabItem("summary",
            fluidPage(
              verbatimTextOutput("summary")
            )
            )
      
    )
  )
)

server <- function(input, output) {
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$features]],
         xlab = "Sepal Length", ylab = "Features")
  })
  
  output$data1 <- DT::renderDataTable(iris)
  
  output$summary <- renderPrint({
    iris %>%
      summary()
  })
}

shinyApp(ui, server)