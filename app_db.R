library(shiny)library(shinydashboard)

ui <- dashboardPage(  dashboardHeader(title = "This is the header"),  dashboardSidebar(    # sliderInput("bins", "Number of Breaks",1,100,50),    menuItem("Dashboard"),      menuSubItem("Dashboard Finance"),      menuSubItem("Dashboard sales"),    menuItem("Detailed Analysis"),    menuItem("Raw Data")  ),  dashboardBody(    fluidRow(    box(plotOutput("histogram")),    box(sliderInput("bins", "Number of Breaks",1,100,50))      )
  ))
server <-function(input, output){
  output$histogram <- renderPlot({      hist(faithful$eruptions, breaks = input$bins)    })
}
shinyApp(ui, server)
