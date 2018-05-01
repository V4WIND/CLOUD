library(shiny)
library(ggplot2)

# UI part ----
ui <- pageWithSidebar(
        
        headerPanel("Select a tab"),
        
        ## conditionalPanel() functions for selected tab ----
        sidebarPanel(
          conditionalPanel(condition="input.tabselected==1",h4("LOSS MIT PROGRAM DASH")),
          conditionalPanel(condition="input.tabselected==2",
                           uiOutput("choices"),
                           radioButtons("choice","Choose an option", choices=c("Dataset" = 1, "Structure" = 2, "Summary" = 3 ))
                          ),
          conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary"))
        ),
        # ----
        
        # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding ----
        mainPanel(
          # id argument is important in the tabsetPanel()
          # value argument is important in the tabPanle()
          tabsetPanel(
              tabPanel("About", value=1, fileInput(
                inputId = "ufile",
                label = "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
              )),
              tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
                       conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
                       conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary"))),
              tabPanel("Plot", value=3, plotOutput("plot")), 
              id = "tabselected"
          )
        )
        # ----
  )

# server part ----
server <- function(input,output){
  
  ## Get the value of the dataset that is selected by user from the list of datasets
  data <- eventReactive(input$ufile,{
    read.csv(input$ufile$datapath,header = TRUE,stringsAsFactors = FALSE,strip.white = TRUE)
  })
  
  ## to output the dataset
  output$dat <- renderPrint({
    data()
  })
  
  # to output the structure of the dataset
  output$struct <- renderPrint({
    str(input$ufile)
  })
  
  # for summary
  output$summary <- renderPrint({
    summary(input$ufile)
  })
  
  # Pulling the list of variable for choice of variable x
  output$varx <- renderUI({
    selectInput("variablex", "select the X variable", choices=names(data()))
  })
  
  # Pulling the list of variable for choice of variable y
  output$vary <- renderUI({
    selectInput("variabley", "select the Y variable", choices=names(data()))
    
  })
  
  # For plot
  output$plot <- renderPlot({
    ggplot( data(), aes_string(x=input$variablex, y=input$variabley) ) + geom_point( ) 
  }) 
  
}




shinyApp(ui = ui, server = server)