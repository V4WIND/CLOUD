# lmpfcst
library(shiny)
library(rhandsontable)
library(ggplot2)
library(plotly)

# Define UI for app that inntakes a data file from user upload and creates a custom view 
ui <- fluidPage(
  titlePanel("View Historical Loss Mitigation Treatement Enrollments"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file
      fileInput(
        inputId = "ufile",
        label = "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      
      # Render a list of views available to user at once as per his choice 
      uiOutput("custview"),
      # Rendered dropdown as an Output: intake L1 SEG to show .. of user uploaded data
      # Rendered Slider as an Output: intake VINTAGES to show .. of user uploaded data
      # Rendered dropdown as an Output: intake L2 SEG to show .. of user uploaded data
      # Rendered dropdown as an Output: intake L3 SEG to show .. of user uploaded data
      uiOutput("custus")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: statistical summary of ssvar selection from user ----
      verbatimTextOutput(outputId = "stats"),
      
      # Output: plot of yvar selection from user Render custom view from above dropdown list ----
      plotlyOutput(outputId = "p"),
      
      # Output: Custom data table view as per user selection of vars & obs ----
      rHandsontableOutput(outputId = "dataview")
      
      
    )
  )
)



####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----
####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----


# Define server logic required to do user defined actions----
server <- function(input, output) {
  
  
  observeEvent(input$ufile, {
    
    RD  <<- read.csv(input$ufile$datapath, header = TRUE, sep = ",",  quote = '"')
    max_n <<- nrow(RD)
    vars <<- colnames(RD)
    
    
    fluidRow(
      # droapdown is rendered for user input of var to create statistical summary info
      output$stats <- renderPrint({
        summary(RD)
      }),
      
      
      output$dataview <- renderRHandsontable({
        # we can add count of input dt vars for debugging the ERROR in case of 1 or no var selection input from the user
        s <- head(RD)
        rhandsontable(s)
      }),
      
      
      output$custview <- renderUI({
        
        fluidRow(
          
          # Sample custom var selection from user as x-axis on plot
          selectInput(
            inputId = "xvar",
            label =  "Choose Var for X-axis",
            choices = vars
          ),
          
          
          # add obseve event to select type of plot
          # add better graphical option for interactive plots
          
          # Sample custom var selection from user as y-axis on line plot
          selectInput(
            inputId = "yvar",
            label =  "Choose var for Y-axis in a line plot",
            choices = vars
          ),
          
          actionButton(
            "showplots",
            "Generate plots from var selection panel "
          )
        )
        
      })
    )
  })
  
  
  
  # EVENT - CLICK SHOW PLOTS INFO FOR VAR CHOSEN ----
  # dropdowns are rendered for user input of vars to create plots
  
  observeEvent(input$showplots, {
    
    # Line Plot - daterange/vintage/xvar vs yvar1 ----
    output$p <- renderPlotly({
      plot_ly(
        x = RD[, input$xvar],
        y = RD[, input$yvar],
        name = "LMP-Actuals",
        type = "bar"
      )
    })
  })
  
}

# Create Shiny app ----
shinyApp(ui , server)
