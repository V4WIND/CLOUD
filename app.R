# library(tibble)
# from_ds <- as_tibble(iris)
# from_reltn <- tibble(x = 1:5, y = 1, z = x ^ 2 + y)


# lmpfcst
library(shiny)
library(rhandsontable)
library(ggplot2)
library(plotly)

# Define UI for app that inntakes a data file from user upload and creates a custom view ----
ui <- fluidPage(# App title ----
                titlePanel("View Historical Loss Mitigation Treatement Enrollments"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput(
                      inputId = "ufile",
                      label = "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                    ),
                    
                    radioButtons("h", "Display",
                                 choices = c(Head = "Head", All = "All"),
                                 selected = "Head"),
                    
                    
                    # Render a list of views available to user at once as per his choice ----
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

#----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----
####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----


# Define server logic required to do user defined actions----
server <- function(input, output) {
  # About the below server script ----
  # 1. It is "reactive" and therefore should be automatically re-executed when inputs (input$ufile & input$h) change
  # 2. Its output are rewritale data table & stat_summary_info & custom interactive plots
  
  # Server output functions to be displayed on the main panel ----
  
  
  observeEvent(input$ufile, {
    # input$ufile will be NULL initially. After the user uploads a file, head of that data file is shown by default 
    # or all obs & vars will be rendered as editable data table,  observing showplots event we show custom plots.
    RD  <<- read.csv(
      input$ufile$datapath, header = TRUE, sep = ",",  quote = '"'
    )
    
    max_n <<- nrow(RD)
    vars <<- colnames(RD)
    
    
    fluidRow(
    # dropdown is rendered for user input of var to create statistical summary info
    output$stats <- renderPrint({
      summary(RD)
    }),
    
    
    # thereby creating an rewritable custom data table view as per user choice of obs & vars ----
    # dataview as per obs & vars selected # confirgure to show only the user choice of vars
    output$dataview <- renderRHandsontable({
      # we can add count of input dt vars for debugging the ERROR in case of 1 or no var selection input from the user
      
      if (input$h != "Head")
      {
        s <- RD
      }
      else
      {
        # head of the table is shown
        s <- head(RD)
      }
      
      rhandsontable(s)
      # as_tibble(rhandsontable(s))
      
      

####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----
####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----####----


# Define server logic required to do user defined actions----
server <- function(input, output) {
  # About the below server script ----
  # 1. It is "reactive" and therefore should be automatically re-executed when inputs (input$ufile & input$h) change
  # 2. Its output are rewritale data table & stat_summary_info & custom interactive plots
  
  # Server output functions to be displayed on the main panel ----
  
  
  observeEvent(input$ufile, {
    # input$ufile will be NULL initially. After the user uploads a file, head of that data file is shown by default 
    # or all obs & vars will be rendered as editable data table,  observing showplots event we show custom plots.
    RD  <<- read.csv(
      input$ufile$datapath, header = TRUE, sep = ",",  quote = '"'
    )
    
    max_n <<- nrow(RD)
    vars <<- colnames(RD)
    
    
    fluidRow(
    # dropdown is rendered for user input of var to create statistical summary info
    output$stats <- renderPrint({
      summary(RD)
    }),
    
    
    # thereby creating an rewritable custom data table view as per user choice of obs & vars ----
    # dataview as per obs & vars selected # confirgure to show only the user choice of vars
    output$dataview <- renderRHandsontable({
      # we can add count of input dt vars for debugging the ERROR in case of 1 or no var selection input from the user
      
      if (input$h != "Head")
      {
        s <- RD
      }
      else
      {
        # head of the table is shown
        s <- head(RD)
      }
      
      rhandsontable(s)
    }),
    
    
    output$custview <- renderUI({
      
      fluidRow(
        # Either the user can select a daterange / any other custom var for a plot / stat_sum_info
        # add observe event to select the x-axis var input type ( 1 of 3 )
        
        # patch for a data range selection
        
        # Horizontal line ----
        # tags$hr(),
        
        # vintage selection for x-axis on plot
        # sliderInput("pernum", "Vintage:",
        #            min = 6285, max = 6411,
        #            value = c(6315,6409)
        # ),
        
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
   
    
    # Line Plot - daterange/vintage/xvar vs yvar1 ---
    
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





# exploraory view of basic stats of yaxis var selected
# need to create a dtaerange input on xaxis for all ts() object plots
# yet to create an adjusted(seasonally/factor-wise normalization) vector on Y-axis
# yet to update plot with capacbility for mathematical transformations( log-scale / %change MoM YoY )


# Phase 1 starts parallel as we create tab1 with same basic structure
#  and will cater to weekly view of LMP NE
#  yet to add delinquency info view MOM YOY
#  yet to add seasonal view (adjusted/unadjusted with dlnq)
#  yet to add bucketwise/programwise/productwise/portfoiowise user selection
#  yet to add WD WE WEH WDH TBD TMD calender day impact view

# Phase 2 starts parallel as we create tab2 with same basic structure
#  and will cater to a future outlook using forecasting of LMP NE
#  yet to figure out the ratio slope /HW fcst/ ARIMA /AJ's methodv
