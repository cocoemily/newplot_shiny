#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#data = read_csv("~/Desktop/NYU/Kazakhstan/EDM/edm_practice/PRACTICE_points_2022_09_04.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("newplot"),
  
  fluidRow(fileInput("input_data", "Upload EDM CSV file", width = "100%")), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( position = "right",
                 sidebarPanel(
                   
                   # numericInput("x", label = "New x value", value = NULL),
                   # numericInput("y", label = "New y value", value = NULL),
                   # numericInput("z", label = "New z value", value = NULL)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   tabsetPanel( id = "plots",
                                ##for testing
                                # tabPanel("Table view", 
                                #          dataTableOutput("printDF")
                                # ),
                                tabPanel("Front view",
                                         h3("FRONT VIEW"),
                                         plotOutput("frontView", 
                                                    click = "plot_click"),
                                ), 
                                tabPanel("Side view",
                                         h3("SIDE VIEW"),
                                         plotOutput("sideView", 
                                                    click = "plot_click")
                                ), 
                                tabPanel("Plan view",
                                         h3("PLAN VIEW"),
                                         plotOutput("planView", 
                                                    click = "plot_click")
                                )
                   )
                 )
  ),
  
  fluidRow(tableOutput("info"))
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dataInput = reactive({
    req(input$input_data)
    inFile = input$input_data
    
    df = read_csv(inFile$datapath)
    #print(df)
    return(df)
    
  })
  
  #for testing
  # output$printDF <- renderDataTable(
  #   dataInput()
  # )
  
  output$frontView <- renderPlot({
    data = dataInput()
    
    #draw front view
    ggplot(data, aes(x = X, y = Z, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2) +
      title("Front view")
  })
  
  output$sideView <- renderPlot({
    data = dataInput()
    
    #draw side view
    ggplot(data, aes(x = Y, y = Z, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2) + 
      title("Side view")
  })
  
  output$planView <- renderPlot({
    data = dataInput()
    
    #draw plan view
    ggplot(data, aes(x = X, y = Y, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2) + 
      title("Plan view")
  })
  
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    
    df = dataInput() %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 1, addDist = T)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
