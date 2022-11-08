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
library(ggrepel)
library(rjson)

theme_set(theme_bw())

#data = read_csv("~/Desktop/NYU/Kazakhstan/EDM/edm_practice/PRACTICE_points_2022_09_04.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("newplot"),
  
  fluidRow(fileInput("input_data", "Upload EDM JSON or CSV file", width = "100%", 
                     accept = c(".json", ".csv"))), 
  
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
                                ), 
                                ##for testing
                                tabPanel("Table view",
                                         dataTableOutput("printDF")
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
    
    ext = tools::file_ext(inFile$datapath)
    
    if(ext == "csv") {
      df = read_csv(inFile$datapath)
      return(list(df, NULL, NULL, NULL))
    }
    
    if(ext == "json") {
      jdata = fromJSON(file = inFile$datapath)
      #print(names(jdata))
    
      dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units"))]
      data = as.data.frame(do.call(rbind, jdata[[dataname]]))
      data[c(1:4, 6:8)] = sapply(data[c(1:4, 6:8)], as.numeric)
      
      
      prisms = as.data.frame(do.call(rbind, jdata$prisms))
      prisms[c(2:3)] = sapply(prisms[c(2:3)], as.numeric)
      units = as.data.frame(do.call(rbind, jdata$units))
      units[c(2:5)] = sapply(units[c(2:5)], as.numeric)
      datums = as.data.frame(do.call(rbind, jdata$datums))
      datums[c(2:4)] = sapply(datums[c(2:4)], as.numeric)
      
      return(list(data, units, prisms, datums))
      
    }
  
    
  })
  
  #for testing
  output$printDF <- renderDataTable({
    datalist = dataInput()
    data = datalist[[1]]
  }
  )
  
  output$frontView <- renderPlot({
    datalist = dataInput()
    data = datalist[[1]]
    
    #draw front view
    ggplot(data, aes(x = X, y = Z, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2)
  })
  
  output$sideView <- renderPlot({
    datalist = dataInput()
    data = datalist[[1]]
    
    #draw side view
    ggplot(data, aes(x = Y, y = Z, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2)
  })
  
  output$planView <- renderPlot({
    datalist = dataInput()
    data = datalist[[1]]
    
    #draw plan view
    ggplot(data, aes(x = X, y = Y, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2)
  })
  
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    datalist = dataInput()
    data = datalist[[1]]
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 5, addDist = T)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
