#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(shiny)){install.packages("shiny")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggrepel)){install.packages("ggrepel")}
if(!require(rjson)){install.packages("rjson")}
if(!require(DT)){install.packages("DT")}

library(shiny)
library(tidyverse)
library(ggrepel)
library(rjson)
library(DT)

theme_set(theme_minimal())

#data = read_csv("~/Desktop/NYU/Kazakhstan/EDM/edm_practice/PRACTICE_points_2022_09_04.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("newplot"),
  
  fluidRow(fileInput("input_data", "Upload EDM JSON or CSV file", width = "100%", 
                     accept = c(".json", ".csv"))), 
  
  sidebarLayout( position = "right",
                 
                 actionButton("edit", label = "Edit points")
                 
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
                                     click = "plot_click",
                                     dblclick = "front_dblclick", 
                                     brush = brushOpts(
                                       id = "front_brush",
                                       resetOnNew = TRUE
                                     )), 
                 ), 
                 tabPanel("Side view",
                          h3("SIDE VIEW"),
                          plotOutput("sideView", 
                                     click = "plot_click",
                                     dblclick = "side_dblclick",
                                     brush = brushOpts(
                                       id = "side_brush",
                                       resetOnNew = TRUE
                                     )), 
                          plotOutput("zoom_side", 
                                     click = "plot_click")
                 ), 
                 tabPanel("Plan view",
                          h3("PLAN VIEW"),
                          plotOutput("planView", 
                                     click = "plot_click",
                                     dblclick = "plan_dblclick",
                                     brush = brushOpts(
                                       id = "plan_brush",
                                       resetOnNew = TRUE
                                     )), 
                          plotOutput("zoom_plan", 
                                     click = "plot_click")
                 ), 
                 ##for testing
                 tabPanel("Table view",
                          DTOutput("printDF")
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
  output$printDF <- renderDT({
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
      geom_label_repel(size = 2) +
      coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
  })
  
  output$sideView <- renderPlot({
    datalist = dataInput()
    data = datalist[[1]]
    
    #draw side view
    ggplot(data, aes(x = Y, y = Z, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2) +
      coord_cartesian(xlim = side_ranges$x, ylim = side_ranges$y, expand = FALSE)
  })
  
  output$planView <- renderPlot({
    datalist = dataInput()
    data = datalist[[1]]
    
    #draw plan view
    ggplot(data, aes(x = X, y = Y, label = ID)) +
      geom_point() +
      geom_label_repel(size = 2) +
      coord_cartesian(xlim = plan_ranges$x, ylim = plan_ranges$y, expand = FALSE)
  })
  
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    datalist = dataInput()
    data = datalist[[1]]
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 5, addDist = T)
  })
  
  observeEvent(input$edit,  {
    showModal(modalDialog(
      title = "Somewhat important message",
      "This is a somewhat important message.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  front_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$front_dblclick, {
    brush = input$front_brush
    if (!is.null(brush)) {
      front_ranges$x <- c(brush$xmin, brush$xmax)
      front_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      front_ranges$x <- NULL
      front_ranges$y <- NULL
    }
  })
  
  side_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$side_dblclick, {
    brush = input$side_brush
    if (!is.null(brush)) {
      side_ranges$x <- c(brush$xmin, brush$xmax)
      side_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      side_ranges$x <- NULL
      side_ranges$y <- NULL
    }
  })
  
  plan_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plan_dblclick, {
    brush = input$plan_brush
    if (!is.null(brush)) {
      plan_ranges$x <- c(brush$xmin, brush$xmax)
      plan_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      plan_ranges$x <- NULL
      plan_ranges$y <- NULL
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
