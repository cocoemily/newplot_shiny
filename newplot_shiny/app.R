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
if(!require(shinythemes)){install.packages("shinythemes")}

library(shiny)
library(tidyverse)
library(ggrepel)
library(rjson)
library(DT)
library(shinythemes)


theme_set(theme_bw())

#data = read_csv("~/Desktop/NYU/Kazakhstan/EDM/edm_practice/PRACTICE_points_2022_09_04.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cosmo"),
  
  # Application title
  #titlePanel("newplot"),
  
  fluidRow(fileInput("input_data", "Upload EDM JSON or CSV file", width = "100%", 
                     accept = c(".json", ".csv"))), 
  
  navbarPage( "newplot", 
              tabPanel("Plots", 
                       sidebarLayout(position = "right",
                                     sidebarPanel(
                                       fluidRow( 
                                         wellPanel(
                                           fluidRow(textInput("find_unit", "UNIT"), 
                                                    textInput("find_id","ID")), 
                                           actionButton("find", label = "Find record")
                                         )
                                       ),
                                       fluidRow( 
                                         wellPanel(
                                           actionButton("edit", label = "Edit record")
                                         )
                                       )
                                     ),
                                     
                                     # numericInput("x", label = "New x value", value = NULL),
                                     # numericInput("y", label = "New y value", value = NULL),
                                     # numericInput("z", label = "New z value", value = NULL)
                                     
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
                                                                        )) 
                                                    ), 
                                                    tabPanel("Side view",
                                                             h3("SIDE VIEW"),
                                                             plotOutput("sideView", 
                                                                        click = "plot_click",
                                                                        dblclick = "side_dblclick",
                                                                        brush = brushOpts(
                                                                          id = "side_brush",
                                                                          resetOnNew = TRUE
                                                                        ))
                                                    ), 
                                                    tabPanel("Plan view",
                                                             h3("PLAN VIEW"),
                                                             plotOutput("planView", 
                                                                        click = "plot_click",
                                                                        dblclick = "plan_dblclick",
                                                                        brush = brushOpts(
                                                                          id = "plan_brush",
                                                                          resetOnNew = TRUE
                                                                        ))
                                                    ) 
                                                    
                                       )
                                     )
                       ),
                       
                       fluidRow(tableOutput("info"))
              ), 
              tabPanel("Data table", 
                       ##for testing
                       tabPanel("Table view",
                                DTOutput("printDF", 
                                         width = "auto")
                       )
              ), 
              navbarMenu("More", 
                         tabPanel("Units", 
                                  dataTableOutput("units")
                         ),
                         tabPanel("Datums", 
                                  dataTableOutput("datums")
                         ), 
                         tabPanel("Prisms", 
                                  dataTableOutput("prisms")
                         ) 
              )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv = reactiveValues(data = NULL, orig = NULL)
  
  dataInput = reactive({
    req(input$input_data)
    inFile = input$input_data
    
    ext = tools::file_ext(inFile$datapath)
    
    if(ext == "csv") {
      df = read_csv(inFile$datapath)
      rv$data = df
      rv$orig = df
      return(list(df, NULL, NULL, NULL))
    }
    
    if(ext == "json") {
      jdata = fromJSON(file = inFile$datapath)
      #print(names(jdata))
      
      dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units"))]
      data = as.data.frame(do.call(rbind, jdata[[dataname]]))
      data[c(1:4, 6:8)] = sapply(data[c(1:4, 6:8)], as.numeric)
      rv$data = df
      rv$orig = df
      
      prisms = as.data.frame(do.call(rbind, jdata$prisms))
      prisms[c(2:3)] = sapply(prisms[c(2:3)], as.numeric)
      units = as.data.frame(do.call(rbind, jdata$units))
      units[c(2:5)] = sapply(units[c(2:5)], as.numeric)
      datums = as.data.frame(do.call(rbind, jdata$datums))
      datums[c(2:4)] = sapply(datums[c(2:4)], as.numeric)
      
      return(list(data, units, prisms, datums))
      
    }
    
    
  })
  
  output$printDF <- renderDT({
    datalist = dataInput() 
    data = datalist[[1]]
  },
  editable = TRUE, 
  rownames = FALSE, 
  selection = "none"
  )
  
  ##TODO EDITTING
  observeEvent(input$printDF_cell_edit, {
    info = input$printDF_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    
    
    
    ##something to mark the changes
  })
  
  
  output$frontView <- renderPlot({
    datalist = dataInput() 
    data = datalist[[1]]
    
    #draw front view
    ggplot(data, aes(x = X, y = Z, label = ID)) +
      geom_point() +
      geom_point(data = special_point$data, color = "red", size = 3) +
      geom_label_repel(size = 2) +
      coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
  })
  
  output$sideView <- renderPlot({
    datalist = dataInput() 
    data = datalist[[1]]
    
    #draw side view
    ggplot(data, aes(x = Y, y = Z, label = ID)) +
      geom_point() +
      geom_point(data = special_point$data, color = "red", size = 3) +
      geom_label_repel(size = 2) +
      coord_cartesian(xlim = side_ranges$x, ylim = side_ranges$y, expand = FALSE)
  })
  
  output$planView <- renderPlot({
    datalist = dataInput() 
    data = datalist[[1]]
    
    #draw plan view
    ggplot(data, aes(x = X, y = Y, label = ID)) +
      geom_point() +
      geom_point(data = special_point$data, color = "red", size = 3) +
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
  
  output$units = renderDT({
    datalist = dataInput()
    data = datalist[[2]]
  }
  )
  
  output$prisms = renderDT({
    datalist = dataInput()
    data = datalist[[3]]
  }
  )
  
  output$datums = renderDT({
    datalist = dataInput()
    data = datalist[[4]]
  }
  )
  
  ##TODO EDITTING
  observeEvent(input$edit,  {
    datalist = dataInput()
    data = datalist[[1]]
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    point = nearPoints(df, input$plot_click, threshold = 10, maxpoints = 1, addDist = F)
    
    showModal(modalDialog(
      title = "Edit",
      textInput("unit_input", label = "UNIT", value = point$UNIT), 
      textInput("id_input", label = "ID", value = point$ID), 
      numericInput("x_input", label = "X", value = point$X),
      numericInput("y_input", label = "Y", value = point$Y), 
      numericInput("z_input", label = "Z", value = point$Z), 
      numericInput("prism_input", label = "PRISM", value = point$PRISM), 
      textInput("level_input", label = "LEVEL", value = point$LEVEL),
      textInput("code_input", label = "CODE", value = point$CODE),
      textInput("excav_input", label = "EXCAVATOR", value = point$EXCAVATOR),
      actionButton("submit_edits", label = "Submit changes"),
      easyClose = TRUE,
      footer = NULL, 
      size = "l"
    ))
  })
  
  ##WHY DO ALL POINTS START READ
  special_point <- reactiveValues(data = NULL)
  observeEvent(input$find, {
    datalist = dataInput()
    data = datalist[[1]]
    
    special_point$data = data %>% filter(UNIT == input$find_unit & ID == input$find_id)
    #print(special_point$data)
    
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
