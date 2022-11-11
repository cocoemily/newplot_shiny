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
  
  # fluidRow(fileInput("input_cfg", "Upload EDM CFG file", width = "100%",
  #                    accept = c(".cfg"))),
  
  fluidRow(fileInput("input_data", "Upload EDM JSON or CSV file", width = "100%",
                     accept = c(".json", ".csv"))),
  
  
  navbarPage( "newplot", 
              tabPanel("Data table", 
                       ##for testing
                       tabPanel("Table view",
                                DTOutput("printDF", 
                                         width = "auto"), 
                                #column(width = 12, downloadButton("download", class = "btn-block"))
                       )
              ),
              
              tabPanel("Plots", 
                       sidebarLayout(position = "right",
                                     sidebarPanel(
                                       fluidRow(
                                         selectInput("select_view", label = "Select point view", 
                                                     choices = list("Points" = 1, "Multi-points" = 2), 
                                                     selected = 1)
                                       ),
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
  
  jsondata = reactiveVal()
  
  data.df = reactiveVal()
  orig.df = reactiveVal()
  
  prisms.df = reactiveVal()
  units.df = reactiveVal()
  datums.df = reactiveVal()
  
  
  # cfgInput = reactive( {
  #   req(input$input_cfg)
  #   inFile = input$input_cfg
  #   ext = tools::file_ext(inFile$datapath)
  #   
  #   file = read_tsv(inFile$datapath)
  #   
  #   data.df(file)
  #   print(data.df)
  #   
  #   return(file)
  #   
  # })
  
  dataInput = reactive({
    req(input$input_data)
    inFile = input$input_data
    
    ext = tools::file_ext(inFile$datapath)
    
    if(ext == "csv") {
      df = read_csv(inFile$datapath)
      data.df(df)
      orig.df(df)
      return(list(df, NULL, NULL, NULL))
    }
    
    if(ext == "json") {
      jdata = fromJSON(file = inFile$datapath)
      jsondata(jdata)
      
      dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units"))]
      data = as.data.frame(do.call(rbind, jdata[[dataname]]))
      data[c(1:4, 6:8)] = sapply(data[c(1:4, 6:8)], as.numeric)
      data.df(data)
      orig.df(data)
      
      prisms = as.data.frame(do.call(rbind, jdata$prisms))
      prisms[c(2:3)] = sapply(prisms[c(2:3)], as.numeric)
      prisms.df(prisms)
      units = as.data.frame(do.call(rbind, jdata$units))
      units[c(2:5)] = sapply(units[c(2:5)], as.numeric)
      units.df(units)
      datums = as.data.frame(do.call(rbind, jdata$datums))
      datums[c(2:4)] = sapply(datums[c(2:4)], as.numeric)
      datums.df(datums)
      
      return(list(data, units, prisms, datums))
      
    }
    
    
  })
  
  output$printDF <- renderDT({
    if(is.null(data.df())) {
      datalist = dataInput()
      data = datalist[[1]]
    } else {
      data = data.df()
    }
  },
  editable = TRUE, 
  rownames = FALSE, 
  selection = "none"
  )
  
  observeEvent(input$printDF_cell_edit, {
    info = input$printDF_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    
    newdf = data.df()
    newdf[i,j] = info$value
    data.df(newdf)
    
    jdata = jsondata()
    sp.df = split(data.df(), row(data.df()))
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units"))]
    #print(jdata[[dataname]])
    jdata[[dataname]] <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), "test.json")
  })
  
  
  output$download = downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext((input$input_data$name)), Sys.Date(), ".csv")
    },
    content = function(file) {
      #TODO deal with this
      vroom::vroom_write(data.df(), file)
    }
  )
  
  special_point <- reactiveValues(data = NULL)
  observeEvent(input$find, {
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
    special_point$data = data %>% filter(UNIT == input$find_unit & ID == input$find_id)
    #print(special_point$data)
  })
  
  
  front_ranges <- reactiveValues(x = NULL, y = NULL)
  output$frontView <- renderPlot({
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
    if(is.null(front_ranges$x)) {
      front_ranges$x <- c(min(data$X) - 100, max(data$X) + 100)
    }
    
    if(is.null(front_ranges$y)) {
      front_ranges$y <- c(min(data$Z) - 100, max(data$Z) + 100)
    }
    
    if(input$select_view == 2) {
      dataToClean = data.df()
      dataToClean = dataToClean %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
      data = dataToClean
    } else {
      data = data.df()
    }
    
    if(!is.null(special_point$data)) {
      if(input$select_view == 2) {
        ggplot(data, aes(x = X, y = Z, label = ID , group = grp)) +
          geom_point() +
          geom_line() +
          geom_point(data = special_point$data, color = "red", size = 3) +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      } else {
        ggplot(data, aes(x = X, y = Z, label = ID)) +
          geom_point() +
          geom_point(data = special_point$data, color = "red", size = 3) +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      }
    }else {
      if(input$select_view == 2) {
        ggplot(data, aes(x = X, y = Z, label = ID, group = grp)) +
          geom_point() +
          geom_line() +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      } else {
        ggplot(data, aes(x = X, y = Z, label = ID)) +
          geom_point() +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      }
    }
  })
  
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
  output$sideView <- renderPlot({
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
    if(is.null(side_ranges$x)) {
      side_ranges$x <- c(min(data$Y) - 100, max(data$Y) + 100)
    }
    
    if(is.null(side_ranges$y)) {
      side_ranges$y <- c(min(data$Z) - 100, max(data$Z) + 100)
    }
    
    if(input$select_view == 2) {
      dataToClean = data.df()
      dataToClean = dataToClean %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
      data = dataToClean
    } else {
      data = data.df()
    }
    
    #draw side view
    if(!is.null(special_point$data)) {
      if(input$select_view == 2) {
        ggplot(data, aes(x = Y, y = Z, label = ID , group = grp)) +
          geom_point() +
          geom_line() +
          geom_point(data = special_point$data, color = "red", size = 3) +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      } else {
        ggplot(data, aes(x = Y, y = Z, label = ID)) +
          geom_point() +
          geom_point(data = special_point$data, color = "red", size = 3) +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      }
    }else {
      if(input$select_view == 2) {
        ggplot(data, aes(x = Y, y = Z, label = ID, group = grp)) +
          geom_point() +
          geom_line() +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      } else {
        ggplot(data, aes(x = Y, y = Z, label = ID)) +
          geom_point() +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      }
    }
  })
  
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
  output$planView <- renderPlot({
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
    if(is.null(plan_ranges$x)) {
      plan_ranges$x <- c(min(data$X) - 100, max(data$X) + 100)
    }
    
    if(is.null(plan_ranges$y)) {
      plan_ranges$y <- c(min(data$Y) - 100, max(data$Y) + 100)
    }
    
    if(input$select_view == 2) {
      dataToClean = data.df()
      dataToClean = dataToClean %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
      data = dataToClean
    } else {
      data = data.df()
    }
    
    #draw plan view
    if(!is.null(special_point$data)) {
      if(input$select_view == 2) {
        ggplot(data, aes(x = X, y = Y, label = ID , group = grp)) +
          geom_point() +
          geom_line() +
          geom_point(data = special_point$data, color = "red", size = 3) +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      } else {
        ggplot(data, aes(x = X, y = Y, label = ID)) +
          geom_point() +
          geom_point(data = special_point$data, color = "red", size = 3) +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      }
    }else {
      if(input$select_view == 2) {
        ggplot(data, aes(x = X, y = Y, label = ID, group = grp)) +
          geom_point() +
          geom_line() +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      } else {
        ggplot(data, aes(x = X, y = Y, label = ID)) +
          geom_point() +
          geom_label_repel(size = 2) +
          coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      }
    }
  })
  
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
  
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    # datalist = dataInput() 
    # data = datalist[[1]]
    if(is.null(data.df())) {
      datalist = dataInput()
      data = datalist[[1]]
    } else {
      data = data.df()
    }
    
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 5, addDist = T)
  })
  
  output$units = renderDT({ units.df() },  editable = TRUE, rownames = FALSE, selection = "none")
  observeEvent(input$units_cell_edit, {
    info = input$units_cell_edit
    i = info$row
    j = info$col + 1
    
    newdf = units.df()
    newdf[i,j] = info$value
    units.df(newdf)
    
    jdata = jsondata()
    sp.df = split(units.df(), row(units.df()))
    jdata$units <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), "test.json")
    
  })
  
  output$prisms = renderDT({ prisms.df() }, editable = TRUE, rownames = FALSE, selection = "none")
  observeEvent(input$prisms_cell_edit, {
    info = input$prisms_cell_edit
    i = info$row
    j = info$col + 1
    
    newdf = prisms.df()
    newdf[i,j] = info$value
    prisms.df(newdf)
    
    jdata = jsondata()
    sp.df = split(prisms.df(), row(prisms.df()))
    jdata$prisms <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), "test.json")
    
  })
  
  output$datums = renderDT({ datums.df()}, editable = TRUE, rownames = FALSE, selection = "none")
  observeEvent(input$datums_cell_edit, {
    info = input$datums_cell_edit
    i = info$row
    j = info$col + 1
    
    newdf = datums.df()
    newdf[i,j] = info$value
    datums.df(newdf)
    
    jdata = jsondata()
    sp.df = split(datums.df(), row(datums.df()))
    jdata$datums <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), "test.json")
    
  })
  
  orig_unit = reactiveVal()
  orig_id = reactiveVal()
  observeEvent(input$edit,  {
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    point = nearPoints(df, input$plot_click, threshold = 10, maxpoints = 1, addDist = F)
    orig_unit(point$UNIT)
    orig_id(point$ID)
    
    showModal(modalDialog(
      title = "Edit",
      conditionalPanel("false", textInput("row_input", label = "row", value = rownames(point))),
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
  
  observeEvent(input$submit_edits, {
    removeModal()
    
    data = data.df()
    orig = orig.df()
    datarow = orig[which(orig$UNIT[[1]] == orig_unit()[[1]] & orig$ID[[1]] == orig_id())[[1]],]
    print(datarow)
    datarow$UNIT = input$unit_input
    datarow$ID = input$id_input
    datarow$X = input$x_input
    datarow$Y = input$y_input
    datarow$Z = input$z_input
    datarow$PRISM = input$prism_input
    datarow$LEVEL = input$level_input
    datarow$CODE = input$code_input
    datarow$EXCAVATOR = input$excav_input
    print(datarow)
    
    data[input$row_input,] <- datarow
    #print(data)
    data.df(data)
    
    jdata = jsondata()
    sp.df = split(data.df(), row(data.df()))
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units"))]
    #print(jdata[[dataname]])
    jdata[[dataname]] <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), "test.json")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
