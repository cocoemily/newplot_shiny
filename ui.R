# Define UI for application
if(!require(shiny)){install.packages("shiny")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggrepel)){install.packages("ggrepel")}
if(!require(rjson)){install.packages("rjson")}
if(!require(data.table)){install.packages("data.table")}
if(!require(DT)){install.packages("DT")}
if(!require(shinythemes)){install.packages("shinythemes")}
if(!require(gtools)){install.packages("gtools")}
if(!require(ggthemes)){install.packages("ggthemes")}
if(!require(here)){install.packages("here")}
if(!require(shinyFiles)){install.packages("shinyFiles")}

library(shiny)
library(tidyverse)
library(ggrepel)
library(rjson)
library(data.table)
library(DT)
library(shinythemes)
library(gtools)
library(ggthemes)
library(shinyFiles)


theme_set(theme_bw())


ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cosmo"),
  
  # Application title
  #titlePanel("newplot"),
  #navbarPage(
  
  fluidRow(
    shinyFilesButton('local_json', label='Upload EDM field JSON', title='Please select a file', multiple=FALSE), 
    textOutput("json_file_name", inline = T)
  ),
  
  #### newplot ####
  navbarPage( "newplot_field", id = "navbar",
              ##### data table view ####
              tabPanel("Data table", id = "dt", 
                       tabPanel("Table view",
                                DTOutput("printDF", 
                                         width = "auto"), 
                                column(width = 12, downloadButton("download", class = "btn-block", label = "Download CSV"))
                       )
              ),
              ##### plots view ####
              tabPanel("Plots", 
                       sidebarLayout(position = "right",
                                     sidebarPanel(
                                       fluidRow(
                                         wellPanel(
                                           tags$style("#plot_im_download, #plot_download {vertical-align: middle; height: 35px; width: 100%; font-size: 10px;}"),
                                           downloadButton("plot_im_download", class = "btn-block", label = "Download plot as PNG"),
                                           downloadButton("plot_download", class = "btn-block", label = "Download plot data as CSV")
                                         ), 
                                       ),
                                       fluidRow(
                                         wellPanel(
                                           div(style ="font-size: 11px",
                                               selectInput("select_view", label = "Select point view", 
                                                           choices = list("All points" = 1, "Last points" = 2), 
                                                           selected = 1),
                                               selectizeInput("select_units", label = "Select units", 
                                                              choices = NULL, multiple = T), 
                                               selectizeInput("select_levels", label = "Select levels", 
                                                              choices = NULL, multiple = T),
                                               selectizeInput("select_code", label = "Select code", 
                                                              choices = NULL, multiple = T),
                                           ),
                                           div(
                                             div(style="display: inline-block; vertical-align:middle; width: 200px; font-size: 11px",
                                                 checkboxGroupInput("color_select", label = "Color points by:", 
                                                                    choices = list("Code" = 1, "Unit" = 2, "Level" = 3), inline = T)
                                             ),
                                             div(style="display: inline-block; vertical-align:middle; font-size: 11px",
                                                 checkboxGroupInput("extra_plots", label = "Plot:", 
                                                                    choices = list("Datums" = 1, "Units" = 2, "Multi-points" = 3), 
                                                                    inline = T)
                                             )
                                           ), 
                                           div(tags$style("#clear_plot {vertical-align: middle; height: 35px; width: 100%; font-size: 10px;}"),
                                               actionButton("clear_plot", label = "Reset plot")
                                           ),
                                         ), 
                                       ),
                                       fluidRow( 
                                         wellPanel(
                                           fluidRow(
                                             div(style = "font-size: 11px",
                                             textInput("find_unit", "UNIT"), 
                                                    textInput("find_id","ID"))
                                             ), 
                                           fluidRow(
                                             tags$style("#find, #clear_find {vertical-align: middle; height: 30px; font-size: 10px;}"),
                                             column(5, actionButton("find", label = "Find record")), 
                                             column(5, actionButton("clear_find", label = "Clear point"))
                                           )
                                         )
                                       ),
                                       
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
                                                    ),
                                                    footer = fluidRow(column(12, tableOutput("info")), 
                                                                      column(12,
                                                                             actionButton("edit", label = "Edit record")
                                                                      )),
                                                    
                                       )
                                     )
                       ),
                       
                       
                       
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
  #)
  
)