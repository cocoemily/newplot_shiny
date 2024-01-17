# Define UI for application
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cosmo"),
  
  # Application title
  #titlePanel("newplot"),
  
  fluidRow(
    shinyFilesButton('local_json', label='Upload EDM JSON', title='Please select a file', multiple=FALSE)
  ),
  
  #### newplot ####
  navbarPage( "newplot", id = "navbar",
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
                                           downloadButton("plot_im_download", class = "btn-block", label = "Download plot as PNG"),
                                           downloadButton("plot_download", class = "btn-block", label = "Download plot data as CSV")
                                         ), 
                                       ),
                                       #fluidRow(downloadButton("plot_im_download", class = "btn-block", label = "Download PNG")),
                                       #fluidRow(downloadButton("plot_download", class = "btn-block", label = "Download CSV")),
                                       fluidRow(
                                         wellPanel(
                                           selectInput("select_view", label = "Select point view", 
                                                       choices = list("Points" = 1, "Multi-points" = 2), 
                                                       selected = 1),
                                           uiOutput("unit_select"),
                                           uiOutput("level_select"),
                                           uiOutput("code_select"),
                                           div(
                                             div(style="display: inline-block; vertical-align:top; width: 150px",
                                                 checkboxGroupInput("color_select", label = "Color points by:", 
                                                                    choices = list("Code" = 1, "Unit" = 2, "Level" = 3))
                                             ),
                                             div(style="display: inline-block; vertical-align:top",
                                                 checkboxGroupInput("extra_plots", label = "Plot:", 
                                                                    choices = list("Datums" = 1, "Units (only in plan view)" = 2))
                                             )
                                           ), 
                                           actionButton("clear_plot", label = "Reset plot")
                                         ), 
                                       ),
                                       fluidRow( 
                                         wellPanel(
                                           fluidRow(textInput("find_unit", "UNIT"), 
                                                    textInput("find_id","ID")), 
                                           div(
                                             div(style="display: inline-block; vertical-align:top; width: 150px",
                                               actionButton("find", label = "Find record")), 
                                             div(style="display: inline-block; vertical-align:top;",
                                               actionButton("clear_find", label = "Clear point highlight")
                                             )
                                           )
                                         )
                                       ),
                                       
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
  
)