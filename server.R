# Define server
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  #### Reactive Values ####
  jsonfile = reactiveVal()
  jsondata = reactiveVal()
  dbname = reactiveVal()
  
  data.df = reactiveVal()
  prisms.df = reactiveVal()
  units.df = reactiveVal()
  datums.df = reactiveVal()
  
  plot.df = reactiveVal()
  last.points = reactiveVal()
  
  #### Read file ####
  #####shinyFiles json input#####
  roots = c(root = "~")
  shinyFileChoose(input, 'local_json', session=session, root = roots, filetypes=c('json'))
  jsonFile = reactive({
    req(input$local_json)
    if(is.null(input$local_json)) {
      return(NULL)
    }
    jsonfile(parseFilePaths(roots = roots, input$local_json)$datapath)
    output$json_file_name <- renderText({parseFilePaths(roots = roots, input$local_json)$datapath})
    return(parseFilePaths(roots = roots, input$local_json)$datapath)
  })
  
  jsonInput = reactive({
    if(is.null(jsonFile())) {
      return()
    }
    
    jdata = fromJSON(file = as.character(jsonFile()))
    jsondata(jdata)
    
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units", "UNIT"))]
    dbname(dataname)
    dflist = list()
    points = jdata[[dataname]] #is there a way to have user select the points table correctly?
    for(i in names(points)) {
      df = as.data.frame(points[[i]])
      df$ROW = i
      df = df[,c("ROW", "UNIT", "ID", "SUFFIX", "CODE", "LEVEL", "EXCAVATOR", "PRISM", "X", "Y", "Z", "DATE", "NOTES")]
      dflist[[as.numeric(i)]] = df
    }
    data = rbindlist(dflist)
    data.df(data)
    
    
    if("prisms" %in% names(jdata)) {
      prisms = as.data.frame(do.call(rbind, jdata$prisms))
    } else{
      prisms = data.frame()
    }
    
    if("units" %in% names(jdata)){
      # units.list = list()
      # for(i in names(jdata$units)) {
      #   unit = as.data.frame(jdata$units[[i]])
      #   if("RADIUS" %in% rownames(unit)) {
      #     unit$ROW = i
      #     unit = unit %>% select("ROW", "ID", "NAME", "MINX", "MINY", "MAXX", "MAXY", "CENTERX", "CENTERY", "RADIUS", "SUFFIX")
      #   }else {
      #     unit$ROW = i
      #     unit$ID = ""
      #     unit$CENTERX = NA
      #     unit$CENTERY = NA
      #     unit$RADIUS = NA
      #     unit$SUFFIX = 0
      #     unit = unit %>% select("ROW", "ID", "NAME", "MINX", "MINY", "MAXX", "MAXY", "CENTERX", "CENTERY", "RADIUS", "SUFFIX")
      #   }
      #   units.list[[as.numeric(i)]] = unit
      # }
      # units = bind_rows(units.list)
      units = as.data.frame(do.call(rbind, jdata$units))
    } else{
      units = data.frame()
    }
    
    if("datums" %in% names(jdata)) {
      datums.list = list()
      for(i in names(jdata$datums)) {
        datums.list[[i]] = as.data.frame(jdata$datums[[i]]) %>% select(NAME, X, Y, Z) %>%
          mutate(X = as.numeric(X), Y = as.numeric(Y), Z = as.numeric(Z))
      }
      #datums = as.data.frame(do.call(smartbind, jdata$datums))
      datums = rbindlist(datums.list)
      
    } else{
      datums = data.frame()
    }
    
    prisms.df(prisms)
    units.df(units)
    datums.df(datums)
    
  })
  
  ##### get most recent points ####  
  observeEvent(data.df(), { #get last points
    data = data.df()
    date.data = data %>% filter(str_detect(DATE, "-")) %>%
      mutate(DATE = strftime(DATE, format = "%F %T"))
    #date.data$DATE = strftime(date.data$DATE, format = "%F %T")
    
    date.data$diff = abs(as.numeric(date(date.data$DATE) - Sys.Date()))
    last.data = date.data %>% filter(diff == min(diff)) %>%
      select(-diff)
    last.points(last.data)
  })
  
  #### Data Table View ####
  output$printDF <- renderDT({
    if(is.null(data.df())) {
      jsonInput()
      data = data.df()
    } else {
      data = data.df()
    }
  },
  editable = TRUE, 
  rownames = FALSE, 
  selection = "none"
  )
  
  ##### edit data table #####
  observeEvent(input$printDF_cell_edit, {
    #TODO fix this with prism change and Z change
    info = input$printDF_cell_edit
    print(input$printDF_cell_edit)
    str(info)
    i = info$row
    j = info$col + 1
    
    newdf = data.df()
    newdf[i,j] = info$value
    data.df(newdf)
    plot.df(newdf)
    
    jdata = jsondata()
    sp.df = split(newdf %>% select(-ROW), newdf$ROW)
    
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units", "UNIT"))]
    jdata[[dataname]] <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), jsonfile())
  })
  
  ##### download from data table #####
  output$download = downloadHandler(
    filename = function() {
      name = dbname()
      return(paste0(name, "_", Sys.Date(), ".csv"))
    },
    content = function(file) {
      vroom::vroom_write(data.df(), file, delim = ",")
    }
  )
  
  #### Plotting ####
  ##### Select input rendering ####
  observeEvent(data.df(), {
    data = data.df()
    updateSelectizeInput(session, "select_units", choices = data$UNIT, server = T, 
                         selected = input$select_units)
    updateSelectizeInput(session, "select_levels", choices = data$LEVEL, server = T, 
                         selected = input$select_levels)
    updateSelectizeInput(session, "select_code", choices = data$CODE, server = T, 
                         selected = input$select_code)
  })
  ##### Clear selections ####
  observeEvent(input$clear_plot, {
    data = data.df()
    color_palette(NULL)
    updateSelectInput(session, "select_view", 
                      choices = list("Points" = 1, 
                                     "Multi-points" = 2, 
                                     "Last points" = 3), 
                      selected = 1)
    updateSelectizeInput(session, "select_units", choices = data$UNIT, server = T, selected = NULL)
    updateSelectizeInput(session, "select_levels", choices = data$LEVEL, server = T, selected = NULL)
    updateSelectizeInput(session, "select_code", choices = data$CODE, server = T, selected = NULL)
    updateCheckboxGroupInput(session, "color_select", choices = list("Code" = 1, "Unit" = 2, "Level" = 3), selected = NULL, inline = T)
    updateCheckboxGroupInput(session, "extra_plots", choices = list("Datums" = 1, "Units" = 2), selected = NULL, inline = T)
  })
  
  ##### highlight "found" point ####
  special_point <- reactiveValues(data = NULL)
  observeEvent(input$find, {
    data = data.df()
    special_point$data = data %>% filter(UNIT == input$find_unit & ID == input$find_id)
  })
  
  observeEvent(input$clear_find, {
    special_point$data = NULL
    updateTextInput(session, "find_unit", value = "")
    updateTextInput(session, "find_id", value = "")
  })
  
  #####picking palettes #####
  color_palette = reactiveVal()
  observeEvent(input$color_select, {
    if(is.null(color_palette())) {
      showModal(modalDialog(
        selectInput("palette_select", "Select palette", choices = c(
          "Set1", "Set2", "Set3", "Dark2", "Accent"
        )),
        actionButton("pick_palette", label = "OK")
      ))
    }
  })
  observeEvent(input$pick_palette, {
    removeModal()
    color_palette(input$palette_select)
  })
  
  
  ##### setting initial plot ranges ####
  front_ranges <- reactiveValues(x = NULL, y = NULL)
  side_ranges <- reactiveValues(x = NULL, y = NULL)
  plan_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(jsonfile(), {
    data = data.frame(X = 0, Y = 0, Z = 0)
    if(!is.null(data.df())) {
      data = data.df()
    }
    front_ranges$x <- c(min(data$X) - 50, max(data$X) + 50)
    front_ranges$y <- c(min(data$Z) - 50, max(data$Z) + 50)
    side_ranges$x <- c(min(data$Y) - 50, max(data$Y) + 50)
    side_ranges$y <- c(min(data$Z) - 50, max(data$Z) + 50)
    plan_ranges$x <- c(min(data$X) - 50, max(data$X) + 50)
    plan_ranges$y <- c(min(data$Y) - 50, max(data$Y) + 50)
  })
  
  #### front plot ####
  front.plot = reactiveVal()
  output$frontView <- renderPlot({
    if(input$select_view == 3) {
      data = last.points()
    } else {
      data = data.df()
    }
    data = data %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
    
    #####draw front view#####
    s.units = unique(data$UNIT)
    if(!is.null(input$select_units)) {
      s.units = input$select_units
    }
    
    s.levels = unique(data$LEVEL)
    if(!is.null(input$select_levels)) {
      s.levels = input$select_levels
    }
    
    s.codes = unique(data$CODE)
    if(!is.null(input$select_code)) {
      s.codes = input$select_code
    }
    
    pdata = data %>% 
      filter(UNIT %in% s.units) %>%
      filter(LEVEL %in% s.levels) %>%
      filter(CODE %in% s.codes)
    plot.df(pdata)
    baseplot = ggplot(pdata, aes(x=X, y=Z)) + 
      geom_point() +
      coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
    p = baseplot
    
    #need to fix this so only multi-points are showing
    if(input$select_view == 2) {
      p = p + geom_line(aes(group = grp))
    }
    
    if(!is.null(special_point$data)) {
      p = p + geom_point(data = special_point$data, color = "red", size = 3) 
    }
    
    if(!is.null(input$color_select)) {
      if(input$color_select == 1) { #code
        p = p + geom_point(aes(x = X, y = Z, color = CODE))
        
      } else if(input$color_select == 2) { #unit
        p = p + geom_point(aes(x = X, y = Z, color = UNIT))
        
      } else { #level
        p = p + geom_point(aes(x = X, y = Z, color = LEVEL))
      }
      if(!is.null(color_palette())) {
        p = p + scale_color_brewer(palette = color_palette())
      }
    }
    
    if(!is.null(input$extra_plots)) { ##here can only plot datums
      if("1" %in% input$extra_plots) {
        datums = datums.df()
        datums = datums %>% select(X, Y, Z) %>%
          mutate_all(unlist) %>%
          mutate_all(as.numeric)
        p = p + geom_point(data = datums, aes(x = X, y = Y), 
                           size = 5, color = "blue")
      }
    }
    
    front.plot(p)
    return(p)
  })
  
  ##### front zoom ####
  observeEvent(input$front_dblclick, {
    brush = input$front_brush
    if (!is.null(brush)) {
      front_ranges$x <- c(brush$xmin, brush$xmax)
      front_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      data = data.df()
      front_ranges$x <- c(min(data$X) - 50, max(data$X) + 50)
      front_ranges$y <- c(min(data$Z) - 50, max(data$Z) + 50)
    }
  })
  
  #### side plot ####
  side.plot = reactiveVal()
  output$sideView <- renderPlot({
    if(input$select_view == 3) {
      data = last.points()
    } else {
      data = data.df()
    }
    
    if(!is.null(data)) {
      #dataToClean = data.df()
      if(input$select_view == 3) {
        dataToClean = last.points()
      } else {
        dataToClean = data.df()
      }
      dataToClean = dataToClean %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
      data = dataToClean
      plot.df(data)
      
      #####draw side view#####
      s.units = unique(data$UNIT)
      if(!is.null(input$select_units)) {
        s.units = input$select_units
      }
      
      s.levels = unique(data$LEVEL)
      if(!is.null(input$select_levels)) {
        s.levels = input$select_levels
      }
      
      s.codes = unique(data$CODE)
      if(!is.null(input$select_code)) {
        s.codes = input$select_code
      }
      
      pdata = data %>% filter(UNIT %in% s.units) %>%
        filter(LEVEL %in% s.levels) %>%
        filter(CODE %in% s.codes)
      plot.df(pdata)
      baseplot = ggplot(pdata, aes(x = Y, y = Z)) +
        geom_point() +
        coord_cartesian(xlim = side_ranges$x, ylim = side_ranges$y, expand = FALSE)
      p = baseplot
      
      
      if(input$select_view == 2) {
        p = p + geom_line(aes(group = grp))
      }
      
      if(!is.null(special_point$data)) {
        p = p + geom_point(data = special_point$data, color = "red", size = 3) 
      }
      
      if(!is.null(input$color_select)) {
        if(input$color_select == 1) { #code
          p = p + geom_point(aes(x = Y, y = Z, color = CODE)) +
            #scale_color_brewer(palette = "Paired")
            scale_color_colorblind()
        } else if(input$color_select == 2) { #unit
          p = p + geom_point(aes(x = Y, y = Z, color = UNIT)) +
            scale_color_colorblind()
        } else { #level
          p = p + geom_point(aes(x = Y, y = Z, color = LEVEL)) +
            scale_color_colorblind()
        }
      }
      
      if(!is.null(input$extra_plots)) { ##here can only plot datums
        if("1" %in% input$extra_plots) {
          datums = datums.df()
          datums = datums %>% select(X, Y, Z) %>%
            mutate_all(unlist) %>%
            mutate_all(as.numeric)
          p = p + geom_point(data = datums, aes(x = X, y = Y), 
                             size = 5, color = "blue")
        }
      }
      
      side.plot(p)
      return(p)
    }
    
  })
  
  ##### side zoom ####
  observeEvent(input$side_dblclick, {
    brush = input$side_brush
    if (!is.null(brush)) {
      side_ranges$x <- c(brush$xmin, brush$xmax)
      side_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      data = data.df()
      side_ranges$x <- c(min(data$Y) - 50, max(data$Y) + 50)
      side_ranges$y <- c(min(data$Z) - 50, max(data$Z) + 50)
    }
  })
  
  #### plan plot ####
  plan.plot = reactiveVal()
  output$planView <- renderPlot({
    # datalist = dataInput() 
    # data = datalist[[1]]
    if(input$select_view == 3) {
      data = last.points()
    } else {
      data = data.df()
    }
    
    if(!is.null(data)) {
      if(input$select_view == 3) {
        dataToClean = last.points()
      } else {
        dataToClean = data.df()
      }
      
      dataToClean = dataToClean %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
      data = dataToClean
      plot.df(data)
      
      ##### draw plan view #####
      s.units = unique(data$UNIT)
      if(!is.null(input$select_units)) {
        s.units = input$select_units
      }
      
      s.levels = unique(data$LEVEL)
      if(!is.null(input$select_levels)) {
        s.levels = input$select_levels
      }
      
      s.codes = unique(data$CODE)
      if(!is.null(input$select_code)) {
        s.codes = input$select_code
      }
      
      pdata = data %>% filter(UNIT %in% s.units) %>%
        filter(LEVEL %in% s.levels) %>%
        filter(CODE %in% s.codes)
      plot.df(pdata)
      baseplot = ggplot(pdata) +
        geom_point(aes(x = X, y = Y)) +
        coord_cartesian(xlim = plan_ranges$x, ylim = plan_ranges$y, expand = FALSE)
      p = baseplot
      
      if(input$select_view == 2) {
        p = p + geom_line(aes(group = grp))
      }
      
      if(!is.null(special_point$data)) {
        p = p + geom_point(data = special_point$data, aes(x = X, y = Y), color = "red", size = 3) 
      }
      
      if(!is.null(input$color_select)) {
        if(input$color_select == 1) { #code
          p = p + geom_point(aes(x = X, y = Y, color = CODE)) +
            #scale_color_brewer(palette = "Paired")
            scale_color_colorblind()
        } else if(input$color_select == 2) { #unit
          p = p + geom_point(aes(x = X, y = Y, color = UNIT)) +
            scale_color_colorblind()
        } else { #level
          p = p + geom_point(aes(x = X, y = Y, color = LEVEL)) +
            scale_color_colorblind()
        }
      }
      
      if(!is.null(input$extra_plots)) {
        if(length(input$extra_plots) == 1) {
          if(input$extra_plots == 1) { #datums
            if(!is.null(datums.df())) {
              datums = datums.df()
              datums = datums %>% select(X, Y, Z) %>%
                mutate_all(unlist) %>%
                mutate_all(as.numeric)
              p = p + geom_point(data = datums, aes(x = X, y = Y), 
                                 size = 5, color = "blue")
              
            }
          }
          if(input$extra_plots == 2) { #units
            units = units.df()
            units = units %>% select(MINX, MAXX, MINY, MAXY) %>%
              mutate_all(unlist) %>%
              mutate_all(as.numeric)
            
            p = p + geom_rect(data = units,
                              mapping = aes(xmin = MINX, xmax = MAXX, ymin = MINY, ymax = MAXY), 
                              color = "grey40", alpha = 0)
          }
        } else {
          datums = datums.df()
          datums = datums %>% select(X, Y, Z) %>%
            mutate_all(unlist) %>%
            mutate_all(as.numeric)
          
          units = units.df()
          units = units %>% select(MINX, MAXX, MINY, MAXY) %>%
            mutate_all(unlist) %>%
            mutate_all(as.numeric)
          
          p = p +
            geom_point(data = datums, aes(x = X, y = Y), 
                       size = 5, color = "blue") +
            geom_rect(data = units,
                      mapping = aes(xmin = MINX, xmax = MAXX, ymin = MINY, ymax = MAXY), 
                      color = "grey40", alpha = 0)
        }
      }
      
      plan.plot(p)
      return(p)
    }
    
  })
  
  ##### plan zoom ####
  observeEvent(input$plan_dblclick, {
    brush = input$plan_brush
    if (!is.null(brush)) {
      plan_ranges$x <- c(brush$xmin, brush$xmax)
      plan_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      data = data.df()
      plan_ranges$x <- c(min(data$X) - 50, max(data$X) + 50)
      plan_ranges$y <- c(min(data$Y) - 50, max(data$Y) + 50)
    }
  })
  
  #### data table for clicked point #####
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    data = plot.df()
    
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 5, addDist = T)
  })
  
  #### Edit point from plot ####
  orig_row = reactiveVal()
  orig_unit = reactiveVal()
  orig_id = reactiveVal()
  orig_prism = reactiveVal()
  observeEvent(input$edit,  {
    data = plot.df()
    
    df = data %>%
      select(ROW, UNIT, ID, SUFFIX, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    point = nearPoints(df, input$plot_click, threshold = 10, maxpoints = 1, addDist = F)
    orig_row(point$ROW)
    orig_unit(point$UNIT)
    orig_id(point$ID)
    orig_prism(point$PRISM)
    
    showModal(modalDialog(
      title = "Edit",
      conditionalPanel("false", textInput("row_input", label = "row", value = point$ROW)),
      #textInput("row_input", label = "ROW", value = point$ROW),
      fluidRow(
        column(4, textInput("unit_input", label = "UNIT", value = point$UNIT)),
        column(4, textInput("id_input", label = "ID", value = point$ID)),
        column(4, textInput("suffix_input", label = "SUFFIX", value = point$SUFFIX)),
      ),
      fluidRow(
        column(4, textInput("x_input", label = "X", value = point$X)),
        column(4, textInput("y_input", label = "Y", value = point$Y)),   
        column(4, textInput("z_input", label = "Z", value = point$Z)),
      ),
      fluidRow(
        column(12, textInput("prism_input", label = "PRISM", value = point$PRISM)), 
      ), 
      fluidRow(
        column(12, textInput("level_input", label = "LEVEL", value = point$LEVEL)),
      ), 
      fluidRow(
        column(12, textInput("code_input", label = "CODE", value = point$CODE)),
      ), 
      fluidRow(
        column(12, textInput("excav_input", label = "EXCAVATOR", value = point$EXCAVATOR)),
      ),
      actionButton("submit_edits", label = "Submit changes"),
      easyClose = TRUE,
      footer = NULL, 
      size = "m"
    ))
  })
  
  observeEvent(input$submit_edits, {
    removeModal()
    orig = data.df()
    datarow = orig[which(orig$ROW == orig_row())[[1]],]
    datarow$UNIT = input$unit_input
    datarow$ID = input$id_input
    datarow$SUFFIX = as.numeric(input$suffix_input)
    datarow$X = as.numeric(input$x_input)
    datarow$Y = as.numeric(input$y_input)
    datarow$PRISM = as.numeric(input$prism_input)
    datarow$LEVEL = input$level_input
    datarow$CODE = input$code_input
    datarow$EXCAVATOR = input$excav_input
    
    if(input$prism_input != orig_prism()) {
      shot_value = as.numeric(input$z_input) + orig_prism() #current stored Z + prism height
      new_Z = shot_value - as.numeric(input$prism_input)
    } else {
      new_Z = as.numeric(input$z_input)
    }
    datarow$Z = new_Z
    
    print(datarow)
    newdf = data.df()
    newdf[which(newdf$ROW == orig_row())[[1]],] = datarow
    data.df(newdf)
    
    jdata = jsondata()
    sp.df = split(newdf %>% select(-ROW), newdf$ROW)
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units", "UNIT"))]
    jdata[[dataname]] <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), jsonfile())
  })
  
  #### download from plot #####
  ###### download CSV of point data #######
  output$plot_download = downloadHandler(
    filename = function() {
      name = dbname()
      tab = str_replace(input$plots, " ", "-")
      return(paste0(name, "_", tab, "_", Sys.Date(), ".csv"))
    },
    content = function(file) {
      data = data.df()
      zoomed = data
      
      if(input$plots == "Front view") {
        zoomed = data %>%
          filter(X >= front_ranges$x[1] & X <= front_ranges$x[2]) %>%
          filter(Z >= front_ranges$y[1] & Z <= front_ranges$y[2])
        
      }else if(input$plots == "Side view") {
        zoomed = data %>%
          filter(Y >= side_ranges$x[1] & Y <= side_ranges$x[2]) %>%
          filter(Z >= side_ranges$y[1] & Z <= side_ranges$y[2])
        
      }else { #input$plots == "Plan view"
        zoomed = data %>%
          filter(X >= plan_ranges$x[1] & X <= plan_ranges$x[2]) %>%
          filter(Y >= plan_ranges$y[1] & Y <= plan_ranges$y[2])
      }
      
      vroom::vroom_write(zoomed, file, delim = ",")
    }
  )
  
  ###### download plot image png #######
  output$plot_im_download = downloadHandler(
    filename = function() {
      name = dbname()
      tab = str_replace(input$plots, " ", "-")
      return(paste0(name, "_", tab, "_", Sys.Date(), ".png"))
    },
    content = function(file) {
      
      if(input$plots == "Front view") {
        ggsave(filename = file, plot = front.plot(), dpi = 300, height = 5, width = 6)
        
      }else if(input$plots == "Side view") {
        ggsave(filename = file, plot = side.plot(), dpi = 300, height = 5, width = 6)
        
      }else { #input$plots == "Plan view"
        ggsave(filename = file, plot = plan.plot(), dpi = 300, height = 5, width = 6)
      }
    }
  )
  
  #### Units Data Table ####
  output$units = renderDT({ units.df() },  editable = FALSE, rownames = FALSE, selection = "none")
  #TODO fix unit editing
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
    write(toJSON(jdata), jsonfile())
    
  })
  
  
  #### Prisms Data Table ####
  output$prisms = renderDT({ prisms.df() }, editable = FALSE, rownames = FALSE, selection = "none")
  #TODO fix prism editing
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
    write(toJSON(jdata), jsonfile())
    
  })
  
  
  #### Datums Data Table ####
  output$datums = renderDT({ datums.df()}, editable = FALSE, rownames = FALSE, selection = "none")
  #TODO fix datum editing
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
    write(toJSON(jdata), jsonfile())
    
  })
}