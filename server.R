# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  #### Reactive Values ####
  jsonfile = reactiveVal()
  jsondata = reactiveVal()
  dbname = reactiveVal()
  
  data.df = reactiveVal()
  orig.df = reactiveVal()
  
  prisms.df = reactiveVal()
  units.df = reactiveVal()
  datums.df = reactiveVal()
  
  plot.df = reactiveVal()
  last.points = reactiveVal()
  
  #### Read file ####
  #####base shiny cfg input#####
  # cfgInput = reactive( {
  #   req(input$input_cfg)
  #   inFile = input$input_cfg
  #   ext = tools::file_ext(inFile$datapath)
  # 
  #   file = readLines(inFile$datapath)
  #   database = file[grepl("DATABASE", file)]
  #   name = str_remove(database, "DATABASE=")
  # 
  #   ##need to get last part and then find file locally
  # 
  #   table = file[grepl("TABLE", file)]
  #   tname = str_remove(table, "TABLE=")
  # 
  #   jdata = fromJSON(file = name)
  #   jsondata(jdata)
  #   dbname(tname)
  # 
  #   dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units"))]
  #   dflist = list()
  #   for(i in 1:length(jdata[[dataname]])) {
  #     dflist[[i]] = as.data.frame(jdata[[dataname]][[i]])
  #   }
  #   data = as.data.frame(do.call(smartbind, dflist))
  #   data[c(1:4, 6:8)] = sapply(data[c(1:4, 6:8)], as.numeric)
  #   data.df(data)
  #   orig.df(data)
  # 
  #   prisms = as.data.frame(do.call(rbind, jdata$prisms))
  #   prisms[c(2:3)] = sapply(prisms[c(2:3)], as.numeric)
  #   prisms.df(prisms)
  #   units = as.data.frame(do.call(rbind, jdata$units))
  #   units[c(2:5)] = sapply(units[c(2:5)], as.numeric)
  #   units.df(units)
  #   datums = as.data.frame(do.call(rbind, jdata$datums))
  #   datums[c(2:4)] = sapply(datums[c(2:4)], as.numeric)
  #   datums.df(datums)
  # 
  #   return(list(data, units, prisms, datums))
  # 
  # })
  
  #####shinyFiles json input#####
  roots = c(root = "~")
  shinyFileChoose(input, 'local_json', session=session, root = roots, filetypes=c('json'))
  #roots = c(roots, ) ##can roots be updated?
  
  jsonInput = reactive({
    req(input$local_json)
    inFile = parseFilePaths(roots = roots, input$local_json)
    jsonfile(as.character(inFile$datapath))
    
    jdata = fromJSON(file = as.character(inFile$datapath))
    jsondata(jdata)
    
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units", "UNIT"))]
    dbname(dataname)
    dflist = list()
    points = jdata[[dataname]]
    for(i in names(points)) {
      df = as.data.frame(points[[i]])
      df$ROW = i
      if("TIME" %in% rownames(df)) {
        df = df %>% select(c("ROW", "UNIT", "ID", "SUFFIX", "CODE", "LEVEL", "EXCAVATOR", "PRISM", "X", "Y", "Z", "DATE", "TIME", "HANGLE", "VANGLE", "SLOPED", "NOTES")) %>%
          mutate(HANGLE = as.character(HANGLE), 
                 VANGLE = as.character(VANGLE), 
                 SLOPED = as.character(SLOPED))
      } else {
        df$TIME = NA
        df = df %>% select(c("ROW", "UNIT", "ID", "SUFFIX", "CODE", "LEVEL", "EXCAVATOR", "PRISM", "X", "Y", "Z", "DATE", "TIME", "HANGLE", "VANGLE", "SLOPED", "NOTES")) %>%
          mutate(HANGLE = as.character(HANGLE), 
                 VANGLE = as.character(VANGLE), 
                 SLOPED = as.character(SLOPED))
      }
      dflist[[as.numeric(i)]] = df
    }
    
    data = bind_rows(dflist)
    #data = as.data.frame(do.call(smartbind, dflist))
    data.df(data)
    orig.df(data)
    
    date.data = data %>% filter(str_detect(DATE, "-"))
    date.data$DATE = strptime(date.data$DATE, format = "%F %H:%M:%S")
    today.data = date.data %>% filter(date(DATE) == Sys.Date())
    last.points(today.data)
    
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
      datums = bind_rows(datums.list)
      
    } else{
      datums = data.frame()
    }
    
    prisms.df(prisms)
    units.df(units)
    datums.df(datums)
    
  })
  
  
  #### Select input rendering ####
  output$unit_select = renderUI({
    data = data.df()
    selectizeInput("select_units", label = "Select units", 
                   choices = data$UNIT, multiple = T)
    
  })
  
  output$level_select = renderUI({
    data = data.df()
    selectizeInput("select_levels", label = "Select levels", 
                   choices = data$LEVEL, multiple = T)
    
  })
  
  output$code_select = renderUI({
    data = data.df()
    selectizeInput("select_code", label = "Select code", 
                   choices = data$CODE, multiple = T)
    
  })
  
  #### Data Table View ####
  output$printDF <- renderDT({
    if(is.null(data.df())) {
      ##there is an issue here that throws an error as soon as File select is clicked
      ##but it doesn't affect how the program functions
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
    #print(jdata[[dataname]])
    jdata[[dataname]] <- sp.df
    jsondata(jdata)
    #write(toJSON(jdata), "test.json")
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
  observeEvent(input$clear_plot, {
    updateSelectInput(session, "select_view", 
                      choices = list("Points" = 1, "Multi-points" = 2, "Last points" = 3), 
                      selected = 1)
    updateSelectizeInput(session, server = T, "select_units", selected = NULL)
    updateSelectizeInput(session, server = T, "select_levels", selected = NULL)
    updateSelectizeInput(session, server = T, "select_code", selected = NULL)
    updateCheckboxGroupInput(session, "color_select", choices = list("Code" = 1, "Unit" = 2, "Level" = 3), selected = NULL)
    updateCheckboxGroupInput(session, "extra_plots", choices = list("Datums" = 1, "Units (only in plan view)" = 2), selected = NULL)
  })
  
  special_point <- reactiveValues(data = NULL)
  observeEvent(input$find, {
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
    special_point$data = data %>% filter(UNIT == input$find_unit & ID == input$find_id)
    #print(special_point$data)
  })
  
  observeEvent(input$clear_find, {
    special_point$data = NULL
    updateTextInput(session, "find_unit", value = "")
    updateTextInput(session, "find_id", value = "")
  })
  
  front_ranges <- reactiveValues(x = NULL, y = NULL)
  front.plot = reactiveVal()
  output$frontView <- renderPlot({
    #data = data.df()
    
    if(input$select_view == 3) {
      data = last.points()
    } else {
      data = data.df()
    }
    
    if(!is.null(data)) {
      
      if(is.null(front_ranges$x)) {
        front_ranges$x <- c(min(data$X) - 100, max(data$X) + 100)
      }
      
      if(is.null(front_ranges$y)) {
        front_ranges$y <- c(min(data$Z) - 100, max(data$Z) + 100)
      }
      
      if(input$select_view == 3) {
        dataToClean = last.points()
      } else {
        dataToClean = data.df()
      }
      dataToClean = dataToClean %>% group_by(UNIT, ID) %>% mutate(grp = cur_group_id())
      data = dataToClean
      plot.df(data)
      
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
      
      pdata = data %>% filter(UNIT %in% s.units) %>%
        filter(LEVEL %in% s.levels) %>%
        filter(CODE %in% s.codes)
      plot.df(pdata)
      baseplot = ggplot(pdata, aes(x=X, y=Z)) + 
        geom_point() +
        coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      
      # if(!is.null(input$select_units)) {
      #   ##filter select units
      #   plot.df(data %>% filter(UNIT %in% input$select_units))
      #   baseplot = ggplot(data %>% filter(UNIT %in% input$select_units),
      #                     aes(x = X, y = Z)) +
      #     geom_point() +
      #     coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      #   
      # } else if(!is.null(input$select_levels)) {
      #   ##filter select levels
      #   plot.df(data %>% filter(LEVEL %in% input$select_levels))
      #   baseplot = ggplot(data %>% filter(LEVEL %in% input$select_levels)
      #                     , aes(x = X, y = Z)) +
      #     geom_point() +
      #     coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      #   
      # } else if(!is.null(input$select_code)) {
      #   ## filter select codes
      #   plot.df(data %>% filter(CODE %in% input$select_code))
      #   baseplot = ggplot(data %>% filter(CODE %in% input$select_code)
      #                     , aes(x = X, y = Z)) +
      #     geom_point() +
      #     coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      #   
      # } else {
      #   ## plot all points
      #   plot.df(data)
      #   baseplot = ggplot(data, aes(x = X, y = Z)) +
      #     geom_point() +
      #     coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
      # }
      
      p = baseplot
      
      if(input$select_view == 2) {
        p = p + geom_line(aes(group = grp))
      }
      
      if(!is.null(special_point$data)) {
        p = p + geom_point(data = special_point$data, color = "red", size = 3) 
      }
      
      if(!is.null(input$color_select)) {
        if(input$color_select == 1) { #code
          p = p + geom_point(aes(x = X, y = Z, color = CODE)) +
            scale_color_brewer(palette = "Paired")
          
        } else if(input$color_select == 2) { #unit
          p = p + geom_point(aes(x = X, y = Z, color = UNIT)) +
            scale_color_colorblind()
          
        } else { #level
          p = p + geom_point(aes(x = X, y = Z, color = LEVEL)) +
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
      
      front.plot(p)
      return(p)
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
  side.plot = reactiveVal()
  output$sideView <- renderPlot({
    # datalist = dataInput() 
    # data = datalist[[1]]
    #data = data.df()
    
    if(input$select_view == 3) {
      data = last.points()
    } else {
      data = data.df()
    }
    
    if(!is.null(data)) {
      
      if(is.null(side_ranges$x)) {
        side_ranges$x <- c(min(data$Y) - 100, max(data$Y) + 100)
      }
      
      if(is.null(side_ranges$y)) {
        side_ranges$y <- c(min(data$Z) - 100, max(data$Z) + 100)
      }
      
      
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
            scale_color_brewer(palette = "Paired")
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
      
      if(is.null(plan_ranges$x)) {
        plan_ranges$x <- c(min(data$X) - 100, max(data$X) + 100)
      }
      
      if(is.null(plan_ranges$y)) {
        plan_ranges$y <- c(min(data$Y) - 100, max(data$Y) + 100)
      }
      
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
            scale_color_brewer(palette = "Paired")
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
  
  ##### data table for clicked point #####
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    # datalist = dataInput() 
    # data = datalist[[1]]
    if(is.null(plot.df())) {
      jsonInput()
      data = plot.df()
    } else {
      data = plot.df()
    }
    
    df = data %>%
      select(UNIT, ID, X, Y, Z, PRISM, LEVEL, CODE, EXCAVATOR)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 5, addDist = T)
  })
  
  ##### download from plot #####
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
  
  
  #### Edit point from plot ####
  orig_row = reactiveVal()
  orig_unit = reactiveVal()
  orig_id = reactiveVal()
  orig_prism = reactiveVal()
  observeEvent(input$edit,  {
    # datalist = dataInput() 
    # data = datalist[[1]]
    data = data.df()
    
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
      textInput("unit_input", label = "UNIT", value = point$UNIT),
      textInput("id_input", label = "ID", value = point$ID),
      numericInput("suffix_input", label = "SUFFIX", value = point$SUFFIX),
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
    
    orig = data.df()
    #datarow = orig[which(orig$UNIT == orig_unit()[[1]] & orig$ID == orig_id())[[1]],]
    datarow = orig[which(orig$ROW == orig_row())[[1]],]
    #print(datarow)
    datarow$UNIT = input$unit_input
    datarow$ID = input$id_input
    datarow$SUFFIX = input$suffix_input
    datarow$X = input$x_input
    datarow$Y = input$y_input
    datarow$PRISM = input$prism_input
    datarow$LEVEL = input$level_input
    datarow$CODE = input$code_input
    datarow$EXCAVATOR = input$excav_input
    
    if(input$prism_input != orig_prism()) {
      shot_value = input$z_input + orig_prism() #current stored Z + prism height
      new_Z = shot_value - input$prism_input
    } else {
      new_Z = input$z_input
    }
    datarow$Z = new_Z
    
    print(datarow)
    newdf = data.df()
    newdf[which(newdf$ROW == orig_row())[[1]],] = datarow
    data.df(newdf)
    plot.df(newdf)
    
    jdata = jsondata()
    sp.df = split(newdf %>% select(-ROW), newdf$ROW)
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units", "UNIT"))]
    jdata[[dataname]] <- sp.df
    jsondata(jdata)
    write(toJSON(jdata), jsonfile())
  })
  
}