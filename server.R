# Define server
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  #### Reactive Values ####
  parentdir = reactiveVal()
  jsonfile = reactiveVal()
  jsondata = reactiveVal()
  site_name = reactiveVal()
  dbname = reactiveVal()
  
  data.df = reactiveVal()
  db.df = reactiveVal()
  prisms.df = reactiveVal()
  units.df = reactiveVal()
  datums.df = reactiveVal()
  
  plot.df = reactiveVal()
  last.points = reactiveVal()
  
  #for doing database edits
  orig_unit = reactiveVal()
  orig_id = reactiveVal()
  orig_suffix = reactiveVal()
  orig_prism = reactiveVal()
  
  #### newplot_shiny instructions ####
  observe({
    showModal(modalDialog(
      title = "Welcome to newplot_shiny",
      HTML("<p>Please transfer the site folder to your computer and open the JSON file in newplot_shiny.</p> 
      <p>Once the field data has loaded in the data transfer tab, please click transfer data before performing any plotting or edits.</p>
           <p>If you have questions, contact Emily Coco</p>"),
      footer = modalButton("Got it")
    ))
  })
  
  #### Read files ####
  #####shinyFiles json input#####
  roots = c(root = "~")
  
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    print("You are using Windows.")
    roots = c(root = "..")
  } else if (os == "Darwin") {
    print("You are using macOS.")
    roots = c(root = "~")
  } else if (os == "Linux") {
    print("You are using Linux.")
    roots = c(root = "/")
  } else {
    print("Unknown operating system.")
  }
  
  shinyFileChoose(input, 'local_json', session=session, root = roots, filetypes=c('json'))
  
  jsonFile = reactive({
    req(input$local_json)
    if(is.null(input$local_json)) {
      return(NULL)
    }
    jsonfile(parseFilePaths(roots = roots, input$local_json)$datapath)
    
    #get directory path
    fullpath = parseFilePaths(roots = roots, input$local_json)$datapath
    fp_split = str_split(fullpath, "/")[[1]]
    sitename = str_remove(fp_split[length(fp_split)], ".json")
    site_name(sitename)
    clean_fp = paste0(fp_split[-length(fp_split)], collapse = "/")
    parentdir(clean_fp)
    
    output$json_file_name <- renderText({parseFilePaths(roots = roots, input$local_json)$datapath})
    return(parseFilePaths(roots = roots, input$local_json)$datapath)
  })
  
  #####load database data#####
  read_database <- function() {
    dbdir = paste0(parentdir(), "/database")
    if(dir.exists(dbdir)) {
      context_table = read.csv(paste0(dbdir, "/context.csv"))
      xyz_table = read.csv(paste0(dbdir, "/xyz.csv"))
      
      db.data = xyz_table %>% left_join(context_table, by = c("Unit", "ID")) %>%
        select(Unit, ID, Suffix, code, level, excavator, Prism, X, Y, Z, Date, Time, NOTES, last_import)
      db.df(db.data)
      
    } else {
      db.data = data.frame()
    }
  }
  
  ##### get most recent points ####  
  observeEvent(db.df(), { #get last points
    data = db.df()
    last.data = data %>% filter(last_import == TRUE)
    last.points(last.data)
  })
  
  #####load field data#####
  read_json = function(){
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
    if(nrow(data) == 0) {
      data = data.frame()
    }
    data.df(data)
    
    
    if("prisms" %in% names(jdata)) {
      prisms = as.data.frame(do.call(rbind, jdata$prisms))
    } else{
      prisms = data.frame()
    }
    
    if("units" %in% names(jdata)){
      units.list = list()
      for(i in names(jdata$units)) {
        unit = as.data.frame(jdata$units[[i]])
        unit$ROW = i
        if(!("RADIUS" %in% rownames(unit))) {
          unit$ROW = i
          unit$ID = ""
          unit$CENTERX = NA
          unit$CENTERY = NA
          unit$RADIUS = NA
        }
        unit = unit[,c("ROW", "NAME", "MINX", "MINY", "MAXX", "MAXY", "CENTERX", "CENTERY", "RADIUS")]
        units.list[[as.numeric(i)]] = unit
      }
      units = rbindlist(units.list)
    } else{
      units = data.frame()
    }
    
    if("datums" %in% names(jdata)) {
      datums.list = list()
      for(i in names(jdata$datums)) {
        datums.list[[i]] = as.data.frame(jdata$datums[[i]]) %>% select(NAME, X, Y, Z) %>%
          mutate(X = as.numeric(X), Y = as.numeric(Y), Z = as.numeric(Z))
      }
      datums = rbindlist(datums.list)
    } else{
      datums = data.frame()
    }
    
    prisms.df(prisms)
    units.df(units)
    datums.df(datums)
    
    read_database()
  }
  
  #### Write files ####
  write_database_edits = function(newrow) {
    #get database files
    dbdir = paste0(parentdir(), "/database")
    context_table = read.csv(paste0(dbdir, "/context.csv"))
    xyz_table = read.csv(paste0(dbdir, "/xyz.csv"))
    
    #get or create edit log directory
    edit_dir = paste0(parentdir(), "/edit_logs")
    if(!dir.exists(edit_dir)) {
      dir.create(edit_dir)
    } 
    
    #get or create edit log file
    edit_fp = paste0(edit_dir, "/", site_name(), "_", Sys.Date(), ".txt")
    if(!file.exists(edit_fp)) {
      file.create(edit_fp)
    } 
    
    #update Context table
    old_crow = context_table[which(context_table$Unit == orig_unit() 
                                   & context_table$ID == orig_id())[[1]],]
    new_crow = context_table[which(context_table$Unit == orig_unit() 
                                   & context_table$ID == orig_id())[[1]],]
    new_crow$Unit = newrow$Unit
    if(new_crow$Unit != old_crow$Unit) {
      write(paste0(new_crow$Unit, "-", new_crow$ID, ": unit changed from ", old_crow$Unit, " to ", new_crow$Unit), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_crow$ID = newrow$ID
    if(new_crow$ID != old_crow$ID) {
      write(paste0(new_crow$Unit, "-", new_crow$ID, ": ID changed from ", old_crow$ID, " to ", new_crow$ID), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_crow$SQUID = paste0(newrow$Unit, "-", newrow$ID)
    new_crow$level = newrow$level
    if(new_crow$level != old_crow$level) {
      write(paste0(new_crow$Unit, "-", new_crow$ID, ": level changed from ", old_crow$level, " to ", new_crow$level), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_crow$code = newrow$code
    if(new_crow$code != old_crow$code) {
      write(paste0(new_crow$Unit, "-", new_crow$ID, ": code changed from ", old_crow$code, " to ", new_crow$code), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_crow$excavator = newrow$excavator
    if(new_crow$excavator != old_crow$excavator) {
      write(paste0(new_crow$Unit, "-", new_crow$ID, ": excavator changed from ", old_crow$excavator, " to ", new_crow$excavator), 
            file = edit_fp, sep = "\n", append = T)
    }
    
    context_table[which(context_table$Unit == orig_unit() 
                        & context_table$ID == orig_id())[[1]],] = new_crow
    
    #update XYZ table
    old_xyzrow = xyz_table[which(xyz_table$Unit == orig_unit() 
                                 & xyz_table$ID == orig_id() 
                                 & xyz_table$Suffix == orig_suffix())[[1]],]
    new_xyzrow = xyz_table[which(xyz_table$Unit == orig_unit() 
                                 & xyz_table$ID == orig_id() 
                                 & xyz_table$Suffix == orig_suffix())[[1]],]
    new_xyzrow$Unit = newrow$Unit
    new_xyzrow$ID = newrow$ID
    new_xyzrow$Suffix = newrow$Suffix
    if(new_xyzrow$Suffix != old_xyzrow$Suffix) {
      write(paste0(new_xyzrow$Unit, "-", new_xyzrow$ID, ": suffix changed from ", old_xyzrow$Suffix, " to ", new_xyzrow$Suffix), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_xyzrow$Prism = newrow$Prism
    if(new_xyzrow$Prism != old_xyzrow$Prism) {
      write(paste0(new_xyzrow$Unit, "-", new_xyzrow$ID, ": prism changed from ", old_xyzrow$Prism, " to ", new_xyzrow$Prism), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_xyzrow$X = newrow$X
    if(new_xyzrow$X != old_xyzrow$X) {
      write(paste0(new_xyzrow$Unit, "-", new_xyzrow$ID, ": X changed from ", old_xyzrow$X, " to ", new_xyzrow$X), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_xyzrow$Y = newrow$Y
    if(new_xyzrow$Y != old_xyzrow$Y) {
      write(paste0(new_xyzrow$Unit, "-", new_xyzrow$ID, ": Y changed from ", old_xyzrow$Y, " to ", new_xyzrow$Y), 
            file = edit_fp, sep = "\n", append = T)
    }
    new_xyzrow$Z = newrow$Z
    if(new_xyzrow$Z != old_xyzrow$Z) {
      write(paste0(new_xyzrow$Unit, "-", new_xyzrow$ID, ": Z changed from ", old_xyzrow$Z, " to ", new_xyzrow$Z), 
            file = edit_fp, sep = "\n", append = T)
    }
    
    xyz_table[which(xyz_table$Unit == orig_unit() 
                    & xyz_table$ID == orig_id() 
                    & xyz_table$Suffix == orig_suffix())[[1]],] = new_xyzrow
    
    write_csv(context_table, file = paste0(dbdir, "/context.csv"))
    write_csv(xyz_table, file = paste0(dbdir, "/xyz.csv"))
  }
  
  
  #### Data Transfer View ####
  output$printDF_json <- renderDT({
    read_json()
    data = data.df()
  },
  editable = FALSE, 
  rownames = FALSE, 
  selection = "none"
  )
  
  ##### data transfer #####
  observeEvent(input$data_transfer, {
    dbdir = paste0(parentdir(), "/database")
    ftdir = paste0(parentdir(), "/field-backup")
    
    output$point_count <- renderText({paste0("Transferring ", nrow(data.df()), " points")})
    
    showModal(modalDialog(
      title = "Transfer data:", 
      fluidRow(
        column(12, textOutput("point_count"))
      ),
      fluidRow(
        column(12, textInput("db_dir", label = "Transfer points to database:", value = dbdir, width = '100%')),
        column(12, textInput("ft_dir", label = "Backup points in folder:", value = ftdir, width = '100%')),
      ),
      actionButton("submit_data_transfer", label = "Confirm data transfer"),
      easyClose = T,
      size = "l"
    ))
  })
  
  observeEvent(input$submit_data_transfer, {
    removeModal()
    
    dbdir = input$db_dir
    ftdir = input$ft_dir
    
    #check for field transfer backup folder
    if(!dir.exists(ftdir)) {
      dir.create(ftdir)
    }
    #write field transfer backup first
    write_delim(data.df(), file = paste0(ftdir, "/", site_name(), "_", dbname(), "_", Sys.Date(), "_", format(Sys.time(), "%H-%M-%S"), ".txt"))
    
    edm_units = data.frame(
      name = character(), 
      minX = numeric(), 
      minY = numeric(), 
      maxX = numeric(), 
      maxY = numeric(), 
      centerX = numeric(), 
      centerY = numeric(), 
      radius = numeric()
    )
    
    edm_datums = data.frame(
      name = character(), 
      X = numeric(), 
      Y = numeric(), 
      Z = numeric()
    )
    
    edm_poles = data.frame(
      name = character(), 
      height = numeric(), 
      offset = numeric()
    )
    
    context_table = data.frame(
      Unit = character(), 
      ID = character(), 
      SQUID = character(), 
      level = character(), 
      code = character(), 
      excavator = character(), 
      recno = numeric(), 
      NOTES = character()
    )
    xyz_table = data.frame(
      Unit = character(), 
      ID = character(), 
      Suffix = numeric(), 
      Prism = numeric(), 
      X = numeric(), 
      Y = numeric(), 
      Z = numeric(), 
      Date = character(), 
      Time = character(),
      Year = numeric(), 
      RecordCounter = numeric(), 
      last_import = logical()
    )
    #check for database folder
    if(!dir.exists(dbdir)) {
      dir.create(dbdir)
      write_csv(context_table, file = paste0(dbdir, "/context.csv"))
      write_csv(xyz_table, file = paste0(dbdir, "/xyz.csv"))
      write_csv(edm_units, file = paste0(dbdir, "/units.csv"))
      write_csv(edm_datums, file = paste0(dbdir, "/datums.csv"))
      write_csv(edm_poles, file = paste0(dbdir, "/poles.csv"))
    } else {
      context_table = read.csv(paste0(dbdir, "/context.csv"))
      xyz_table = read.csv(paste0(dbdir, "/xyz.csv"))
      xyz_table$last_import = FALSE
      edm_units = read.csv(paste0(dbdir, "/units.csv"))
      edm_datums = read.csv(paste0(dbdir, "/datums.csv"))
      edm_poles = read.csv(paste0(dbdir, "/poles.csv"))
    }
    
    
    #now write json data to context and xyz dataframes
    data = data.df()
    
    c.recno = 1
    if(nrow(context_table) > 0) {
      c.recno = max(context_table$recno, na.rm = T) + 1
    }
    xyz.recno = 1
    if(nrow(xyz_table) > 0) {
      xyz.recno = max(xyz_table$RecordCounter, na.rm = T) + 1
    }
    withProgress(message = "Transferring points", value = 0, {
      for(i in 1:nrow(data)) {
        rowdf = data[i,]
        #stop duplicates in the context table
        check.context = context_table[0,]
        if(nrow(context_table) > 0) {
          check.context = context_table %>% filter(Unit == rowdf$UNIT & ID == rowdf$ID)
        }
        
        if(nrow(check.context) == 0) {
          crow = data.frame(
            Unit = as.character(rowdf$UNIT),
            ID = as.character(rowdf$ID),
            SQUID = as.character(paste0(rowdf$UNIT, "-", rowdf$ID)),
            level = as.character(rowdf$LEVEL),
            code = tolower(as.character(rowdf$CODE)),
            excavator = as.character(rowdf$EXCAVATOR),
            recno = c.recno,
            NOTES = as.character(rowdf$NOTES),
            stringsAsFactors = FALSE  
          )
          context_table = rbind(context_table, crow)
          c.recno = c.recno + 1
        }
        
        date = Sys.Date()
        time = format(Sys.time(), "%T")
        year = as.numeric(format(date, "%Y"))
        if(str_detect(rowdf$DATE, "/")) {
          date.string = paste0(
            substr(rowdf$DATE, 1, nchar(rowdf$DATE) - 8), 
            " ", 
            substr(rowdf$DATE, nchar(rowdf$DATE) - 7, nchar(rowdf$DATE))
          )
          data.date = strptime(date.string, format="%m/%d/%Y %H:%M:%S")
          date = format(data.date, "%D")
          time = format(data.date, "%T")
          year = format(data.date, "%Y")
        } else {
          date = format(as.Date(rowdf$DATE), "%D")
          time = format(as.Date(rowdf$DATE), "%T")
          year = format(as.Date(rowdf$DATE), "%Y")
        }
        
        xyzrow = data.frame(
          Unit = rowdf$UNIT,
          ID =  rowdf$ID,
          Suffix = rowdf$SUFFIX,
          Prism = rowdf$PRISM, 
          X = rowdf$X, 
          Y = rowdf$Y,  
          Z = rowdf$Z,
          Date = date, 
          Time = time,
          Year = year,
          RecordCounter = xyz.recno,
          last_import = TRUE,
          stringsAsFactors = FALSE  
        )
        xyz_table = rbind(xyz_table, xyzrow)
        xyz.recno = xyz.recno + 1
        
        incProgress(1/nrow(data), detail = paste0("Transferring point ", i, " of ", nrow(data)))
      }
      
      write_csv(context_table, file = paste0(dbdir, "/context.csv"))
      write_csv(xyz_table, file = paste0(dbdir, "/xyz.csv"))
    })
    
    #blast points from JSON
    jdata = jsondata()
    dataname = names(jdata)[!(names(jdata) %in% c("prisms", "datums", "units", "UNIT"))]
    empty_points = jdata[[dataname]]
    empty_points[1:length(empty_points)] <- NULL
    jdata[[dataname]] <- empty_points
    jsondata(jdata)
    write(toJSON(jdata), jsonfile())
    
    #write datum data, pole data, unit data
    units = units.df()
    datums = datums.df()
    poles = prisms.df()
    
    for(i in 1:nrow(units)) {
      u.rowdf = units[i,]
      if(!(u.rowdf$NAME %in% edm_units$name)) {
        urow = data.frame(
          name = u.rowdf$NAME, 
          minX = as.numeric(u.rowdf$MINX), 
          minY = as.numeric(u.rowdf$MINY),
          maxX = as.numeric(u.rowdf$MAXX), 
          maxY = as.numeric(u.rowdf$MAXY), 
          centerX = as.numeric(u.rowdf$CENTERX), 
          centerY = as.numeric(u.rowdf$CENTERY), 
          radius = as.numeric(u.rowdf$RADIUS)
        )
        edm_units = rbind(edm_units, urow)
      } else {
        edm_units[which(edm_units$name == u.rowdf$NAME),]$minX = as.numeric(u.rowdf$MINX)
        edm_units[which(edm_units$name == u.rowdf$NAME),]$minY = as.numeric(u.rowdf$MINY)
        edm_units[which(edm_units$name == u.rowdf$NAME),]$maxX = as.numeric(u.rowdf$MAXX)
        edm_units[which(edm_units$name == u.rowdf$NAME),]$maxY = as.numeric(u.rowdf$MAXY)
        edm_units[which(edm_units$name == u.rowdf$NAME),]$centerX = as.numeric(u.rowdf$CENTERX)
        edm_units[which(edm_units$name == u.rowdf$NAME),]$centerY = as.numeric(u.rowdf$CENTERY)
        edm_units[which(edm_units$name == u.rowdf$NAME),]$radius = as.numeric(u.rowdf$RADIUS)
      }
    }
    write_csv(edm_units, file = paste0(dbdir, "/units.csv"))
    
    for(i in 1:nrow(datums)) {
      d.rowdf = datums[i,]
      if(!(d.rowdf$NAME %in% edm_datums$name)) {
        drow = data.frame(
          name = d.rowdf$NAME, 
          X = as.numeric(d.rowdf$X), 
          Y = as.numeric(d.rowdf$Y), 
          Z = as.numeric(d.rowdf$Z)
        )
        edm_datums = rbind(edm_datums, drow)
      } else {
        edm_datums[which(edm_datums$name == d.rowdf$NAME),]$X = as.numeric(d.rowdf$X)
        edm_datums[which(edm_datums$name == d.rowdf$NAME),]$Y = as.numeric(d.rowdf$Y)
        edm_datums[which(edm_datums$name == d.rowdf$NAME),]$Z = as.numeric(d.rowdf$Z)   
      }
    }
    write_csv(edm_datums, file = paste0(dbdir, "/datums.csv"))
    
    for(i in 1:nrow(poles)) {
      p.rowdf = poles[i,]
      if(!(p.rowdf$NAME %in% edm_poles$name)) {
        prow = data.frame(
          name = as.character(p.rowdf$NAME), 
          height = as.numeric(p.rowdf$HEIGHT), 
          offset = as.numeric(p.rowdf$OFFSET)
        )
        edm_poles = rbind(edm_poles, prow)
      } else {
        edm_poles[which(edm_poles$name == p.rowdf$NAME),]$height = as.numeric(p.rowdf$HEIGHT)
        edm_poles[which(edm_poles$name == p.rowdf$NAME),]$offset = as.numeric(p.rowdf$OFFSET)
        
      }
    }
    write_csv(edm_poles, file = paste0(dbdir, "/poles.csv"))
    
    #trigger reading from json again
    read_json()
    
    #trigger reading data from database
    read_database()
  })
  
  
  #### Database View ####
  output$printDF <- renderDT({
    read_database()
    
    validate(
      need(!is.null(db.df()), "No data has been read in.")
    )
    
    data = db.df()
  },
  editable = TRUE, 
  rownames = FALSE, 
  selection = "none"
  )
  
  ##### edit data table #####
  observeEvent(input$printDF_cell_edit, {
    info = input$printDF_cell_edit
    i = info$row
    j = info$col + 1
    
    orig = db.df()
    orig_row = orig[i,]
    orig_unit(orig_row$Unit)
    orig_id(orig_row$ID)
    orig_suffix(orig_row$Suffix)
    orig_prism(orig_row$Prism)
    
    newdf = db.df()
    
    newdf[i,j] = info$value
    db.df(newdf)
    plot.df(newdf)
    
    newrow = newdf[i, ]
    if(colnames(newdf[i,])[j] == "Prism") {
      shot_value = as.numeric(newrow$Z) + orig_prism() #current stored Z + prism height
      new_Z = shot_value - as.numeric(newrow$Prism)
      newrow$Z = new_Z
    }
    
    print(newrow)
    write_database_edits(newrow)
  })
  
  ##### download from data table #####
  output$download = downloadHandler(
    filename = function() {
      name = dbname()
      return(paste0(name, "_", Sys.Date(), ".csv"))
    },
    content = function(file) {
      vroom::vroom_write(db.df(), file, delim = ",")
    }
  )
  
  #### Plotting ####
  ##### color palettes #####
  assign_random_colors = function(categories) {
    colors = colors()
    random_colors = sample(colors, length(categories), replace = F)
    category_colors = setNames(random_colors, categories)
    return(category_colors)
  }
  
  unit_palette = reactiveValues(colors = c())
  code_palette = reactiveValues(colors = c())
  level_palette = reactiveValues(colors = c())
  
  category_palette = reactiveValues(colors = NULL)
  
  ##### Select input rendering ####
  observeEvent(db.df(), {
    data = db.df()
    updateSelectizeInput(session, "select_units", choices = sort(unique(data$Unit)), server = T, 
                         selected = input$select_units)
    updateSelectizeInput(session, "select_levels", choices = sort(unique(data$level)), server = T, 
                         selected = input$select_levels)
    updateSelectizeInput(session, "select_code", choices = sort(unique(data$code)), server = T, 
                         selected = input$select_code)
  })
  
  ##### Clear selections ####
  observeEvent(input$clear_plot, {
    data = db.df()
    category_palette$colors = NULL
    updateSelectInput(session, "select_view", label = "Select point view", 
                      choices = list("All points" = 1, "Last points" = 2), 
                      selected = 1)
    updateSelectizeInput(session, "select_units", choices = sort(unique(data$Unit)), server = T, selected = NULL)
    updateSelectizeInput(session, "select_levels", choices = sort(unique(data$level)), server = T, selected = NULL)
    updateSelectizeInput(session, "select_code", choices = sort(unique(data$code)), server = T, selected = NULL)
    updateCheckboxGroupInput(session, "color_select", choices = list("Code" = 1, "Unit" = 2, "Level" = 3), selected = NULL, inline = T)
    updateCheckboxGroupInput(session, "extra_plots", choices = list("Datums" = 1, "Units" = 2, "Multi-points" = 3), selected = NULL, inline = T)
  })
  
  ##### highlight "found" point ####
  special_point <- reactiveValues(data = NULL)
  observeEvent(input$find, {
    data = db.df()
    special_point$data = data %>% filter(Unit == input$find_unit & ID == input$find_id)
  })
  
  observeEvent(input$clear_find, {
    special_point$data = NULL
    updateTextInput(session, "find_unit", value = "")
    updateTextInput(session, "find_id", value = "")
  })
  
  #####picking palettes #####
  observeEvent(input$color_select, {
    data = db.df()
    categories = c()
    if(input$color_select == 1) { #code
      if(!is.null(input$select_code)) {
        categories = input$select_code
      } else {
        categories = sort(unique(data$code))
      }
    } else if(input$color_select == 2) { #unit
      if(!is.null(input$select_units)) {
        categories = input$select_units
      } else {
        categories = sort(unique(data$Unit))
      }
    } else { #level
      if(!is.null(input$select_levels)) {
        categories = input$select_levels
      } else {
        categories = sort(unique(data$level))
      }
      categories = sort(unique(data$level))
    }
    
    categories = categories[categories != ""]
    
    category_palette$colors = assign_random_colors(categories)
    selected_colors = category_palette$colors
    
    half_cat = ceiling(length(categories)/2)
    
    showModal(modalDialog(
      fluidRow(
        column(6, lapply(categories[1:half_cat], function(cat) {
          colourInput(inputId = paste0("color_", cat), label = cat, value = selected_colors[cat])
        })),
        column(6, lapply(categories[(half_cat + 1):length(categories)], function(cat) {
          colourInput(inputId = paste0("color_", cat), label = cat, value = selected_colors[cat])
        }))
      ),
      actionButton("pick_palette", label = "OK"), 
      easyClose = T
    ))
  })
  
  observeEvent(input$pick_palette, {
    removeModal()
    data = db.df()
    if(input$color_select == 1) { #code
      if(!is.null(input$select_code)) {
        categories = input$select_code
      } else {
        categories = sort(unique(data$code))
      }
    } else if(input$color_select == 2) { #unit
      if(!is.null(input$select_units)) {
        categories = input$select_units
      } else {
        categories = sort(unique(data$Unit))
      }
    } else { #level
      if(!is.null(input$select_levels)) {
        categories = input$select_levels
      } else {
        categories = sort(unique(data$level))
      }
      categories = sort(unique(data$level))
    }
    
    categories = categories[categories != ""]
    
    selected_colors = c()
    for (cat in categories) {
      selected_colors = c(selected_colors, input[[paste0("color_", cat)]])
    }
    category_colors = setNames(selected_colors, categories)
    
    category_palette$colors = category_colors
  })
  
  
  ##### setting initial plot ranges ####
  front_ranges <- reactiveValues(x = NULL, y = NULL)
  side_ranges <- reactiveValues(x = NULL, y = NULL)
  plan_ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(db.df(), {
    data = data.frame(X = 0, Y = 0, Z = 0)
    if(!is.null(db.df())) {
      data = db.df()
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
    read_database()
    
    validate(
      need(!is.null(db.df()), "No data has been read in.")
    )
    
    if(input$select_view == 2) {
      data = last.points()
    } else {
      data = db.df()
    }
    
    data = data %>% group_by(Unit, ID) %>% mutate(grp = cur_group_id())
    
    if(input$select_view == 3) {
      data = data[duplicated(data$grp) | duplicated(data$grp, fromLast = TRUE), ]
    }
    
    #####draw front view#####
    s.units = unique(data$Unit)
    if(!is.null(input$select_units)) {
      s.units = input$select_units
    }
    
    s.levels = unique(data$level)
    if(!is.null(input$select_levels)) {
      s.levels = input$select_levels
    }
    
    s.codes = unique(data$code)
    if(!is.null(input$select_code)) {
      s.codes = input$select_code
    }
    
    pdata = data %>% 
      filter(Unit %in% s.units) %>%
      filter(level %in% s.levels) %>%
      filter(code %in% s.codes)
    plot.df(pdata)
    baseplot = ggplot(pdata, aes(x=X, y=Z)) + 
      geom_point() +
      coord_cartesian(xlim = front_ranges$x, ylim = front_ranges$y, expand = FALSE)
    p = baseplot
    
    if(!is.null(special_point$data)) {
      p = p + geom_point(data = special_point$data, color = "red", size = 3) 
    }
    
    if(!is.null(input$color_select)) {
      if(input$color_select == 1) { #code
        p = p + geom_point(aes(x = X, y = Z, color = code))
        
      } else if(input$color_select == 2) { #unit
        p = p + geom_point(aes(x = X, y = Z, color = Unit))
        
      } else { #level
        p = p + geom_point(aes(x = X, y = Z, color = level))
      }
      if(!is.null(category_palette$colors)) {
        p = p + scale_color_manual(values = category_palette$colors)
      }
    }
    
    ###### plotting datums, units, multi-points ####
    if(!is.null(input$extra_plots)) { ##here can only plot datums
      if("1" %in% input$extra_plots) {
        datums = datums.df()
        p = p + geom_point(data = datums, size = 5, color = "blue")
      }
      if("3" %in% input$extra_plots){
        p = p + geom_line(aes(group = grp))
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
      data = db.df()
      front_ranges$x <- c(min(data$X) - 50, max(data$X) + 50)
      front_ranges$y <- c(min(data$Z) - 50, max(data$Z) + 50)
    }
  })
  
  #### side plot ####
  side.plot = reactiveVal()
  output$sideView <- renderPlot({
    read_database()
    
    validate(
      need(!is.null(db.df()), "No data has been read in.")
    )
    
    if(input$select_view == 2) {
      data = last.points()
    } else {
      data = db.df()
    }
    data = data %>% group_by(Unit, ID) %>% mutate(grp = cur_group_id())
    
    if(input$select_view == 3) {
      data = data[duplicated(data$grp) | duplicated(data$grp, fromLast = TRUE), ]
    }
    
    #####draw side view#####
    s.units = unique(data$Unit)
    if(!is.null(input$select_units)) {
      s.units = input$select_units
    }
    
    s.levels = unique(data$level)
    if(!is.null(input$select_levels)) {
      s.levels = input$select_levels
    }
    
    s.codes = unique(data$code)
    if(!is.null(input$select_code)) {
      s.codes = input$select_code
    }
    
    pdata = data %>% filter(Unit %in% s.units) %>%
      filter(level %in% s.levels) %>%
      filter(code %in% s.codes)
    plot.df(pdata)
    baseplot = ggplot(pdata, aes(x = Y, y = Z)) +
      geom_point() +
      coord_cartesian(xlim = side_ranges$x, ylim = side_ranges$y, expand = FALSE)
    p = baseplot
    
    if(!is.null(special_point$data)) {
      p = p + geom_point(data = special_point$data, color = "red", size = 3) 
    }
    
    if(!is.null(input$color_select)) {
      if(input$color_select == 1) { #code
        p = p + geom_point(aes(x = Y, y = Z, color = code))
      } else if(input$color_select == 2) { #unit
        p = p + geom_point(aes(x = Y, y = Z, color = Unit))
      } else { #level
        p = p + geom_point(aes(x = Y, y = Z, color = level))
      }
      if(!is.null(category_palette$colors)) {
        p = p + scale_color_manual(values = category_palette$colors)
      }
    }
    
    ###### plotting datums, units, multi-points ####
    if(!is.null(input$extra_plots)) { ##here can only plot datums
      if("1" %in% input$extra_plots) {
        datums = datums.df()
        p = p + geom_point(data = datums, size = 5, color = "blue")
      }
      if("3" %in% input$extra_plots){
        p = p + geom_line(aes(group = grp))
      }
    }
    
    side.plot(p)
    return(p)
  })
  
  ##### side zoom #####
  observeEvent(input$side_dblclick, {
    brush = input$side_brush
    if (!is.null(brush)) {
      side_ranges$x <- c(brush$xmin, brush$xmax)
      side_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      data = db.df()
      side_ranges$x <- c(min(data$Y) - 50, max(data$Y) + 50)
      side_ranges$y <- c(min(data$Z) - 50, max(data$Z) + 50)
    }
  })
  
  #### plan plot ####
  plan.plot = reactiveVal()
  output$planView <- renderPlot({
    read_database()
    
    validate(
      need(!is.null(db.df()), "No data has been read in.")
    )
    
    if(input$select_view == 2) {
      data = last.points()
    } else {
      data = db.df()
    }
    data = data %>% group_by(Unit, ID) %>% mutate(grp = cur_group_id())
    
    if(input$select_view == 3) {
      data = data[duplicated(data$grp) | duplicated(data$grp, fromLast = TRUE), ]
    }
    
    ##### draw plan view #####
    s.units = unique(data$Unit)
    if(!is.null(input$select_units)) {
      s.units = input$select_units
    }
    
    s.levels = unique(data$level)
    if(!is.null(input$select_levels)) {
      s.levels = input$select_levels
    }
    
    s.codes = unique(data$code)
    if(!is.null(input$select_code)) {
      s.codes = input$select_code
    }
    
    pdata = data %>% filter(Unit %in% s.units) %>%
      filter(level %in% s.levels) %>%
      filter(code %in% s.codes)
    plot.df(pdata)
    baseplot = ggplot(pdata) +
      geom_point(aes(x = X, y = Y)) +
      coord_cartesian(xlim = plan_ranges$x, ylim = plan_ranges$y, expand = FALSE)
    p = baseplot
    
    if(!is.null(special_point$data)) {
      p = p + geom_point(data = special_point$data, aes(x = X, y = Y), color = "red", size = 3) 
    }
    
    if(!is.null(input$color_select)) {
      if(input$color_select == 1) { #code
        p = p + geom_point(aes(x = X, y = Y, color = code))
      } else if(input$color_select == 2) { #unit
        p = p + geom_point(aes(x = X, y = Y, color = Unit))
      } else { #level
        p = p + geom_point(aes(x = X, y = Y, color = level))
      }
      if(!is.null(category_palette$colors)) {
        p = p + scale_color_manual(values = category_palette$colors)
      }
    }
    
    ###### plotting datums, units, multi-points ####
    if(!is.null(input$extra_plots)) {
      if("1" %in% input$extra_plots) {
        datums = datums.df()
        datums = datums %>% select(X, Y, Z) %>%
          mutate_all(unlist) %>%
          mutate_all(as.numeric)
        p = p + geom_point(data = datums, aes(x = X, y = Y), 
                           size = 5, color = "blue")
      }
      if("2" %in% input$extra_plots) {
        units = units.df()
        units = units %>% select(MINX, MAXX, MINY, MAXY) %>%
          mutate_all(unlist) %>%
          mutate_all(as.numeric)
        
        p = p + geom_rect(data = units,
                          mapping = aes(xmin = MINX, xmax = MAXX, ymin = MINY, ymax = MAXY),
                          color = "orange", alpha = 0)
      }
      if("3" %in% input$extra_plots){
        p = p + geom_line(aes(x = X, y = Y, group = grp))
      }
    }
    
    plan.plot(p)
    return(p)
  })
  
  ##### plan zoom ####
  observeEvent(input$plan_dblclick, {
    brush = input$plan_brush
    if (!is.null(brush)) {
      plan_ranges$x <- c(brush$xmin, brush$xmax)
      plan_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      data = db.df()
      plan_ranges$x <- c(min(data$X) - 50, max(data$X) + 50)
      plan_ranges$y <- c(min(data$Y) - 50, max(data$Y) + 50)
    }
  })
  
  #### data table for clicked point #####
  output$info = renderTable(striped = T, bordered = T, width = "100%", {
    
    validate(
      need(!is.null(plot.df()), "No data has been read in.")
    )
    
    data = plot.df()
    
    df = data %>%
      select(Unit, ID, Suffix, X, Y, Z, Prism, level, code, excavator)
    df = as.data.frame(df)
    
    nearPoints(df, input$plot_click, threshold = 10, maxpoints = 5, addDist = T)
  })
  
  #### Edit point from plot ####
  observeEvent(input$edit,  {
    data = plot.df()
    
    df = data %>%
      select(Unit, ID, Suffix, X, Y, Z, Prism, level, code, excavator)
    df = as.data.frame(df)
    
    point = nearPoints(df, input$plot_click, threshold = 10, maxpoints = 1, addDist = F)
    orig_unit(point$Unit)
    orig_id(point$ID)
    orig_suffix(point$Suffix)
    orig_prism(point$Prism)
    
    showModal(modalDialog(
      title = "Edit",
      #conditionalPanel("false", textInput("row_input", label = "row", value = point$ROW)),
      #textInput("row_input", label = "ROW", value = point$ROW),
      fluidRow(
        column(4, textInput("unit_input", label = "UNIT", value = point$Unit)),
        column(4, textInput("id_input", label = "ID", value = point$ID)),
        column(4, textInput("suffix_input", label = "SUFFIX", value = point$Suffix)),
      ),
      fluidRow(
        column(4, textInput("x_input", label = "X", value = point$X)),
        column(4, textInput("y_input", label = "Y", value = point$Y)),   
        column(4, textInput("z_input", label = "Z", value = point$Z)),
      ),
      fluidRow(
        column(12, textInput("prism_input", label = "PRISM", value = point$Prism)), 
      ), 
      fluidRow(
        column(12, textInput("level_input", label = "LEVEL", value = point$level)),
      ), 
      fluidRow(
        column(12, textInput("code_input", label = "CODE", value = point$code)),
      ), 
      fluidRow(
        column(12, textInput("excav_input", label = "EXCAVATOR", value = point$excavator)),
      ),
      actionButton("submit_edits", label = "Submit changes"),
      easyClose = TRUE,
      footer = NULL, 
      size = "m"
    ))
  })
  
  observeEvent(input$submit_edits, {
    removeModal()
    orig = db.df()
    
    datarow = orig[which(orig$Unit == orig_unit() & orig$ID == orig_id())[[1]],]
    
    datarow$Unit = input$unit_input
    datarow$ID = input$id_input
    datarow$Suffix = as.numeric(input$suffix_input)
    datarow$X = as.numeric(input$x_input)
    datarow$Y = as.numeric(input$y_input)
    datarow$Prism = as.numeric(input$prism_input)
    datarow$level = input$level_input
    datarow$code = input$code_input
    datarow$excavator = input$excav_input
    
    if(input$prism_input != orig_prism()) {
      shot_value = as.numeric(input$z_input) + orig_prism() #current stored Z + prism height
      new_Z = shot_value - as.numeric(input$prism_input)
    } else {
      new_Z = as.numeric(input$z_input)
    }
    datarow$Z = new_Z
    
    print(datarow)
    newdf = db.df()
    newdf[which(newdf$Unit == orig_unit() & newdf$ID == orig_id())[[1]],] = datarow
    db.df(newdf)
    
    write_database_edits(datarow)
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
      data = plot.df()
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