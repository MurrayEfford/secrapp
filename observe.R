## read import file
observe({
  req(input$importfilename)
  removeNotification("invalidinput")
  req(!importrv$clear)
  ch <- readRDS(input$importfilename[1,"datapath"])
  if (inherits(ch, 'capthist')) {
    importrv$data <- ch
    traprv$data <- traps(ch)
    if (detector(traprv$data)[1] %in% polygondetectors) {
      updateSelectInput(session, "detectfnbox", choices = hazarddetectfn, 
                        selected = "HHN")
      disable('suggestbufferlink')
    }
    else {
      if (ms(ch)) {
        updateNumericInput(session, "sess", max = length(ch))
        updateNumericInput(session, "masksess", max = length(ch))
        updateNumericInput(session, "rad", value = signif(spacing(traprv$data[[1]])/5, 2))
      }
      else {
        updateNumericInput(session, "rad", value = signif(spacing(traprv$data)/5, 2))
      }
      updateSelectInput(session, "detectfnbox", choices = c('HN','HR','EX', hazarddetectfn), 
                        selected = "HN")
      enable('suggestbufferlink')
    }
    removeNotification("invalidinput")
    showNotification("capthist imported", 
                     type = "message", id = "lastaction", duration = seconds)
  }
  else {
    importrv$data <- NULL
    traprv$data <- NULL
    showNotification("not a valid capthist RDS file", 
                     type = "error", id = "invalidinput", duration = invalidseconds)
  }
})

##############################################################################
## read capture file
observe({
  req(!captrv$clear)
  removeNotification("invalidinput")
  sheet <- ""
  if (input$datasource == 'Text files') {
    req(input$captfilename)
    # temporary fix for bad shiny bookmark
    if (bookmarkrv$value) {
      captdataname <- bookmarkrv$captfilename
    }
    else {
      captdataname <- input$captfilename[1,"datapath"]
    }
    captfilename <- input$captfilename[1,"name"]
  }
  else {
    req(input$captxlsname)
    # temporary fix for bad shiny bookmark
    if (bookmarkrv$value) {
      captdataname <- bookmarkrv$captlsname
    }
    else {
      captdataname <- input$captxlsname[1,"datapath"]
    }
    
    captfilename <- input$captxlsname[1,"name"]
    if (!input$captsheet %in% readxl::excel_sheets(captdataname)) return()
    
    samexls <- captfilename == input$trapxlsname[1,"name"]
    if (samexls && input$captsheet %in% input$trapsheet) {
      showNotification("cannot use same xls sheet",
                       id = "invalidinput", type = "error", duration = invalidseconds)
      captrv$data <- NULL
      return()
    }
    
    if (!grepl('sheet', input$captotherargs)) {
      sheet <- paste0(", sheet = '", input$captsheet, "'")
    }
  }
  args <- input$captotherargs
  mincol <- (input$fmt == "XY") + 4
  tempargs <- try(eval(parse(text = paste0("list(", args, ")"))), silent = TRUE)
  if (inherits(tempargs, "try-error")) {
    showNotification("arguments incomplete or invalid",
                     id = "invalidinput", type = "error", duration = invalidseconds)
  }
  else {
    removeNotification(id = "invalidinput")
    if (args != "")
      args <- paste0(", ", args)
    if (grepl('.xls', captdataname)) {
      readcaptcall <- paste0("readxl::read_excel(captdataname", args, sheet, ")")
    }
    else {
      # ensure correct type for trapID
      nfield <- count.fields(captdataname)[1]
      if (nfield >= mincol && input$fmt == "trapID" && ! grepl("colClasses", args)) {
        colClass <- c("character", "character","integer","character", rep(NA, nfield-4))
        args <- paste0(args, ", colClasses = colClass")
      }
      readcaptcall <- paste0("read.table (captdataname", args, ")")
    }
    captrv$data <- try(eval(parse(text = readcaptcall)))
    captrv$data <- as.data.frame(captrv$data)
    captrv$clear <- TRUE
    if (!inherits(captrv$data, "data.frame") || ncol(captrv$data) < mincol) {
      showNotification("invalid capture file or arguments; try again",
                       id = "invalidinput", type = "error", duration = invalidseconds)
      captrv$data <- NULL
    }
    else {
      showNotification("capthist loaded",
                       id = "lastaction", type = "message", duration = seconds)
    }
  }
  
})
##############################################################################

## read mask polygon file or object
observe({
  req(input$maskpolybtn)
  req(!polyrv$clear)
  removeNotification("invalidinput")
  if (input$maskpolybtn == "None") {
    polyrv$data <- NULL
  }
  else if (input$maskpolybtn == "File(s)") {
    polyrv$data <- readpolygon(input$maskpolyfilename)
  }
  else if (input$maskpolybtn == "R object") {
    if (!exists(input$maskpolyobjectname)) {
      polyrv$data <- NULL
    }
    else {
      polyrv$data <- get(input$maskpolyobjectname)
    }
  }
  if (!is.null(polyrv$data)) {
    if (!inherits(polyrv$data, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
      showNotification("invalid polygon file or object; try again",
                       id = "invalidinput", type = "error", duration = invalidseconds)
      polyrv$data <- NULL
    }
    removeNotification("invalidinput")
  }
})
##############################################################################

## read mask covariate source file
observe({
  req(input$maskcovariatefilename)
  req(!covariaterv$clear)
  ext <- tolower(tools::file_ext(input$maskcovariatefilename[1,1]))
  if (ext == "txt") {
    covariaterv$data <- read.mask(input$maskcovariatefilename[1,4], 
                                  header = TRUE, stringsAsFactors = TRUE)
  }
  else if (ext == "rds") {
    covariaterv$data <- readRDS(input$maskcovariatefilename[1,4])
  }
  else {
    # shapefile
    covariaterv$data <- readshapefile(input$maskcovariatefilename)
  }
  
  if (!inherits(covariaterv$data, c("SpatialPolygonsDataFrame", "sf",  "mask"))) {
    showNotification("invalid covariate file; try again",
                     id = "invalidinput", type = "error", duration = invalidseconds)
    covariaterv$data <- NULL
    covariaterv$names <- character(0)
    reset("maskcovariatefilename")
  }
  else {
    if (inherits(covariaterv$data, "sf")) {
      covariaterv$names <- names(sf::st_drop_geometry(covariaterv$data))
    }
    else if (inherits(covariaterv$data, "mask")) {
      covariaterv$names <- names(covariates(covariaterv$data))
    }
    else {
      covariaterv$names <- names(covariaterv$data)
    }
  }
  updateSelectInput(session, "maskcov",
                    choices = c("none", covariaterv$names),
                    selected = "none")
})
##############################################################################
## read mask file
observe({
  req(input$maskfilename)
  req(!maskrv$clear)
  maskrv$data <- NULL
  if (input$masktype == 'File') {
    covariaterv$names <- character(0)
    if (!is.null(input$maskfilename)) {
      # temporary fix for shiny problem with multiple files to upload
      if (bookmarkrv$value) {
        maskrv$data <- read.mask(bookmarkrv$maskfilename, header = TRUE, 
                                 stringsAsFactors = TRUE) 
      }
      else {
        maskrv$data <- read.mask(input$maskfilename[1,4], header = TRUE, 
                                 stringsAsFactors = TRUE) 
      }
      covariaterv$names <- names(covariates(maskrv$data))
      updateSelectInput(session, "maskcov", 
                        choices = c("none", covariaterv$names))
    }
  }
})

##############################################################################
