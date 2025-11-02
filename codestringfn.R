
## arraycode
## maskcode
## captcode
## fitcode
## getSPcode

getSPcode <- function (inputfilename, varname, comment = TRUE) {
  filename <- inputfilename[1,1]
  if (is.null(filename)) {
    return("")
  }
  else {
    ext <- tolower(tools::file_ext(filename))
    if (ext == "txt") {
      code <- paste0( 
        if (comment) "# coordinates from text file\n" else "",
        "coord <- read.table('", filename, "')   # read boundary coordinates\n",
        varname, " <- secr::boundarytoSF(coord)  # convert to sfc_POLYGON\n")
    }
    else if (ext %in% c("rdata", "rda")) {
      objlist <- load(inputfilename[1,4])
      code <- paste0( 
        if (comment) "# polygons from RData file\n" else "",
        "objlist <- load('", filename, "')\n",
        varname, " <- get(objlist[1]) \n")
    }
    else if (ext == "rds") {
      code <- paste0( 
        if (comment) "# polygons from RDS file\n" else "",
        varname, " <- readRDS('", filename, "') \n")
    }
    else {
      code <- paste0(
        if (comment) "# ESRI polygon shapefile\n" else "",
        varname, " <- sf::st_read(dsn = '", 
        tools::file_path_sans_ext(basename(filename)), 
        ".shp')\n"
      )
    }
    code
  }
}    
################################################################################

arraycode <- function (comment = FALSE) {
  # returns the R code needed to generate the specified array, 
  # as a character value
  code <- ""  
  sheet <- ""
  args <- input$trapotherargs
  if (args != "") {
    args <- paste0(", ", args)
  }
  if (input$datasource == "Text files") {
    filename <- input$trapfilename[,"name"]
  }
  else if (input$datasource == "Excel files") {
    filename <- input$trapxlsname[,"name"]
    if (length(filename)==1) {
      dataname <- input$trapxlsname[1,"datapath"]
      if (!input$trapsheet %in% readxl::excel_sheets(dataname)) return()
      if(!grepl("sheet", args)) {
        sheet <- paste0(", sheet = '", input$trapsheet, "'")
      }
    }
  }
  else {
    filename <- ""
  }
  
  if (nchar(filename[1])>0) {
    if (length(filename)>1) {
      cr <- ",\n   "
      filename <- paste0("c('", paste(filename, collapse = "','"), "')")
    }
    else {
      cr <- ", "
      filename <- paste0("'", filename, "'")
    }
    code <- paste0("array <- read.traps (",
                   filename,
                   cr, "detector = '", input$detector, "'", 
                   args, sheet, ")\n")
    cov <- covariates(traprv$data)
    if (!is.null(cov) && !ms(cov)) {     # defer hard case
      ncov <- if (ms(cov)) ncol(cov[[1]]) else ncol(cov)
      covnames <- getcovnames(input$trapcovnames, ncov, 'T', TRUE)
      covnamecode <- if (ms(cov)) ""   # defer hard case
      else paste0("names(covariates(array)) <- ", covnames, "\n")
      code <- paste0(code, covnamecode)
    }
    if (comment) {
      tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
      tmp$sep <- "\n"
      code <- do.call(paste, tmp)
    }
  }
  code        
}
################################################################################

maskcode <- function () {
  removeNotification("invalidinput")
  
  if (input$masktype == 'Build') {
    if (is.null(traprv$data))
      return("\n")
    else {
      type <- if (input$maskshapebtn == 'Rectangular') 'traprect' else 'trapbuffer'
      buffer <- as.character(round(input$buffer,2))
      polycode <- ""
      polyhabitat <- ""
      sourcecode <- ""
      addcovcode <- ""
      dropcode <- ""
      
      if (!is.null(input$maskpolyfilename) && 
          input$maskpolybtn %in% c("File(s)", "R object")) { 
        polyhabitat <- input$includeexcludebtn == "Include"
        if (input$maskpolybtn == "File(s)")
          polycode <- getSPcode(input$maskpolyfilename, "poly")
        else
          polycode <- maskpolyobjectname    
      }
      if (!is.null(input$maskcovariatefilename) && input$maskcovariatebtn == "File(s)") { 
        ext <- tolower(tools::file_ext(input$maskcovariatefilename[1,1]))
        if (ext == "txt") {
          sourcecode <- paste0(
            "covariatesource <- read.mask('", 
            input$maskcovariatefilename[1,1], 
            "')\n"
          )
        }
        else {
          sourcecode <- getSPcode(input$maskcovariatefilename, 
                                  "covariatesource", comment = FALSE)
        }
        addcovcode <- "mask <- addCovariates(mask, covariatesource)\n"
        if (input$dropmissing) {
          dropcode <- paste0(
            "covariatesOK <- apply(!is.na(covariates(mask)), 1, all)\n",
            "mask <- subset(mask, covariatesOK)\n"
          )
        }
      }
      trps <- if (is.null(importrv$data) && is.null(secrrv$data)) "array" else "traps(ch)"
      paste0(
        polycode,
        "mask <- make.mask (", trps,  
        ", buffer = ", buffer, 
        if (input$meshdimensionbtn == "Spacing") 
          paste0(", spacing = ", input$habspacing)
        else paste0(", nx = ", input$habnx), 
        ", type = '", type, "'",  
        if (polycode == "") "" else ",\n    poly = poly",
        if (polycode == "") "" else ", poly.habitat = ", polyhabitat,
        ")\n",
        sourcecode,
        addcovcode,
        dropcode)
    }
  }
  else {
    if (!is.null(input$maskfilename))
      paste0("mask <- read.mask ('", input$maskfilename[1,1], "', header = TRUE)\n")
  }
}
##############################################################################

captcode <- function (comment = FALSE) {
  # returns the R code needed to generate the specified array, 
  # as a character value
  code <- ""  
  sheet <- ""
  if (!is.null(secrrv$data)) {
    code <- paste0("ch <- ", input$secrdatabox, "\n")
  }
  else if (!is.null(importrv$data)) {
    code <- paste0("ch <- readRDS('", input$importfilename[1,"name"], "')\n")
  }
  else {
    if (!is.null(captrv$data)) {
      args <- input$captotherargs
      
      if (args != "") {
        args <- paste0(", ", args)
      }
      
      if (input$fmt=='XY') {
        fmt <- ", fmt = 'XY'"
        datacols <- 5
      }
      else {
        fmt <- ""
        datacols <- 4
      }
      
      ncov <- ncol(captrv$data) - datacols
      
      if (ncov==0) {
        cov <- NULL
      }
      else {
        covnames <- getcovnames(input$covnames, ncov, 'C', TRUE)
        cov <- if (covnames == "") "" else paste0(", covnames = ", covnames)
      }
      
      if (input$datasource == 'Text files') {
        filename <- input$captfilename[1,"name"]
        dataname <- input$captfilename[1,"datapath"]
        nfield <- count.fields(dataname)[1]
        mincol <- (input$fmt == "XY") + 4
        # ensure correct type for trapID
        if (nfield>=mincol && input$fmt == "trapID" && ! grepl("colClasses", args)) {
          colClass <- c("'character'", "'character'","'integer'","'character'", rep(NA, nfield-4))
          args <- paste0(args, ", colClasses = c(", paste0(colClass, collapse = ","), ")")
        }
        code <- paste0("capt <- read.table('", filename, "'", args, ")\n")
      }
      else {
        filename <- input$captxlsname[1,"name"]
        dataname <- input$captxlsname[1,"datapath"]
        if (!input$captsheet %in% readxl::excel_sheets(dataname)) return()
        
        if(!grepl("sheet", args)) {
          sheet <- paste0(", sheet = '", input$captsheet, "'")
        }
        code <- paste0("capt <- readxl::read_excel('", filename, "'", args, sheet, ")\n")
        code <- paste0(code, "capt <- data.frame(capt)\n")
      }
      
      code <- paste0(code, "ch <- make.capthist (capt, traps = array", fmt, cov, ")\n")
      
      if (filtercaptrv$value && !input$filtercapttext=="") {
        subset <- input$filtercapttext
        numsubset <- as.numeric(subset)
        if (all(!is.na(numsubset))) {
          if (all(numsubset<0)) {
            subset <- paste0("!(1:nrow(ch)) %in% c(", paste0(-numsubset, collapse = ","), ")")
          }
        }
        code <- paste0(code, "ch <- subset(capthist = ch, ", subset, ")\n")
      }
      
      if (comment) {
        tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
        tmp$sep <- "\n"
        code <- do.call(paste, tmp)
      }
    }
  }
  code
}

################################################################################

fitcode <- function() {
  detfn <- input$detectfnbox
  if (is.character(detfn)) detfn <- paste0("'", detfn, "'")
  CL <- if (input$likelihoodbtn == "Full") "" else ", CL = TRUE"
  hcov <- if(input$hcovbox == "none") "" else paste0(", hcov = '", input$hcovbox, "'")
  model <- paste0("model = list(", input$model, ")")
  distn <- if (input$distributionbtn == "Poisson") "" else
    ",\n      details = list(distribution = 'binomial')"
  method <- if (input$method == "Newton-Raphson") "" else
    paste0(",\n      method = '", input$method, "'")
  nc <- input$ncores
  ncores <- paste0(", ncores = ", nc)
  modelotherargs <- if (input$modelotherargs=="") "" else paste0(",\n      ", input$modelotherargs)
  code <- paste0(
    "fitted <- secr.fit(ch, mask = mask, detectfn = ", detfn, CL, hcov, ", \n",
    "      ", model, ", trace = FALSE", distn, method, ncores, modelotherargs, ")\n"
  )
  code
}
##############################################################################

fxicode <- function(add = FALSE) {
  i <- input$animal
  sessnum <- input$sess
  border <- input$buffer
  if (ms(capthist()))
    code <- paste0("plot(ch[[", sessnum, "]]); ")  
  else
    code <- paste0("plot(ch); ")  
  
  code <- paste0(code,
                 "fxiContour(fitted",
                 ", i = ", i, 
                 ", sessnum = ", sessnum, 
                 ", border = ", border, 
                 ", add = TRUE)\n") 
  code
}
##############################################################################
