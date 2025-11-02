## renderText

# maskPrint
# resultsPrint 
# codePrint 
# movesPrint

##############################################################################

output$maskPrint <- renderPrint({
  if (is.null(mask())) {
    if (input$masktype == 'Build') 
      cat("No mask yet - load trap file on main screen\n")
    else 
      cat("No mask yet - load mask file\n")
  }
  else {
    summary(mask())
  }
})
##############################################################################

output$maskdetailPrint <- renderPrint({
  if (input$masktype == 'Build') {
    if (input$meshdimensionbtn == "Spacing")
      x1 <- paste0("Buffer ", input$buffer, " m, spacing ", signif(input$habspacing,3), " m")
    else
      x1 <- paste0("Buffer ", input$buffer, " m,  nx ", input$habnx)
  }
  else
    x1 <- paste0("Habitat mask from file")
  
  if (is.null(mask())) { 
    if (input$masktype == 'File')
      x2 <- "[yet to load]"
    else
      x2 <- ""
  }
  else {
    if (ms(mask()))
      x2 <- paste0("S1: ", nrow(mask()[[1]]), " points")
    else
      x2 <- paste0(nrow(mask()), " points")
    if (input$meshdimensionbtn == "Spacing" && input$masktype == 'Build') {
      # spacing already displayed      
    }
    else {
      x2 <- paste0(x2, 
                   ", spacing ",
                   if (ms(mask())) signif(spacing(mask()[[1]]),3) 
                   else signif(spacing(mask()), 3),
                   " m")
    }
    
  }
  
  cat(x1, "\n")
  cat(x2, "\n")
  
})
#-----------------------------------------------------------------------------

output$animalIDPrint <- renderPrint({
  if(is.na(input$animal) || input$animal<=0 || is.null(nsessions()))
    cat("")
  else {
    if (nsessions()>1) {
      ch <- capthist()[[input$sess]]
    }    
    else {
      ch <- capthist()
    }
    ch <- subset(ch, input$animal, dropnullocc = TRUE)
    
    covar <- covariates(ch) 
    if (length(covar)>0)
      covar <- paste(lapply(1:length(covar), function(i) 
        paste(names(covar)[i], as.character(covar[[i]]))), collapse = ', ')
    ID <- paste0("ID ", currentIDrv$value)
    ncapt <- sum(ch)
    nDet <- paste0("\n", ncapt, " detection", if (ncapt>1) "s" else "") 
    ORL <- paste0("\nORL ", signif(ORL(ch)$ORL, 3), " m")
    cat(paste(ID, covar, nDet, ORL, sep = " "))
  }
})
#-----------------------------------------------------------------------------

otherfn <- function (fncall, ch, fitted) {
  if (fncall=="") {
    cat("No function specified\n")
  }
  else if (substring(fncall,1,1) == "?") {
    ff <- get('index.search', envir = asNamespace('utils'))(
      topic = substring(fncall,2,100), 
      paths = find.package('secr'),
      TRUE)
    path    <- dirname(ff)
    dirpath <- dirname(path)
    pkgname <- basename(dirpath)
    RdDB    <- file.path(path, 'secr')
    fetchRdDB <- get('fetchRdDB', envir = asNamespace('tools'))
    # no need for warnings from incomplete topic names
    Rdfile <- tryCatch(
      expr = suppressWarnings(fetchRdDB(RdDB, basename(ff))), 
      error = function(e) return(NULL),
      silent = TRUE)
    if (!is.null(Rdfile)) {
        tools::Rd2txt(Rd = Rdfile, options = list(underline_titles = FALSE))
    }
  }
  else {
    # fails with AIC
    # if (fitted) fncall <- gsub("fitted", "fitrv$value", fncall)
    if (fitted) fitted <- fitrv$value  ## 2022-02-10
    if (ch) fncall <- gsub("ch", "capthist()", fncall)
    tr <- traprv$data
    out <- tryCatch(
      expr = eval(parse(text = fncall)), 
      error = function(e) e,
      silent = TRUE)
    if (inherits(out, "simpleError")) {
      cat("Did not compute\n")
      out$message
    }
    else {
      out
    }
  }  
}
#-----------------------------------------------------------------------------

output$resultsPrint <- renderPrint({
  rse <- RSE() 
  if (is.null(rse) || is.na(rse)) {
    maxRSE <- 100
    rse <- maxRSE
  }
  else {
    if (rse<10) maxRSE <- 20
    else if (rse<20) maxRSE <- 30
    else if (rse<30) maxRSE <- 40
    else if (rse<40) maxRSE <- 50
    else maxRSE <- 100
  }
  updateSliderInput(session, "RSEslider",
                    min = 1.0,
                    max = maxRSE,
                    value = rse,
                    step = 0.1)
  
  minchar <- 50
  if (traptextrv$value) {
    header <- paste("Detector layout file :", input$trapfilename[1,'name'])
  }
  else if (capttextrv$value) {
    header <- paste("Capture file :", input$captfilename[1,'name'])
  }
  else if (is.null(traprv$data)) {
    header <- "No data loaded"
  }
  else if (input$resultsbtn == "other") {
    header <- input$otherfunction
  }
  else if (is.null(capthist())) {
    if (input$datasource == 'Text files') {
      header <- paste("Summary of detector layout from", input$trapfilename[1,'name'])
      if (nrow(input$trapfilename)>1) {
        header <- paste0(header, " etc.")
      }
    }
    else if (input$datasource == 'Excel files') {
      header <- paste("Summary of detector layout from", input$trapxlsname[1,'name'])
      if (nrow(input$trapxlsname)>1) {
        header <- paste0(header, " etc.")
      }
    }
  }
  else if (is.null(fitrv$value)) {
    if (input$datasource == 'Text files') {
      header <- paste0("Summary of capthist object from ", input$trapfilename[1,'name'], 
                       if (nrow(input$trapfilename)>1) " etc." else "",
                       " and ", 
                       input$captfilename[1,'name'])
    }
    else if (input$datasource == 'Excel files') {
      header <- paste("Summary of capthist object from", input$trapxlsname[1,'name'])
      if (input$captxlsname[1,'name'] != input$trapxlsname[1,'name']) {
        header <- paste(header, "and", input$captxlsname[1,'name'])
      }
    }
    else if (input$datasource == 'Stored capthist object') {
      header <- paste("Summary of capthist object from", input$importfilename[1,'name'])
    }
    else if (input$datasource == 'secr dataset') {
      header <- paste("Summary of secr data object", input$secrdatabox)
    }
    if (filtercaptrv$value) {
      header <- paste0(header, ", ", input$filtercapttext)
    }
  }
  else if (inherits(fitrv$value, "secr")) {
    if (input$resultsbtn=='summary') {
      minchar <- nchar(summary(fitrv$value)$versiontime)+6
      header <- paste("Summary of fitted model", input$model)
    }
    else if (input$resultsbtn=='predict') {
      minchar <- 48
      header <- paste("Parameter estimates")
    }
    else if (input$resultsbtn=='derived')
      header <- paste("Derived estimates of effective sampling area 'esa' and density 'D'")
    else header <- ""
  }
  #--------------------------------------------------
  if (nchar(header)>0) {
    cat(header, "\n")
    if (substring(header,1,2)!="No") cat(strrep('-', max(minchar,nchar(header))), "\n")
  }    
  #--------------------------------------------------
  
  if (traptextrv$value) {
    rawText <- readLines(input$trapfilename[1,"datapath"]) # get raw text
    writeLines(rawText)
  }
  else if (capttextrv$value) {
    rawText <- readLines(input$captfilename[1,"datapath"]) # get raw text
    writeLines(rawText)
  }
  else if (is.null(traprv$data)) {
    cat("")
  }
  else if (is.null(capthist())) {
    if (input$resultsbtn == "summary")
      summary(traprv$data, covariates = TRUE)   ## arg covariates new in secr 4.3.1
    else
      otherfn(input$otherfunction, FALSE, FALSE)
  }
  else if (is.null(fitrv$value)) {
    if (input$resultsbtn == "summary") {
      if (ms(capthist())) {
        list(terse = summary(capthist(), terse = TRUE), bysession = summary(capthist(), moves = TRUE))            }
      else {
        summary(capthist(), moves = TRUE)
      }
    }
    else {
      otherfn(input$otherfunction, TRUE, FALSE)
    }
  }
  else if (inherits(fitrv$value, "secr")) {
    
    if (input$resultsbtn == "summary")
      summary(fitrv$value)
    else if (input$resultsbtn == "predict") {
      predictresult()
    }
    else if (input$resultsbtn == "derived") {
      derivedresult()
    }
    else {
      otherfn(input$otherfunction, TRUE, TRUE)
    }
  }
  else {
    fitrv$value
  }
})
##############################################################################

output$codePrint <- renderPrint({
  cat("library(secr)\n")
  if (!is.null(traprv$data)) {
    if (input$datasource == 'secr dataset')
      cat("\n# use built-in secr dataset\n")
    else 
      cat("\n# input data\n")
    cat(arraycode())
  }
  if (!is.null(captrv$data) || !is.null(importrv$data) || !is.null(secrrv$data)) {
    cat(captcode())
  }
  
  if (!is.null(capthist())) {
    cat("summary(ch)\n")
    cat("\n# habitat mask\n")
    cat(maskcode())
    cat("\n# fit model\n")
    cat(fitcode())
    cat("summary(fitted)\n")
  }
})
##############################################################################

output$movesPrint <- renderPrint({
  m <- movements()
  if (!is.null(m)) {
    maxm <- max(unlist(m))
    maxbyanimal <- suppressWarnings(sapply(m, max))
    nmax <- match(maxm, maxbyanimal)
    IDmax <- names(m)[nmax] 
    rpsv <- unlist(RPSV(capthist(), CC = TRUE))[input$sess]
    cat("Number of moves", length(unlist(m)), "\n")
    cat("Median move", round(median(unlist(m)),1), "m\n")
    cat("Naive HN sigma", round(rpsv,1), "m\n")
    cat("Longest move", round(maxm,1), "m\n")
    cat("by animal ID", IDmax, "\n")
  }
})
##############################################################################

# Render the log content
# 2025-10-30
output$logPrint <- renderPrint({
    if (length(log_data$messages) > 0) {
      cat(paste(log_data$messages, collapse = ""))
    } else {
      cat("No calls, messages, warnings, or errors captured.")
    }
})
##############################################################################
