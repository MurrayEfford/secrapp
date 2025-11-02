addtosummary <- function() {
  ## input$fields is character vector of selected fields
  OK <- FALSE
  df <- data.frame(
    date = format(Sys.time(), "%Y-%m-%d"),
    time = format(Sys.time(), "%H:%M:%S"),
    note = input$title,
    
    traps = if (input$datasource=="Text files") 
      paste(input$trapfilename$name[1], if (nrow(input$trapfilename)>1) "etc." else "")
    else if (input$datasource=="Excel files") 
      paste(input$trapxlsname$name[1], if (nrow(input$trapxlsname)>1) "etc." else "")
    else "",
    captures = if (input$datasource=="Text files") input$captfilename$name[1]
    else if (input$datasource=="Excel files") input$captxlsname$name[1] 
    else if (input$datasource=="Stored capthist object") input$importfilename$name[1]
    else if (is.null(input$secrdatabox)) "" else input$secrdatabox,
    filter = if (filtercaptrv$value && input$filtercapttext!="") input$filtercapttext else "",
    ndetectors = ndetectors()[input$sess],
    noccasions = noccasions()[input$sess],
    usagepct = usagepct()[input$sess],
    maskbuffer = if (input$masktype=='Build') input$buffer else input$maskfilename$name[1],
    masknrow = masknrow()[input$sess],
    maskspace = round(maskspace()[input$sess], 1),
    n = n(),
    r = r(),
    likelihood = input$likelihoodbtn,
    distribution = input$distributionbtn,
    model =  modelstring(), # input$model,
    detectfn = input$detectfnbox,
    hcov = "",
    npar = NA_real_,
    logLik = NA_real_,
    AIC = NA_real_,
    dAIC = 0,
    D = NA_real_, 
    se.D = NA_real_, 
    RSE.D = NA_real_,
    g0 = NA_real_, 
    se.g0 = NA_real_,
    lambda0 = NA_real_,
    se.lambda0 = NA_real_,
    sigma = NA_real_, 
    se.sigma = NA_real_,
    z = NA_real_, 
    se.z = NA_real_,
    k = NA_real_,
    proctime = NA_real_,
    method = ""
  )
  
  if (inherits(fitrv$value, "secr")) {
    fitsum <- summary(fitrv$value)
    df$hcov <- fitsum$modeldetails[['hcov']]
    df$npar <- fitsum$AICtable$npar
    df$logLik <- fitsum$AICtable$logLik
    df$AIC <- fitsum$AICtable$AIC
    df$dAIC <- 0
    df$D <- density()
    df$se.D <- se.density()
    df$RSE.D <- se.density() / density()
    df$g0 <- ifelse (detectrv$value=="g0", detect0(), NA_real_)
    df$se.g0 <- ifelse (detectrv$value=="g0", se.detect0(), NA_real_)
    df$lambda0 <- ifelse (detectrv$value=="lambda0", detect0(), NA_real_)
    df$se.lambda0 <-ifelse (detectrv$value=="lambda0", se.detect0(), NA_real_)
    df$sigma <- sigma()
    df$se.sigma <- se.sigma()
    df$z <- zw()
    df$se.z <- se.zw()
    if (input$detectfnbox %in% c("HN", "HHN"))
      df$k <- density()^0.5 * sigma() / 100
    else 
      df$k <- NA_real_ # force numeric NA
    df$proctime <- fitrv$value$proctime
    if (!is.null(fitrv$value$method)) df$method <- fitrv$value$method
    df[,21:22] <- round(df[,21:22], 2)   # logLik, AIC
    df[,23:35] <- signif(df[,23:35], input$dec)
    OK <- TRUE
  }
  sumrv$value <- rbind (sumrv$value, df)
  
  if (nrow(sumrv$value)>0) {
    rownames(sumrv$value) <- paste0(input$summaryprefix, 1:nrow(sumrv$value))
    
    updateCheckboxGroupInput(session, "analyses", 
                             choices = rownames(sumrv$value),
                             selected = c(input$analyses, paste0(input$summaryprefix, nrow(sumrv$value)))
    )
    output$summaries <- renderText("true")
  }
  return(OK)
}
##############################################################################



modellist <- function() {
  f <- paste0("list(", input$model, ")")
  f <- tryCatch(parse(text = f), error = function(e) NULL)
  eval(f)
}

fitmodel <- function(LLonly = FALSE) {
  removeNotification(id = "error")
  if (!LLonly) {
    progress <- Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Fitting...', detail = '')
  }
  CL <- input$likelihoodbtn != "Full"
  
  model <- modellist()
  
  modelotherargs <- tryCatch(
    expr = eval(parse(text = paste0("list(", input$modelotherargs, ")"))), 
    error = function(e) return(NULL),
    silent = TRUE)
  if (is.null(modelotherargs) && !LLonly) {
    showNotification("model fit failed - check other arguments",
                     id = "error", type = "error", duration = errorseconds)
    fit <- NULL
  }
  else {
    modelotherargs <- modelotherargs[!(names(modelotherargs) %in% c('capthist','trace','mask','model','detectfn'))]
    # if (grepl("fitted", input$modelotherargs)) fitted <- fitrv$value
    nc <- input$ncores
    args <- c(list(capthist = capthist(), 
                   trace = FALSE,
                   mask = mask(), 
                   model = model,
                   detectfn = input$detectfnbox,
                   method = input$method,
                   ncores = nc),
              modelotherargs)
    args$CL <- CL 
    args$details <- as.list(replace (args$details, "distribution", input$distributionbtn))
    if (input$hcovbox != "none") {
      args$hcov <- input$hcovbox
    }
    if (LLonly) {
      args$details <- as.list(replace (args$details, "LLonly", TRUE))
      args$biaslimit <- NA
      # suppress warning "multi-catch likelihood used for single-catch traps"
      # force garbage collection for greater accuracy of timing
      gc(verbose = FALSE)
      isolate(fit <- suppressWarnings(do.call("secr.fit", args)))
    }
    else {
      fit <- 
        log_and_run(
          expr = do.call("secr.fit", args), 
          fitcode()
        )
    }
    if (is.null(fit) && !LLonly) {
        showNotification("model fit failed - check data, formulae and mask",
                       type = "error", id = "error", duration = errorseconds)
    }
  }
  if (LLonly) {
    # if (is.null(fit)) 
    #   stop("model failed to fit")
    # else
      return(fit)
  }
  else {
    updateRadioButtons(session, "resultsbtn", label = "", 
                       inline = TRUE, choices = fittedresultsbtn)
    updateTextInput(session, "otherfunction", placeholder = "e.g., vcov(fitted)")
    
    fitrv$value <- fit
    if (length(fit)>0) {
      fitrv$dsurf <- predictDsurface(fit)
    }
    else {
      fitrv$dsurf <- NULL
    }
    
    if (is.null(fit) || fit$fit$minimum == 1e+10) {
      showNotification("Model failed to fit",
                       type = "error", id = "error", duration = errorseconds)
    }
    else {
      OK <- tryCatch(
        expr = addtosummary(),
        error = function(e) return(NULL)  
      )
      
      if (is.null(OK)) {
        showNotification("Problem adding results to summary",
                         type = "warning", id = "warning", duration = warningseconds)
      }
      else {
        showNotification("Model fitted", 
                         type = "message", id = "lastaction", duration = seconds)
        
        if (input$masktype == "Build") {
          x <- log_and_run(
            expr = secr::bufferBiasCheck(
              object = fit, 
              buffer = round(input$buffer,2), 
              biasLimit = 0.01
            ),
            ""
          )
          if (!is.null(x) && !x) {
            showNotification("predicted buffer bias > 1%", 
                             type = "warning", id = "warning", duration = warningseconds)
          }
        }
        
        if (!is.null(fit$fit$hessian) && !is.null(fit$beta.vcv)) {
          svtol <- 1e-5
          eigH <- eigen(fit$fit$hessian)$values
          eigH <- abs(eigH)/max(abs(eigH))   
          
          eig <- round(eigH, -log10(svtol))
          rankH <- length(which(eigH > svtol))
          nbeta <- nrow(fit$beta.vcv)
          if (rankH < nbeta) {
            showNotification("at least one beta parameter is not identifiable (svtol=1e-5)",
                             type = "warning", id = 'warning', duration = warningseconds)
          }
        }
      }
    }
    
  }
  if (is.null(fitrv$value)) {
    updateRadioButtons (session, "resultsbtn", label = "", 
                        inline = TRUE, choices = defaultresultsbtn)
    disable("refitbtn")
  }
  else {
    enable("refitbtn")
  }
}
##############################################################################

# 2024-11-23
# may later add to 'secr'

refit <- function (method = NULL, trace = FALSE, ncores = NULL) {
  # omit start, dframe, verify, biasLimit, trace, ncores
  object <- fitrv$value
  if (!is.null(fitrv$value) && inherits(object, 'secr')) {
    argnames <- c("capthist", "model", "mask", "CL", "detectfn", 
                  "link", "fixed", "timecov", "sessioncov", "hcov", "groups",
                  "details", "method")
    args <- object[argnames]
    args$start  <- object
    args$trace  <- trace
    args$ncores <- ncores
    args$binomN <- object$details$binomN  
    if (is.null(object$method)) {
      # not saved in secr object until 5.1.1
      object$method <- "Newton-Raphson"
    }
    if (!is.null(method)) args$method <- method
    fitrv$value <- log_and_run(do.call(secr.fit, args), "refit")
    OK <- tryCatch(
      expr = addtosummary(),
      error = function(e) return(NULL))
    if (is.null(OK)) {
        showNotification("Problem adding refit to summary",
                       type = "warning", id = "warning", duration = warningseconds)
    }
    else {
      showNotification("Model re-fitted", 
                       type = "message", id = "lastaction", duration = seconds)
    }
    
  }
}
