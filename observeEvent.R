## observeEvent

# navlist

# trapfilename,  trapxlsname, trapsheet
# captfilename, captxlsname, captsheet
# importfilename
# maskpolyfilename
# maskcovariatefilename
# maskfilename

# alpha
# areaunit
# CIclick
# clearimportlink, datasource
# selectallanalyseslink
# selectnoanalyseslink
# detector
# detectfnbox
# distributionbtn
# fitbtn

# incrementtime
# showtrapfilelink

# mainlink
# mainlink2
# mainlink3
# optionslink
# masklink
# helplink
# clearspatialdata
# clearpolygondata
# clearbufferspec

# model
# okbtn
# modelotherargs
# pxyclick
# Dclick
# randompopbtn
# resetbtn
# selectallfieldslink
# selectfieldsbtn
# selectanalysesbtn
# selectnofieldslink
# suggestbufferlink

## Invalidate results when model specification changes
# likelihoodbtn
# detectfnbox
# distributionbtn
# model
# otherarg

# detectfnbox, likelihoodbtn, distributionbtn, hcovbox, model, modelotherargs,
#     masktype, buffer, habnx, maskshapebtn, maskpolyfilename, maskfilename,
#     filtercaptlink, filtercapttext
# masktype
# arrayClick
# animal
# sess
# trapxlsname
# captxlsname

##############################################################################

observeEvent(input$navlist, {
  if (input$navlist != "Introduction") {
    removeNotification("lastaction")
  }
  if (input$navlist == "Main screen") {
    if (is.null(traprv$data)) {
      showNotification("waiting for input", id = "lastaction", 
                       closeButton = FALSE, type = "message", duration = NULL)
    }
  }
  else if (input$navlist == "Habitat mask") {
    if (is.null(traprv$data))
      showNotification("waiting for detector layout on Main screen", id = "lastaction", 
                       closeButton = FALSE, type = "message", duration = NULL)
  }
  else if (input$navlist == "Summary") {
    if (is.null(sumrv$value) || nrow(sumrv$value)==0)
      showNotification("no model has been fitted", id = "lastaction", 
                       closeButton = FALSE, type = "message", duration = NULL)
  }
})
##############################################################################

observeEvent(c(input$trapfilename,  input$trapxlsname, input$trapsheet), {
  traprv$clear <- FALSE
  updateNumericInput(session, "animal", value = 1)
  updateRadioButtons(session, "resultsbtn", label = "", 
                     inline = TRUE, choices = defaultresultsbtn)
}, priority = 1000)

observeEvent(c(input$captfilename, input$captxlsname, input$captsheet), {
  captrv$clear <- FALSE
}, priority = 1000)

observeEvent(input$importfilename, {
  importrv$clear <- FALSE
  updateRadioButtons(session, "resultsbtn", label = "", 
                     inline = TRUE, choices = defaultresultsbtn)
}, priority = 1000)

observeEvent(input$secrdatabox, {
  # secrrv$clear <- FALSE
  updateRadioButtons(session, "resultsbtn", label = "", 
                     inline = TRUE, choices = defaultresultsbtn)
}, priority = 1000)

observeEvent(c(input$maskpolyfilename, input$maskpolyobjectname), {
  polyrv$clear <- FALSE
}, priority = 1000)

observeEvent(input$maskcovariatefilename, {
  covariaterv$clear <- FALSE
}, priority = 1000)

observeEvent(input$maskfilename, {
  maskrv$clear <- FALSE
}, priority = 1000)

##############################################################################

# Invalidate model
observeEvent(c(input$detectfnbox, input$likelihoodbtn, input$distributionbtn,
               input$hcovbox, input$model, input$modelotherargs,
               input$masktype, input$buffer, input$habnx, input$maskshapebtn, 
               input$maskpolybtn, input$maskpolyfilename, input$maskpolyobjectname, 
               input$maskcovariatefilename, input$clearspatialdata,
               input$clearpolygondata,
               input$maskfilename, input$filtercaptlink, input$filtercapttext), 
             ignoreInit = TRUE, {
               fitrv$value <- NULL
               updateRadioButtons(session, "resultsbtn", label = "", 
                                  inline = TRUE, choices = defaultresultsbtn)
               showNotification("model modified, yet to be fitted", id="lastaction", 
                                closeButton = FALSE,type="message", duration = NULL)
             })
##############################################################################

# Invalidate mask
observeEvent(c(input$datasource, input$secrdatabox, input$masktype), {
  reset("maskfilename")
  reset("maskpolygonsfilename")
  reset("maskcovariatefilename")
  updateNumericInput(session, "buffer", value = 100)
  updateNumericInput(session, "habnx", value = 32)
  updateRadioButtons(session, "maskshapebtn", selected = "Trap buffer")
  updateRadioButtons(session, "maskpolybtn", select = "None")
  updateRadioButtons(session, "maskcovariatebtn", select = "None")
  updateSelectInput(session, "maskcov", choices = "none", select = "none")
  updateCheckboxInput(session, "legend", value = FALSE)
  updateCheckboxInput(session, "dropmissing", value = FALSE)
  maskrv$data <- NULL
  maskrv$clear <- TRUE
  filtermaskrv$value <- FALSE
  updateTextInput(session, "filtermasktext", value = "")
  covariaterv$data <- NULL
  covariaterv$names <- character(0)
  covariaterv$clear <- TRUE
}, priority = 1000)
##############################################################################

observeEvent(input$arrayClick, {
  xy <- c(input$arrayClick$x, input$arrayClick$y)
  tmpgrid <- isolate(traprv$data)
  tmpcapt <- capthist()
  if (ms(tmpcapt)) {
    tmpcapt <- tmpcapt[[input$sess]]
    tmpgrid <- traps(tmpcapt)
  }
  if (!is.null(xy) && !is.null(tmpcapt)) 
  {
    if (detector(tmpgrid)[1] %in% polygondetectors) {
      nearest <- nearesttrap(xy, xy(tmpcapt))
      updateNumericInput(session, "animal", value = animalID(tmpcapt, names=FALSE)[nearest])
      id <- paste0(animalID(tmpcapt)[nearest], ":")
    }
    else {
      nearest <- nearesttrap(xy, tmpgrid)
      #-----------------------------------------------------
      ## machinery to cycle through animals at this detector
      if (lasttrap != nearest) clickno <<- 0
      clickno <<- clickno + 1
      lasttrap <<- nearest
      at.xy <- apply(tmpcapt[,,nearest, drop = FALSE],1,sum)
      at.xy <- which(at.xy>0)
      clickno <<- ((clickno-1) %% length(at.xy)) + 1
      #-----------------------------------------------------
      if (length(at.xy)>0) {
        updateNumericInput(session, "animal", value = as.numeric(at.xy[clickno]))
      }
    }
  }
})
##############################################################################

observeEvent(input$animal, {
  if (is.numeric(input$animal) && input$animal>0) {
    if (ms(capthist())) {
      currentIDrv$value <- rownames(capthist()[[input$sess]])[input$animal]
    }
    else {
      currentIDrv$value <- rownames(capthist())[input$animal]
    }
  }
  else {
    if (is.na(input$animal)) {
      updateNumericInput(session, "animal", value = 0)
    }
    currentIDrv$value <- ""
  }
})

observeEvent(input$sess, ignoreInit = TRUE, {
  if (input$animal>0) {
    # Tracking ID over sessions not working 2020-08-26, so suppress
    # ID <- currentIDrv$value
    # assume ms(capthist())
    # newsessanimal <- match(ID, rownames(capthist()[[input$sess]]))
    # if (is.na(newsessanimal)) {
    #   newsessanimal <- 1
    #   currentIDrv$value <- rownames(capthist()[[input$sess]])[1]
    # }
    # updateNumericInput(session, "animal", value = newsessanimal)
    updateNumericInput(session, "animal", value = 1)
  }
})

observeEvent(input$trapxlsname, {
  dataname <- input$trapxlsname[1,"datapath"]
  updateSelectInput(session, "trapsheet", choices = readxl::excel_sheets(dataname))
})

##############################################################################

observeEvent(input$captxlsname, {
  dataname <- input$captxlsname[1,"datapath"]
  sheets <- readxl::excel_sheets(dataname)
  samexls <- input$captxlsname[1,"name"] == input$trapxlsname[1,"name"]
  sheetnumber <- if (samexls) length(input$trapsheet)+1 else 1
  if (samexls && length(sheets)<2) {
    showNotification(id = "lastaction", type = "error", duration = NULL,
                     "cannot use same xls sheet")
    sheetnumber <- 1
  }
  updateSelectInput(session, "captsheet", 
                    choices = sheets,
                    selected = sheets[sheetnumber])
})

##############################################################################

observeEvent(input$alpha, {
  updateCheckboxInput(session, "powertype", label = paste0(
    round(100 *(1-input$alpha), 1), "% CI"))
})
##############################################################################

observeEvent(input$areaunit, ignoreInit = TRUE, {
  new.unit <- isolate(input$areaunit)
  if (new.unit != current$unit) {
    if (new.unit=="ha") {
      newD <- isolate(input$D)/100
    }
    else {
      newD <- isolate(input$D)*100
    }
    updateNumericInput(session, "D", paste0("D (animals / ", new.unit, ")"), value = newD)
    current$unit <- new.unit
  }
})
##############################################################################

observeEvent(input$CIclick, {
  invalidateOutputs()
  if (input$powertype) {
    updateNumericInput(session, "xpos", value = round(input$CIclick$x))
  }
  else {
  }
})
##############################################################################

observeEvent(input$selectnoanalyseslink, {
  updateCheckboxGroupInput(session, "analyses", 
                           selected = character(0))
})

##############################################################################

observeEvent(input$selectallanalyseslink, {
  if (nrow(sumrv$value) == 0) 
    selected <- character(0)
  else
    selected <-  paste0("Analysis", 1:nrow(sumrv$value))
  updateCheckboxGroupInput(session, "analyses", selected =  selected)
})

##############################################################################

observeEvent(input$clearallanalyseslink, {
  showModal(modalDialog(
    title="Clear all summaries",
    footer = tagList(actionButton("confirmclearall", "Yes"),
                     modalButton("No")
    ),
    size = 's'
  ))
})

observeEvent(input$confirmclearall, {
  sumrv$value <- sumrv$value[0,]
  updateCheckboxGroupInput(session, "analyses",
                           choices = character(0), selected = character(0))
  updateCheckboxInput(session, "keepselectedbox", value = FALSE)
  output$summaries <- renderText("false")
  removeModal()
})

##############################################################################

observeEvent(c(input$clearimportlink, input$datasource), ignoreInit = TRUE, {
  
  reset('trapfilename')
  reset('captfilename')
  reset('captxlsname')
  disable("captfilename")  # waiting for trap file
  disable("captxlsname")  # waiting for trap xls
  
  reset('importfilename')
  
  updateSelectInput(session, "detectfnbox", choices = c('HN','HR','EX', hazarddetectfn), 
                    selected = "HN")
  
  if (!is.null(input$datasource) && input$datasource != 'secr dataset') {
    reset('secrdatabox')   # 2022-02-08
    secrrv$data <- NULL
  }
  
  importrv$data <- NULL
  importrv$clear <- TRUE
  
  traprv$data <- NULL
  traprv$clear <- TRUE
  
  captrv$data <- NULL
  captrv$clear <- TRUE
  
})

##############################################################################

observeEvent(c(input$detector), {
  if (input$detector %in% polygondetectors) {
    updateSelectInput(session, "detectfnbox", choices = hazarddetectfn, selected = "HHN")
    updateSelectInput(session, "fmt", label = "Format", choices = c("XY"), selected = 'XY')
    disable('suggestbufferlink')
  }
  else {
    updateSelectInput(session, "detectfnbox", choices = c('HN','HR','EX',hazarddetectfn))
    updateSelectInput(session, "fmt", label = "Format", choices = c("trapID", "XY"))
    enable('suggestbufferlink')
  }
  fitrv$value <- NULL
})
##############################################################################

observeEvent(input$detectfnbox, {
  fitrv$value <- NULL
})
##############################################################################

observeEvent(input$distributionbtn, {
  fitrv$value <- NULL
})
##############################################################################

observeEvent(input$incrementtime, ignoreInit = TRUE, {
  timerv$timelimit <- timerv$timelimit+1
})

observeEvent(input$showtrapfilelink, ignoreInit = TRUE, {
  capttextrv$value <- FALSE
  traptextrv$value <- !is.null(input$trapfilename) && (input$showtrapfilelink %% 2 == 1)
})

observeEvent(input$mainlink, ignoreInit = TRUE, {
  updateNavlistPanel(session, "navlist", "Main screen")
})

observeEvent(input$mainlink2, ignoreInit = TRUE, {
  updateNavlistPanel(session, "navlist", "Main screen")
})

observeEvent(input$mainlink3, ignoreInit = TRUE, {
  updateNavlistPanel(session, "navlist", "Main screen")
})

observeEvent(input$optionslink, ignoreInit = TRUE, {
  updateNavlistPanel(session, "navlist", "Options")
})

observeEvent(input$hideresultsbtn, ignoreInit = TRUE, {
  if (input$hideresultsbtn %% 2 == 1) {
    shinyjs::hide("resultsbtn")
    shinyjs::hide("otherfunction")
    shinyjs::hide("resultsPrint")
    shinyjs::hide("savebtn")
    # updateActionLink(session, "hideresultslink", HTML("<small>show summary results</small>"))
    updateActionButton(session, "hideresultsbtn", HTML("<small>show summary results</small>"))
  }
  else {
    shinyjs::show("resultsbtn")
    shinyjs::show("otherfunction")
    shinyjs::show("resultsPrint")
    if (!is.null(fitrv$value)) shinyjs::show("savebtn")
    # updateActionLink(session, "hideresultslink", HTML("<small>hide</small>"))
    updateActionButton(session, "hideresultsbtn", HTML("<small>hide</small>"))
  }
})

observeEvent(input$masklink, ignoreInit = TRUE, {
  updateNavlistPanel(session, "navlist", "Habitat mask")
})

observeEvent(input$clearspatialdata, ignoreInit = TRUE, {
  updateRadioButtons(session, "maskcovariatebtn", select = "None")
  updateSelectInput(session, "maskcov", choices = "none", select = "none")
  updateCheckboxInput(session, "legend", value = FALSE)
  updateCheckboxInput(session, "dropmissing", value = FALSE)
  reset("maskcovariatefilename")
  removeNotification(id = "badcovariatefile")
  removeNotification(id = "nofile")
  removeNotification(id = "lastaction")
  covariaterv$data <- NULL
  covariaterv$names <- character(0)
  covariaterv$clear <- TRUE
}, priority = 1000)

observeEvent(input$maskcovariatebtn, ignoreInit = TRUE, {
  # identical but does not reset itself
  updateSelectInput(session, "maskcov", choices = "none", select = "none")
  updateCheckboxInput(session, "legend", value = FALSE)
  updateCheckboxInput(session, "dropmissing", value = FALSE)
  reset("maskcovariatefilename")
  removeNotification(id = "badcovariatefile")
  removeNotification(id = "nofile")
  removeNotification(id = "lastaction")
  covariaterv$data  <- NULL
  covariaterv$names <- character(0)
  covariaterv$clear <- TRUE
}, priority = 1000)

observeEvent(input$maskpolybtn, ignoreInit = TRUE, {
  reset("maskpolyfilename")
  reset("maskpolyobjectname")
  polyrv$data <- NULL
  polyrv$clear <- TRUE
}, priority = 1000)

observeEvent(input$clearpolygondata, ignoreInit = TRUE, {
  reset("maskpolyfilename")
  reset("maskpolyobjectname")
  polyrv$data <- NULL
  polyrv$clear <- TRUE
  updateRadioButtons(session, "includeexcludebtn", selected = "Include")
  updateRadioButtons(session, "maskpolybtn", selected = "None")
}, priority = 1000)

observeEvent(input$clearbufferspec, ignoreInit = TRUE, {
  updateNumericInput(session, "buffer", value = 100)
  updateNumericInput(session, "habnx", value = 32)
  updateRadioButtons(session, "maskshapebtn", selected = "Trap buffer")
}, priority = 1000)

observeEvent(input$helplink, ignoreInit = TRUE, {
  updateNavlistPanel(session, "navlist", "Help")
})


observeEvent(input$showcaptfilelink, ignoreInit = TRUE, {
  traptextrv$value <- FALSE
  capttextrv$value <- !is.null(input$captfilename) && (input$showcaptfilelink %% 2 == 1)
})

observeEvent(input$filtercaptlink, ignoreInit = TRUE, {
  filtercaptrv$value <- (input$filtercaptlink %% 2) == 1
})

observeEvent(input$filtermask, ignoreInit = TRUE, {
  filtermaskrv$value <- (input$filtermask %% 2) == 1
})

observeEvent(input$hidegraphicsbtn, ignoreInit = TRUE, {
  if (input$hidegraphicsbtn %% 2 == 1) {
    shinyjs::hide("plottabs")
    updateActionButton(session, "hidegraphicsbtn", HTML("<small>show code and plots</small>"))
    # tags$head(
    #   tags$style(
    #     HTML(
    #       "#resultsPrint {
    #         max-height: 500px;
    #       }"
    #     )
    #   )
    # )
  }
  else {
    shinyjs::show("plottabs")
    updateActionButton(session, "hidegraphicsbtn", HTML("<small>hide</small>"))
    
    # tags$head(
    #   tags$style(
    #     HTML(
    #       "#resultsPrint {
    #         max-height: 270px;
    #       }"
    #     )
    #   )
    # )
  }
})

observeEvent(input$secrhelptopicbtn, ignoreInit = TRUE, {
  if (input$secrhelptopicbtn %% 2 == 0) {
    updateRadioButtons(session, 'resultsbtn', selected = "summary")
    updateTextInput(session, "otherfunction", value = oldfn)
  }
  else {
    oldfn <<- input$otherfunction
    updateRadioButtons(session, 'resultsbtn', selected = "other")
    updateTextInput(session, "otherfunction", value = paste0("?", input$secrdatabox))
  }
})

##############################################################################
## Modal dialogue to confirm fitting if it might take a long time
OKModal <- function(expectedtime) {
  timestr <- format(round(expectedtime,2), nsmall = 2)
  modalDialog(
    paste("Fitting is predicted to take ", timestr, " minutes"),
    size = "s",
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okbtn", "Continue")
    )
  )
}

observeEvent(input$fitbtn, ignoreInit = TRUE, {
  ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
  if (is.null(capthist())) {
    showNotification("load data",
                     type = "warning", id = "nodata", duration = seconds)
  }
  else {
    # one likelihood
    # force garbage collection for greater accuracy of timing
    gc(verbose = FALSE)
    LL <- try(fitmodel(LLonly = TRUE) )
    if (inherits(LL, 'try-error')) {
      showNotification("failed to compute expected time")
      expectedtime <- Inf
    }
    else {
      expectedtime <- timefn(LL)/60
      if (expectedtime > timerv$timewarning)
        if (expectedtime > timerv$timelimit) {
          showNotification("exceeds time limit")
        }
      else {
        showModal(OKModal(expectedtime))
      }
      else {
        fitmodel()
      }
    }
  }
})

##############################################################################

# defunct 2020-09-07 as browseURL doesn't work on server  
# observeEvent(input$secrhelpbtn, ignoreInit = TRUE, {
#   secrhelp <- file.path(path.package("secr"), "html",  "00Index.html")
#   if (file.exists(secrhelp)) {
#     browseURL(secrhelp)
#   }
#   else {
#     # revert to pdf manual if html help not found
#     browseURL("https://www.otago.ac.nz/density/pdfs/secr-manual.pdf")
#   }
# })
##############################################################################

observeEvent(input$likelihoodbtn, ignoreInit = TRUE, {
  fitrv$value <- NULL
  ## drop or add density formula depending on full/conditional likelihood
  if (input$likelihoodbtn == "Full") {
    updateTextInput(session, "model", value = paste0("D~1, ", input$model))
  }
  else {
    form <- strsplit(input$model, ",")[[1]]
    form <- form[!grepl("D", form)]
    form <- stringr::str_trim(form)
    newmodel <- paste(form, collapse = ", ")
    updateTextInput(session, "model", value = newmodel)
  }
  
})
##############################################################################

observeEvent(input$detectfnbox, ignoreInit = TRUE,  {
  fitrv$value <- NULL
  if (input$detectfnbox %in% c('HHN', 'HHR', 'HEX', 'HAN', 'HCG', 'HVP'))
    detectrv$value <- 'lambda0'
  else 
    detectrv$value <- 'g0'
  ## switch detect0 name
  form <- strsplit(input$model, ",")[[1]]
  form <- stringr::str_trim(form)
  ## find g0/lambda0
  form <- gsub("g0~", "detect0~", form)
  form <- gsub("lambda0~", "detect0~", form)
  form <- gsub("detect0~", paste0(detectrv$value, "~"), form)
  
  usez <- input$detectfnbox %in% c('HR', 'CLN','CG','HHR','HCG','HVP')
  zpos <- grep('z~', form)
  if (usez && length(zpos)==0)
    form <- c(form, 'z~1')
  if (!usez && length(zpos)>0)
    form <- form[-zpos]
  
  usew <- input$detectfnbox %in% c('WEX', 'ANN', 'HAN')
  wpos <- grep('w~', form)
  if (usew && length(wpos)==0)
    form <- c(form, 'w~1')
  if (!usew && length(wpos)>0)
    form <- form[-wpos]
  
  newmodel <- paste(form, collapse = ", ")
  updateTextInput(session, "model", value = newmodel)
})
##############################################################################

observeEvent(input$detector, ignoreInit = TRUE, {
  fitrv$value <- NULL
  traprv$value <- NULL
  captrv$value <- NULL
  secrrv$value <- NULL
})
##############################################################################

observeEvent(input$trapcovnames, ignoreInit = TRUE, {
  fitrv$value <- NULL
  traprv$value <- NULL
  captrv$value <- NULL
  secrrv$value <- NULL
})
##############################################################################

observeEvent(input$trapotherargs, ignoreInit = TRUE, {
  fitrv$value <- NULL
  traprv$value <- NULL
  captrv$value <- NULL
  secrrv$value <- NULL
})

##############################################################################

observeEvent(input$covnames, ignoreInit = TRUE, {
  fitrv$value <- NULL
  captrv$value <- NULL
  secrrv$value <- NULL
})

observeEvent(input$captotherargs, ignoreInit = TRUE, {
  fitrv$value <- NULL
  captrv$value <- NULL
  secrrv$value <- NULL
})

##############################################################################

observeEvent(input$model, {
  fitrv$value <- NULL
})
##############################################################################

observeEvent(input$okbtn, {
  ## user happy: proceed with long simulation
  removeModal()
  fitmodel()
})

##############################################################################

observeEvent(input$modelotherargs, {
  fitrv$value <- NULL
})
##############################################################################

observeEvent(input$pxyclick, {
  invalidateOutputs()
  if (ms(traprv$data)) 
    trps <- traprv$data[[input$sess]]
  else
    trps <- traprv$data
  
  border <- border(input$pxyborder)
  
  xy <- c(input$pxyclick$x, input$pxyclick$y)
  if ((xy[2] < (min(trps$y) - border)) ||
      (xy[2] > (max(trps$y) + border)) ||
      (xy[1] < (min(trps$x) - border)) ||
      (xy[1] > (max(trps$x) + border)) ) {
    pxyrv$value <- NULL
  }
  else {
    detectparlist <- list(detect0(), sigma(), zw())
    names(detectparlist) <- c(detectrv$value, 'sigma', 'z')
    Pxy <- pdot (xy, trps,
                 detectfn = input$detectfnbox,
                 detectpar = detectparlist,
                 noccasions = noccasions()[input$sess])
    pxyrv$xy <-xy
    pxyrv$value <- Pxy}
})

##############################################################################
observeEvent(input$Dclick, {
  invalidateOutputs()
  dsurf <- if (ms(fitrv$dsurf)) fitrv$dsurf[[input$sess]] else fitrv$dsurf
  Drv$xy <-c(input$Dclick$x, input$Dclick$y)
  if (pointsInPolygon(Drv$xy, dsurf)) {
    msk <- if (ms(mask())) mask()[[input$sess]] else mask()
    maskrow <- nearesttrap(Drv$xy, msk)
    cov <- 'D.0'
    Drv$value <- covariates(dsurf)[maskrow, cov, drop = FALSE]
  }
  else {
    Drv$value <- NULL
  }
})
##############################################################################

observeEvent(input$randompopbtn, ignoreInit = TRUE, {
  # invalidates pop when button pressed
  poprv$v <- poprv$v + 1
})

##############################################################################

observeEvent(input$dummybookmarkbutton, ignoreInit = TRUE, {
  showNotification("Bookmarking is disabled in secrapp 2.0", id = "lastaction", 
                   duration = NULL)  
})

observeEvent(input$resetbtn, ignoreInit = TRUE, {
  
  current$unit <- "ha"
  fitrv$value <- NULL
  traptextrv$value <- FALSE
  capttextrv$value <- FALSE
  filtercaptrv$value <- FALSE
  filtermaskrv$value <- FALSE
  timerv$timewarning <- timewarning
  timerv$timelimit <- timelimit
  
  ## Data input
  updateRadioButtons(session, "datasource", selected = "Text files")
  
  ## Trap layout
  updateTextInput(session, "trapotherargs", 
                  value = "", placeholder = "e.g., skip = 1")
  updateSelectInput(session, "detector", selected = "multi")
  updateTextInput(session, "trapcovnames", value = "", placeholder = "e.g., traptype, habitat")
  updateSelectInput(session, "trapsheet", "Sheet", choices = c("Sheet1"))
  
  ## Captures
  updateTextInput(session, "captotherargs", 
                  value = "", placeholder = "e.g., skip = 1")
  updateSelectInput(session, "fmt", selected = "trapID")
  updateTextInput(session, "covnames", value = "", placeholder = "e.g., sex")
  updateSelectInput(session, "captsheet", "Sheet", choices = c("Sheet1"))
  updateTextInput(session, "filtercapttext", value = "", placeholder = "e.g., session = 1")
  
  ## Model
  
  updateSelectInput(session, "detectfnbox", selected = "HN")
  updateRadioButtons(session, "distributionbtn", selected = "Poisson")
  updateRadioButtons(session, "likelihoodbtn", selected = "Full")
  updateSelectInput(session, "hcovbox", choices = "none", selected = "none")
  
  updateTextInput(session, "model", 
                  value = "D~1, g0~1, sigma~1", placeholder = "")
  updateTextInput(session, "modelotherargs", 
                  value = "", placeholder = "e.g., details = list(fastproximity = FALSE)")
  
  ## Actions
  updateTextInput(session, "title", "", value = "",
                  placeholder = "label for Summary")
  updateTextInput(session, "otherfunction", 
                  value = "", placeholder = "e.g., RPSV(ch, CC = TRUE)")
  
  ## Results
  updateRadioButtons(session, "resultsbtn", label = "", 
                     inline = TRUE, choices = defaultresultsbtn, selected = "summary")
  
  ## Array plot
  updateCheckboxInput(session, "tracks", value = FALSE)
  updateCheckboxInput(session, "fxi", value = FALSE)
  updateCheckboxInput(session, "varycol", value = FALSE)
  updateNumericInput(session, "animal", value = 1)
  updateNumericInput(session, "sess", value = 1)
  updateNumericInput(session, "masksess", value = 1)
  updateCheckboxInput(session, "usageplot", value = FALSE)
  
  ## Moves plot
  updateNumericInput(session, "nbar", value = 10)
  
  updateCheckboxInput(session, "movesallbox", value = FALSE)
  updateCheckboxInput(session, "withinsessiononly", value = FALSE)
  
  ## pop plot
  updateCheckboxInput(session, "showHRbox", "Display 95% home range", value = FALSE)
  
  ## pxy plot
  updateCheckboxInput(session, "maskedge", value = FALSE)
  
  ## Dxy plot
  updateTextInput(session, "Dxycol", value = "heat.colors(15, alpha = 1, rev = TRUE)")
  
  ## Density plot
  updateCheckboxInput(session, "Dmaskedge", value = FALSE)
  updateCheckboxInput(session, "Dshowdetectors", value = FALSE)
  updateCheckboxInput(session, "Dshowdetections", value = FALSE)
  updateCheckboxInput(session, "Dshowpopn", value = FALSE)
  
  ## power plot
  updateCheckboxInput(session, "adjustRSEbox", value = TRUE)
  updateCheckboxInput(session, "powertype", "95% CI", value = FALSE)
  updateNumericInput(session, "xpos", value = 0)
  
  ## Habitat mask
  
  updateNumericInput(session, "buffer", value = 100)
  updateNumericInput(session, "habnx", value = 32)
  updateRadioButtons(session, "maskshapebtn", selected = "Trap buffer")
  updateRadioButtons(session, "maskpolybtn", selected = "None")
  
  updateCheckboxInput(session, "dropmissing", value = FALSE)
  updateRadioButtons(session, "includeexcludebtn", selected = "Include")
  
  updateCheckboxInput(session, "dotsbox", value = FALSE)
  updateNumericInput (session, "maskborder", value = 1)
  updateCheckboxInput(session, "frame", value = TRUE)
  updateCheckboxInput(session, "maskedge2", value = FALSE)
  updateCheckboxInput(session, "legend", value = FALSE)
  updateCheckboxInput(session, "showpoly", value = FALSE)
  updateSelectInput(session, "maskcov", choices = "none", selected = "none")
  updateTextInput(session, "filtermasktext", value = "", placeholder = "")
  
  ## Summary
  
  # safer to leave this for manual reset using Summary page buttons       
  # sumrv$value <- sumrv$value[0,]
  # updateCheckboxGroupInput(session, "analyses", 
  #                          choices = character(0), selected = character(0))
  # updateCheckboxInput(session, "keepselectedbox", value = FALSE)
  
  ## Options
  
  updateNumericInput(session, "ncores", value = defaultcores)
  updateSelectInput(session, "method", selected = "Newton-Raphson")
  
  ## detector array
  updateRadioButtons(session, "areaunit", selected = "ha")
  
  updateNumericInput(session, "dec", value = 4)
  
  ## array plot
  updateCheckboxInput(session, "entireregionbox", value = FALSE)
  updateNumericInput(session, "arrayborder", value = 20)
  updateCheckboxInput(session, "arrayframe", value = FALSE)
  
  updateRadioButtons(session, "gridlines", selected = "None")
  updateNumericInput(session, "rad", value = 5)
  updateNumericInput(session, "cex", value = 1)
  
  ## pxy plot
  updateNumericInput(session, "pxyborder", value = 3)
  updateNumericInput(session, "pxynx", value = 64)
  updateCheckboxInput(session, "pxyfillbox", value = TRUE)
  updateCheckboxInput(session, "pxyframebox", value = FALSE)
  updateCheckboxInput(session, "pxylabelbox", value = TRUE)
  
  updateRadioButtons(session, "powerplotbtn", selected = "Null hypothesis power")
  
  updateNumericInput(session, "alpha", value = 0.05)
  updateNumericInput(session, "target", value = 80)
  updateSelectInput(session, "testtype", selected = "two.sided")
  updateNumericInput(session, "minEffect", value = -99)
  updateNumericInput(session, "maxEffect", value = 150)
  updateNumericInput(session, "fromR", value = 0.2)
  updateNumericInput(session, "toR", value = 4)
  updateNumericInput(session, "byR", value = 0.2)
  updateNumericInput(session, "simbyR", value = 0.4)
  invalidateOutputs()
  
  traprv$data <- NULL
  traprv$clear <- TRUE
  reset('trapfilename')
  reset('trapxlsname')
  
  captrv$data <- NULL
  captrv$clear <- TRUE
  reset('captfilename')
  reset('captxlsname')
  disable("captfilename")
  disable("captxlsname")
  
  polyrv$data <- NULL
  polyrv$clear <- TRUE
  reset("maskpolyfilename")
  reset("maskpolyobjectname")
  
  
  covariaterv$data <- NULL
  covariaterv$names <- character(0)
  covariaterv$clear <- TRUE
  reset('maskcovariatefilename')
  
  maskrv$data <- NULL
  maskrv$clear <- TRUE
  reset("maskfilename") 
  
  importrv$data <- NULL
  importrv$clear <- TRUE
  reset('importfilename')
  
  secrrv$data <- NULL
  # secrrv$clear <- TRUE
  updateSelectInput(session, "secrdatabox", selected = "captdata")
  
  detectrv$value <- 'g0'
  
  # hide/show
  # reset() doesn't work for actionButton (see ?shinyjs::reset)
  if (input$hideresultsbtn %% 2 == 1) shinyjs::click("hideresultsbtn")
  if (input$hidegraphicsbtn %% 2 == 1) shinyjs::click("hidegraphicsbtn")
  if (input$secrhelptopicbtn %%2 == 1) shinyjs::click("secrhelptopicbtn")
  
  showNotification("all inputs reset", id = "lastaction",
                   closeButton = FALSE, type = "message", duration = seconds)
  
}, priority = 1000)

##############################################################################

observeEvent(input$selectallfieldslink, {
  updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
  updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])
}   )
##############################################################################

observeEvent(input$selectdefaultfieldslink, {
  updateCheckboxGroupInput(session, "fields1", selected = defaultfields1)
  updateCheckboxGroupInput(session, "fields2", selected = defaultfields2)
}   )
##############################################################################

observeEvent(input$selectfieldsbtn, {
  selectingfieldsrv$value <- ! selectingfieldsrv$value
  output$selectingfields <- renderText(selectingfieldsrv$value)
  
}   )
##############################################################################

observeEvent(input$selectanalysesbtn, {
  selectinganalysesrv$value <- ! selectinganalysesrv$value
  output$selectinganalyses <- renderText(selectinganalysesrv$value)
  
}   )
##############################################################################

observeEvent(input$selectnofieldslink, {
  updateCheckboxGroupInput(session, "fields1", selected = "")
  updateCheckboxGroupInput(session, "fields2", selected = "")
}   )
##############################################################################

updatebuffer <- function() {
  ch <- capthist()
  if (is.null(ch)) {
    showNotification(id = "lastaction", type = "warning", duration = NULL,
                     "input capthist before selecting suggest buffer")
  }
  else {
    det <- if (ms(ch)) detector(traps(ch[[1]])) else detector(traps(ch))
    if (det[1] %in% polygondetectors) {
      showNotification(id = "lastaction", type = "warning", duration = NULL,
                       "suggest.buffer is for point detectors; set manually")
    }
    else {
      ## 2020-09-08 suppressed use of fitted model fitrv$value
      ## because could be misleading if detectfn changed
      RBtarget <- 0.001   ## 0.1%
      progress <- Progress$new(session, min = 1, max = 15)
      on.exit(progress$close())
      progress$set(message = 'Suggesting buffer width ...', detail = '')
      ch <- if (ms(ch)) ch[[input$masksess]] else ch
      # 0.3 is a guess
      detectparlist <- list(0.3, RPSV(ch, CC = TRUE), 1)
      names(detectparlist) <- c(detectrv$value, 'sigma', 'z')
      buff <- try(suggest.buffer(ch, detectfn = input$detectfnbox,
                                 detectpar = detectparlist,
                                 noccasions = noccasions()[1], RBtarget = RBtarget))
      if (inherits(buff, "try-error")) {
        showNotification(id = "lastaction", type = "error", duration = NULL,
                         "suggest.buffer failed; set width manually")
      }
      else {
        updateNumericInput(session, "buffer", value = signif(buff[1],3))
      }
    }
  }
}

##############################################################################
observeEvent(input$suggestbufferlink, ignoreInit = TRUE, {
  updatebuffer()
})
##############################################################################
observeEvent(input$suggestbufferlink2, ignoreInit = TRUE, {
  updatebuffer()
})
##############################################################################

observeEvent(input$maskcovariatefilename, {
  updateSelectInput(session, "maskcov", 
                    choices = c("none", covariaterv$names))
  removeNotification(id="lastaction")
})


##############################################################################
observeEvent(c(input$masktype), {
  updateSelectInput(session, "maskcov", choices = "none", selected = "none")
  covariaterv$data <- NULL
  covariaterv$names <- character(0)
  removeNotification(id="lastaction")
})
##############################################################################


observeEvent(c(
  input$hideresultsbtn, 
  input$hidegraphicsbtn,
  traprv$data,
  capthist(),
  fitrv$value,
  input$likelihoodbtn
), {
  hideplotif <- function (condition, tab) {
    if (condition || hideallplots)
      hideTab(inputId = "plottabs", target = tab)
    else 
      showTab(inputId = "plottabs", target = tab)
  }
  hideallplots <- input$hidegraphicsbtn %% 2 == 1
  hideplotif (FALSE, "Code")
  hideplotif (is.null(traprv$data), "Array")
  hideplotif (is.null(capthist()), "Moves")
  hideplotif (is.null(fitrv$value), "Detectfn")
  hideplotif (is.null(fitrv$value) , "Buffer")
  hideplotif (is.null(fitrv$value) , "Pxy")
  hideplotif (is.null(fitrv$value) || (input$likelihoodbtn != 'Full'), "Dxy")
  hideplotif (is.null(fitrv$value) || (input$likelihoodbtn != "Full"), "Popn")
  hideplotif (is.null(fitrv$value) || (input$likelihoodbtn != "Full"), "Power")
})

## read or re-read trap file

observeEvent(c(input$trapfilename, input$trapxlsname, input$detector, 
               input$trapcovnames, input$trapotherargs, input$trapsheet), 
             ignoreInit = FALSE, {
               req(!traprv$clear)
               if (input$datasource == 'Text files') {
                 req(input$trapfilename)
               }
               else if (input$datasource == 'Excel files') {
                 req(input$trapxlsname)
               }
               removeNotification("badtrapotherargs")
               removeNotification("badtrap")
               removeNotification("badcapt")
               
               if (!bookmarkrv$value) {
                 reset('importfilename')
                 importrv$data <- NULL
                 importrv$clear <- TRUE
                 
                 # reset('secrdatabox')
                 # secrrv$data <- NULL
                 # secrrv$clear <- TRUE
                 
                 reset('captfilename')
                 reset('captxlsname')
                 updateSelectInput(session, "captsheet", choices = "Sheet1")
                 captrv$data <- NULL
                 captrv$clear <- TRUE
               }
               sheet <- ""
               if (input$datasource == 'Text files') {
                 if (bookmarkrv$value)
                   trapdataname <- bookmarkrv$trapfilename
                 else
                   trapdataname <- input$trapfilename[,"datapath"]
               }
               else {
                 if (bookmarkrv$value)
                   trapdataname <- bookmarkrv$trapxlsname
                 else
                   trapdataname <- input$trapxlsname[,"datapath"]
                 # assume common sheet name for now
                 checksheet <- function(dname) {
                   input$trapsheet %in% readxl::excel_sheets(dname)
                 }
                 if (!all(sapply(trapdataname, checksheet))) return()
                 if (!grepl('sheet', input$trapotherargs)) {
                   sheet <- paste0(", sheet = '", input$trapsheet, "'")
                 }
               }
               
               tempargs <- try(eval(parse(text = paste0("list(", input$trapotherargs, ")"))), silent = TRUE)
               if (inherits(tempargs, "try-error")) {
                 showNotification("trap arguments incomplete or invalid", type = "error", 
                                  id = "badtrapotherargs", duration = NULL)
               }
               else {
                 args <- input$trapotherargs
                 if (args != "") {
                   args <- paste0(", ", args)
                 }
                 
                 readtrapcall <-  paste0("read.traps (trapdataname, detector = input$detector", 
                                         args, sheet, ")")
                 
                 temp <- try(eval(parse(text = readtrapcall)))
                 if (!inherits(temp, "traps")) {
                   showNotification("invalid trap file or arguments; try again",
                                    type = "error", id = "badtrap", duration = NULL)
                   traprv$data <- NULL
                 }
                 else {
                   if (ms(temp)) {
                     ## temporary session names
                     names(temp) <- paste('Session', 1:length(temp))  
                     updateNumericInput(session, "sess", max = length(temp))
                     updateNumericInput(session, "masksess", max = length(temp))
                     updateNumericInput(session, "rad", value = signif(spacing(temp[[1]])/5, 2))
                   }
                   else {
                     updateNumericInput(session, "rad", value = signif(spacing(temp)/5, 2))
                   }
                   output$multisession <- renderText(tolower(ms(temp)))
                   ncov <- ncol(covariates(temp))
                   if (length(ncov)>0 && ncov>0) {
                     covnames <- getcovnames(input$trapcovnames, ncov, 'T', TRUE, FALSE)
                     names(covariates(temp)) <- covnames
                   }
                   traprv$data <- temp
                   enable("captfilename")
                   enable("captxlsname")
                   showNotification("detector layout loaded", closeButton = FALSE, 
                                    type = "message", id = "lastaction", duration = seconds)
                 }
               }
             })

##############################################################################
## use builtin data 
observeEvent(c(input$datasource, input$secrdatabox), ignoreInit = TRUE, {
  req(input$secrdatabox)
  removeNotification("badcapt")
  secrrv$data <- NULL
  traprv$data <- NULL
  fitrv$value <- NULL
  if (input$datasource == "secr dataset") {
    ch <- get(input$secrdatabox)
    if (inherits(ch, 'capthist')) {
      secrrv$data <- ch
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
      showNotification(paste("using", input$secrdatabox), closeButton = FALSE, 
                       type = "message", id = "lastaction", duration = seconds)
    }
    else {
      showNotification("not a builtin capthist object", 
                       type = "error", id = "badcapt", duration = NULL)
    }
  }
})

