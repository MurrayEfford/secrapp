## renderUI

## persqkm
## detectorhelp
## clusterhelp
## clusteroverlap
## randomtext
## shapefile
## exclusionfile
## habitatfile
## uipopN
## uigridlines

##############################################################################

output$detectfnui <- renderUI({
  if (input$detectfnbox == 'HN') x <- HTML("halfnormal")  
  else if (input$detectfnbox == 'EX') x <- HTML("negative exponential")  
  else if (input$detectfnbox == 'HR') x <- HTML("hazard rate")  
  else if (input$detectfnbox == 'HHN') x <- HTML("hazard halfnormal")  
  else if (input$detectfnbox == 'HEX') x <- HTML("hazard negative exponential")  
  else if (input$detectfnbox == 'HHR') x <- HTML("hazard hazard rate")  
  else if (input$detectfnbox == 'HVP') x <- HTML("hazard variable power")  
  else x <- ''
  helpText(x)
})
#-----------------------------------------------------------------------------

output$secrdatadescriptionui <- renderUI({
  x <- ""
  if (input$secrdatabox == 'captdata') 
    x <- HTML("Simulated data from grid of multi-catch traps (from DENSITY)")  
  else if (input$secrdatabox %in% c('blackbearCH'))
    x <- HTML("<i>Ursus americanus</i> DNA hair snag data from ",
              "Great Smoky Mountains National Park, Tennessee") 
  else if (input$secrdatabox %in% c('deermouse.ESG', 'deermouse.WSG'))
    x <- HTML("<i>Peromyscus maniculatus</i> Live-trapping data of V. H. Reid ",
              "published as a CAPTURE example by Otis et al. (1978) Wildlife Monographs 62") 
  else if (input$secrdatabox == 'hornedlizardCH') 
    x <- HTML("Repeated searches of a quadrat in Arizona for flat-tailed ",
              "horned lizards <i>Phrynosoma mcallii</i> (Royle & Young Ecology 89, 2281--2289)") 
  else if (input$secrdatabox == 'housemouse') 
    x <- HTML("<i>Mus musculus</i> live-trapping data of H. N. Coulombe published ",
              "as a CAPTURE example by Otis et al. (1978) Wildlife Monographs 62 ")
  else if (input$secrdatabox %in% c('ovenCHp'))
    x <- HTML("Multi-year mist-netting study of ovenbirds ",
              "<i>Seiurus aurocapilla</i> at a site in Maryland, USA ",
              "(Dawson and Efford 2009) (nets as binary proximity detectors)")
  else if (input$secrdatabox == 'OVpossumCH')
    x <- HTML("Brushtail possum <i>Trichosurus vulpecula</i> live trapping in the ",
              "Orongorongo Valley, Wellington, New Zealand 1996--1997 ",
              "(Efford and Cowan In: The Biology of Australian Possums and Gliders ",
              "Goldingay and Jackson eds. Pp. 471--483).")
  else if (input$secrdatabox == 'possumCH') 
    x <- HTML("Brushtail possum <i>Trichosurus vulpecula</i> live trapping at ",
              "Waitarere, North Island, New Zealand April 2002 (Efford et al. 2005 ",
              "Wildlife Society Bulletin 33, 731--738)")
  else if (input$secrdatabox %in% c('lineoCH')) 
    x <- HTML(" Multi-session skink (",
              "<i>O. lineoocellatum</i>) pitfall trapping data from Lake Station, ",
              "Upper Buller Valley, South Island, New Zealand (M. G. Efford, ",
              "B. W. Thomas and N. J. Spencer unpublished). ")
  else if (input$secrdatabox %in% c('infraCH')) 
    x <- HTML(" Multi-session skink (<i>Oligosoma infrapunctatum</i>) ",
              "pitfall trapping data from Lake Station, ",
              "Upper Buller Valley, South Island, New Zealand (M. G. Efford, ",
              "B. W. Thomas and N. J. Spencer unpublished). ")
  else if (input$secrdatabox == 'stoatCH')
    x <- HTML("Stoat <i>Mustela erminea</i> hair tube DNA data from Matakitaki ",
              "Valley, South Island, New Zealand (Efford, Borchers and Byrom 2009).")
  helpText(x)
})

output$moveswarningui <- renderUI({
  if (ms(capthist())) {
    if (input$movesallbox) 
      ch <- join(capthist())
    else
      ch <- capthist()[[input$sess]]
  }
  else {
    ch <- capthist()
  }
  captperoccasion <- table(apply(ch != 0, c(1,2), sum))
  ncap <- as.numeric(names(captperoccasion))
  withinoccasionrecap <- 0
  if (any (ncap>1)) {
    withinoccasionrecap <- sum(captperoccasion[ncap>1] * (ncap[ncap>1]-1))
  }
  x <- NULL
  if (withinoccasionrecap>0) {
    x <- HTML("Warning: detector allows within-occasion re-detections ( n = ", withinoccasionrecap, 
              ") whose order in time is unknown; corresponding movements are unreliable.")
  }
  helpText(x)
})
#-----------------------------------------------------------------------------

output$sessionnumberui <- renderUI({
  x <- NULL
  if (ms(capthist())) {
    if (input$movesallbox) {
      x <- HTML("Sessions combined")
    }
    else {
      x <- HTML("Session ", input$sess, " selected on Array tab")
    }
  }
  helpText(x)
})
#-----------------------------------------------------------------------------

output$timelimitui <- renderUI({
  req(timerv)
  x <- paste0("Time limit ", timerv$timelimit, " minutes")
  helpText(x)
})
#-----------------------------------------------------------------------------

output$ncoresui <- renderUI({
  x <- paste0(availablecores, " cores available")
  helpText(x)
})
#-----------------------------------------------------------------------------

output$secrdesignurl <- renderUI ({
  if (is.null(fitrv$value) & !is.null(capthist())) {
    LL <- try(fitmodel(LLonly = TRUE) , silent = TRUE)
    if (is.null(LL) || inherits(LL, 'try-error')) {
      NULL # tag$a(" ")
    }
    else {
      expectedtime <- timefn(LL)/60
      tags$a(paste0('Fit time ~', round(expectedtime,1), ' min'))
    }
  }
  else  {
    # only show after model fitted
    req(fitrv$value)
    
    parm <- c(
      paste0("detectfnbox=", input$detectfnbox),
      paste0("distributionbtn=", input$distributionbtn),
      paste0("detector=", detector(traprv$data)[1])
    )
    
    if (!is.null(input$trapfilename)) {
      parm <- c(parm,
                paste0("trapotherargs=", input$trapotherargs))
    }
    
    if (!is.null(captrv$data)) {
      parm <- c(parm,
                paste0("noccasions=", as.character(noccasions())))
    }
    
    if (!is.null(fitrv$value)) {
      parm <- c(parm,
                paste0("D=", as.character(signif(density(), input$dec))),
                paste0(detectrv$value, "=", as.character(signif(detect0(), input$dec))),
                paste0("sigma=", as.character(signif(sigma(), input$dec))))
    }
    
    parm <- paste(parm, collapse = "&")
    # open secrdesignapp in a new tab, with parameters from secrapp
    # designurl is set at top of this file
    
    if (input$detectfnbox %in% c('HHN','HEX') && 
        detector(traprv$data)[1] %in% c('multi','proximity','count')) {
      tags$a(href =  paste0(designurl, "?", parm), "Switch to secrdesign", target="_blank")  
    }
  }
})
#-----------------------------------------------------------------------------

output$persqkm <- renderUI({
  ## display density in animals/km^2
  Dkm <- density() * 100
  Dkmtext <- paste0(Dkm, '&nbsp; animals / km<sup>2</sup>')
  helpText(HTML(Dkmtext))
})
#-----------------------------------------------------------------------------

output$detectorhelp <- renderUI({
  helptext <- ""
  if (input$detector == 'proximity')
    helptext <- "binary proximity detector; max. one detection per animal per detector per occasion"
  else if (input$detector == 'multi')
    helptext <- "multi-catch trap; max. one detection per animal per occasion"
  else if (input$detector == 'count')
    helptext <- "count proximity detector; integer # detections per animal per occasion"
  else if (input$detector == 'single')
    helptext <- "single-catch trap; max. one detection per animal & one per trap on any occasion"
  else if (input$detector == 'polygon')
    helptext <- "searched polygons"
  else if (input$detector == 'polygonX')
    helptext <- "searched polygons; max. one detection per animal on any occasion"
  else if (input$detector == 'transect')
    helptext <- "searched transect"
  else if (input$detector == 'transectX')
    helptext <- "searched transect; max. one detection per animal on any occasion"
  helpText(HTML(helptext))
})
#-----------------------------------------------------------------------------

output$captfilehelp <- renderUI({
  helptext <- ""
  if (input$fmt == "trapID")
    helptext <- "Session, ID, Occasion, TrapID"
  else
    helptext <- "Session, ID, Occasion, X, Y"
  helpText(HTML(helptext))
})                                                                  
#-----------------------------------------------------------------------------

output$shapefile <- renderUI({
  helptext <- ""
  if (!is.null(input$regionfilename)) {
    pos <- grep(".shp", tolower(input$regionfilename[,1]))
    if (length(pos)>0)
      helptext <- paste0(input$regionfilename[pos,1])
    pos <- grep(".rda", tolower(input$regionfilename[,1]))  # .rda, .rdata
    if (length(pos)>0) {
      objlist <- load(input$regionfilename[1,4])
      helptext <- paste0(objlist[1])
    }
    pos <- grep(".rds", tolower(input$regionfilename[,1])) 
    if (length(pos)>0) {
      helptext <- paste0(input$regionfilename[pos,1])
    }
  }
  helpText(HTML(helptext))
})
#-----------------------------------------------------------------------------

output$habitatfile <- renderUI({
  helptext <- ""
  if (!is.null(polyrv$data)) {
    pos <- grep(".shp", tolower(input$maskpolyfilename[,1]))
    if (length(pos)>0)
      helptext <- paste0(input$maskpolyfilename[pos,1])
    pos <- grep(".rda", tolower(input$maskpolyfilename[,1]))  # .rda, .rdata
    if (length(pos)>0) {
      objlist <- load(input$maskpolyfilename[1,4])
      helptext <- paste0(objlist[1])
    }
    pos <- grep(".rds", tolower(input$maskpolyfilename[,1])) 
    if (length(pos)>0) {
      helptext <- paste0(input$maskpolyfilename[pos,1])
    }
  }
  helpText(HTML(helptext))
})
#-----------------------------------------------------------------------------

output$covariatefile <- renderUI({
  helptext <- ""
  if (!is.null(covariaterv$data)) {
    pos <- grep(".shp", tolower(input$maskcovariatefilename[,1]))
    if (length(pos)>0)
      helptext <- paste0(input$maskcovariatefilename[pos,1])
    pos <- grep(".rda", tolower(input$maskcovariatefilename[,1]))  # .rda, .rdata
    if (length(pos)>0) {
      objlist <- load(input$maskcovariatefilename[1,4])
      helptext <- paste0(objlist[1])
    }
    pos <- grep(".rds", tolower(input$maskcovariatefilename[,1])) 
    if (length(pos)>0) {
      helptext <- paste0(input$maskcovariatefilename[pos,1])
    }
    pos <- grep(".txt", tolower(input$maskcovariatefilename[,1])) 
    if (length(pos)>0) {
      helptext <- paste0(input$maskcovariatefilename[pos,1])
    }
  }
  helpText(HTML(helptext))
})
#-----------------------------------------------------------------------------

output$uigridlines <- renderUI({
  if(input$gridlines=="None")
    helpText("")
  else if (input$gridlines=="100")
    helpText(span(style="color:gray", HTML("100-m grid")))
  else 
    helpText(span(style="color:gray", HTML(paste0(round(as.numeric(input$gridlines)/1000,1), "-km grid"))))
})
#-----------------------------------------------------------------------------

output$uipopN <- renderUI({
  N <- nrow(pop())
  if (is.null(pop()) || !input$Dshowpopn) 
    helpText("") 
  else 
    helpText(HTML(paste0("N = ", N)))
})
#-----------------------------------------------------------------------------

output$xycoord <- renderUI({
  xy <- c(input$arrayHover$x, input$arrayHover$y)
  tmpgrid <- isolate(traprv$data)
  if (ms(tmpgrid)) tmpgrid <- tmpgrid[[input$sess]]   ## 2020-08-16
  tmpcapt <- capthist()
  if (ms(tmpcapt)) {
    tmpcapt <- tmpcapt[[input$sess]]
    tmpgrid <- traps(tmpcapt)
  }
  if (is.null(xy)) 
    helpText("")
  else {
    if (detector(tmpgrid)[1] %in% polygondetectors) {
      if (!is.null(tmpcapt)) {
        nearest <- nearesttrap(xy, xy(tmpcapt))
        ## updateNumericInput(session, "animal", value = animalID(tmpcapt, names=FALSE)[nearest])
        id <- paste0(animalID(tmpcapt)[nearest], ":")
      }
      else {
        nearest <- nearesttrap(xy, tmpgrid)
        id <- polyID(tmpgrid)[nearest]
      }
    }
    else {
      nearest <- nearesttrap(xy, tmpgrid)
      id <- paste0(rownames(tmpgrid)[nearest], ":")
      xy <- tmpgrid[nearest,]
    }
    # 2020-02-22 this is transient on capthist plot 
    #            as plot is redrawn when new animal selected
    helpText(HTML(paste(id, paste(round(xy), collapse = ", "))))
  }
})
##############################################################################

