
output$maskready <- reactive({
  return(!is.null(mask()))
})

output$maskcovariatefileready <- reactive({
  return(!is.null(covariaterv$data))
})

output$maskcovariatesready <- reactive({
  return(length(covariaterv$names>0))
})

output$maskpolygonsready <- reactive({
  return(!is.null(polyrv$data))
})

output$modelFitted <- reactive({
  return(!is.null(fitrv$value))
})

output$filterCapt <- reactive({
  return(filtercaptrv$value)
})

output$filterMask <- reactive({
  return(filtermaskrv$value)
})

output$capthistLoaded <- reactive({
  return(!is.null(capthist()))
})

output$usage <- reactive({
  return(!is.null(usage(traprv$data)) && is.null(capthist()))
})

## reactive

# capthist 
# density 
# predictresult 
# derivedresult 
# detectorarray
# invalidateOutputs 
# detect0 
# mask 
# masknrow 
# maskspace 
# n 
# ndetectors
# newdata 
# noccasions 
# nsessions 
# poly 
# pop
# r 
# RSE
# se.density 
# se.detect0 
# se.sigma 
# sigma 
# zw
# usagepct 

capthist <- reactive( {
  # showNotification("debug: capthist eval", closeButton = FALSE, type = "message", duration = 0.1)
  ch <- NULL
  covnames <- character(0)
  if ((is.null(traprv$data) || is.null(captrv$data)) && 
      is.null(importrv$data) &&
      is.null(secrrv$data)) {
    updateNumericInput(session, "animal", max = 0)
    ch <- NULL
  }
  else {
    if (!is.null(secrrv$data)) {
      ch <- secrrv$data
    }
    else if (!is.null(importrv$data)) {
      ch <- importrv$data
    }
    else {
      ch <- try(suppressWarnings(make.capthist(captrv$data, traprv$data, 
                                               fmt = input$fmt)))
      if (filtercaptrv$value && !input$filtercapttext=="") {
        subset <- input$filtercapttext
        numsubset <- as.numeric(subset)
        if (all(!is.na(numsubset))) {
          if (all(numsubset<0)) {
            subset <- paste0("!(1:nrow(ch)) %in% c(", paste0(-numsubset, collapse = ","), ")")
          }
          else if (any(numbset<0)) {
            showNotification("filter indices must be all positive or all negative",
                             id = "lastaction", duration = NULL)
          }
        }
        
        subsetcaptcall <- paste0("subset (ch,",subset, ")")
      }
    }
    if (inherits(ch, 'try-error')) {
      showNotification("invalid capture file or arguments; try again",
                       type = "error", id = "badcapt")
      ch <- NULL
    }
    else {
      if (ms(ch)) {
        ncov <- ncol(covariates(ch[[1]]))
        if (length(ncov)>0 && ncov>0) {
          covnames <- getcovnames(input$covnames, ncov, 'C', TRUE, FALSE)
        }
        if (length(ncov)>0 && ncov>0) {
          for (i in 1:length(ch)) names(covariates(ch[[i]])) <- covnames
        }
        nanimal <- nrow(ch[[input$sess]])
      }
      else {
        ncov <- ncol(covariates(ch))
        if (length(ncov)>0 && ncov>0) {
          covnames <- getcovnames(input$covnames, ncov, 'C', TRUE, FALSE)
        }
        if (length(ncov)>0 && ncov>0) {
          names(covariates(ch)) <- covnames
        }
        nanimal <- nrow(ch)
      }
      
      if (filtercaptrv$value && !input$filtercapttext=="") {
        ch <- try(eval(parse(text = subsetcaptcall)))
        if (inherits(ch, "try-error")) {
          ch <- NULL
        }
      }
      
      if (ms(ch)) 
        currentIDrv$value <- rownames(ch[[1]])[1]
      else 
        currentIDrv$value <- rownames(ch)[1]
      updateNumericInput(session, "animal", max = nanimal)
      output$multisession <- renderText(tolower(ms(ch)))
      updateNumericInput(session, "sess", max = length(ch))
      updateNumericInput(session, "masksess", value = 1, max = length(ch))
      updateSelectInput(session, "hcovbox", choices = c("none", covnames))
    }
  }
  if (is.null(ch)) {
    # added 2020-02-22; weak as some other changes can invalidate model
    fitrv$value <- NULL  
    disable("fitbtn")
  }
  else {
    enable("fitbtn")
  }
  ch
})
##############################################################################

density <- reactive( {
  if (!inherits(fitrv$value, "secr")) {
    NA
  }
  else {
    if (input$likelihoodbtn == "Full") {
      D <- predictparm('D', 'estimate')
    }
    else {
      if (nsessions()==1)
        D <- derivedresult()['D', 'estimate']
      else
        D <- derivedresult()[[input$sess]]['D', 'estimate']
      
    }
    D
  }
})
##############################################################################

setdetectrv <- reactive({
  if (input$detectfnbox %in% hazarddetectfn) {
    detectrv$value <- 'lambda0'
  }
  else {
    detectrv$value <- 'g0'
  }
})
##############################################################################

predictresult <- reactive({
  if (inherits(fitrv$value, 'secr')) {
    pred <- predict(fitrv$value, all.levels = TRUE)
    roundpr <- function (pr) {pr[,-1] <- signif(pr[,-1], input$dec); pr}
    if (is.data.frame(pred))
      pred <- roundpr(pred)
    else 
      pred <- lapply(pred, roundpr)
    pred
  }
  else NULL
})
##############################################################################

derivedresult <- reactive({
  # progress <- Progress$new(session, min = 1, max = 15)
  # on.exit(progress$close())
  # progress$set(message = 'Computing derived estimates ...', detail = '')
  if (inherits(fitrv$value, 'secr') && (!any(is.na(coef(fitrv$value)[,'beta'])))) {
    showNotification("Computing derived estimates ...",
                     id = "derived", duration = seconds)
    
    der <- derived(fitrv$value, distribution = tolower(input$distributionbtn),
                   se.esa = TRUE)
    if (ms(capthist()))
      lapply(der, signif, input$dec)
    else 
      signif(der, input$dec)
  }
  else NULL
})
##############################################################################

invalidateOutputs <- reactive({
  pxyrv$value <- NULL
  Drv$value <- NULL
  # updateNumericInput(session, "D", step = 10^trunc(log10(density()/50)))
})

##############################################################################

predictparm <- function(parm = 'D', stat = 'estimate') {
  if (inherits(fitrv$value, "secr")) {
    pred <- predict(fitrv$value, newdata = newdata())
    if (is.data.frame(pred)) {
      pred[parm, stat]
    }
    else {
      pred[[1]][parm,stat]
    }
  }
  else NA
}
##############################################################################

detect0 <- reactive({
  if (detectrv$value == 'lambda0')
    predictparm (parm = 'lambda0', stat = 'estimate') 
  else 
    predictparm (parm = 'g0', stat = 'estimate') 
})

sigma <- reactive ({
  predictparm (parm = 'sigma', stat = 'estimate') 
})

zw <- reactive ({
  predictparm (parm = 'z', stat = 'estimate') 
})

##############################################################################

maskOK <- function () {
  if (is.null(polyrv$data) || is.null(traprv$data)) {
    TRUE
  }
  else {
    if (ms(traprv$data)) {
      all(sapply(traprv$data, function(x) sum(pointsInPolygon(x, polyrv$data)) > 0))
    }
    else {
      sum(pointsInPolygon(traprv$data, polyrv$data)) > 0
    }
  }
}

mask <- reactive( {
  pxyrv$value <- NULL
  Drv$value <- NULL
  if (input$masktype=="Build") {
    
    if (is.null(traprv$data)) {
      return(NULL)
    }
    else {
      if (!maskOK()) {
        showNotification("no detectors in habitat polygon(s)",
                         type = "warning", id = "notrapsinpoly",
                         duration = seconds)
      }
      msk <- make.mask (traprv$data,
                        buffer = input$buffer,
                        nx = input$habnx,
                        type = if (input$maskshapebtn=='Rectangular') 'traprect' else 'trapbuffer',
                        poly = polyrv$data,
                        poly.habitat = input$includeexcludebtn == "Include",
                        keep.poly = FALSE)
      if (ms(msk) && !is.null(capthist())) names(msk) <- names(capthist())
      nrw <- if (ms(msk)) nrow(msk[[1]]) else nrow(msk)
      if (nrw > 10000) {
        showNotification(paste0(nrw, " mask points is excessive; reduce buffer or nx?"),
                         type = "warning", id = "maskrows", duration = NULL)
      }
      else if (nrw < 500) {
        showNotification(paste0("only ", nrw, " mask points; increase buffer or nx?"),
                         type = "warning", id = "maskrows", duration = NULL)
      }
      else {
        removeNotification(id = "maskrows")
      }
    }
    # covariaterv$names <- character(0)
  }
  else {
    msk <- maskrv$data
  }
  if (!is.null(covariaterv$data)) {
    removeNotification(id = "lastaction")
    msk <- addCovariates (msk, covariaterv$data)
    covariates(msk) <- secr:::stringsAsFactors(covariates(msk))
    
    OK <- apply(!is.na(covariates(msk)),1,all)
    if (sum(OK)<nrow(msk)) {
      if (input$dropmissing) {
        msk <- subset(msk, OK)
      }
      else {
        showNotification("covariate(s) missing at some mask points",
                         id = "lastaction", duration = NULL)
      }
    }
    if (!is.null(covariates(msk))) {
      if (input$filtermasktext != "" && filtermaskrv$value) {
        OKF <- try(with (covariates(msk), {
          eval(parse(text = input$filtermasktext))
        }), silent = TRUE)
        if (!inherits(OKF, "try-error")) {
          msk <- subset (msk, OKF)
        }
      }
    }
  }
  # check spacing 2020-09-02
  if (!is.null(capthist())) {
    rpsv <- unlist(RPSV(capthist(), CC = TRUE))[input$sess]
    sp <- unlist(spacing(msk))[1] # custom for this session
    if (!is.null(sp) && sp > rpsv) {    # check for NULL 2021-06-20
      showNotification(id = "lastaction", type = "warning", duration = NULL,
                       paste0("mask spacing ", signif(sp,3), " exceeds naive sigma ", signif(rpsv,3)))
    }
  }
  msk
}
)
##############################################################################

masknrow <- reactive ({
  msk <- mask()
  if (is.null(msk))
    NA
  else if (ms(msk))
    sapply(msk, nrow)
  else
    nrow(msk)
})
##############################################################################

maskspace <- reactive ({
  msk <- mask()
  if (is.null(msk))
    NA
  else if (ms(msk))
    sapply(msk, spacing)
  else
    spacing(msk)
})
##############################################################################

movements <- reactive( {
  req(capthist())
  if (ms(capthist())) {
    if (input$movesallbox) {
      ch <- capthist()
      if (input$withinsessiononly) {
        for (i in 1:length(ch)) rownames(ch[[i]]) <- paste(rownames(ch[[i]]), i, sep='.')
      }
      ch <- join(ch)
    }
    else
      ch <- capthist()[[input$sess]]
  }
  else
    ch <- capthist()
  
  captperoccasion <- table(apply(ch != 0, c(1,2), sum))
  ncap <- as.numeric(names(captperoccasion))
  withinoccasionrecap <- 0
  if (any (ncap>1)) {
    withinoccasionrecap <- sum(captperoccasion[ncap>1] * (ncap[ncap>1]-1))
  }
  
  if (! all(detector(traps(ch)) %in% c('single','multi', 'polygonX', 'transectX'))
      && withinoccasionrecap>0) {
    output$nontrap <- renderText("true")
  }
  moves(ch, names = TRUE)
})
##############################################################################

n <- reactive ({
  if (is.null(capthist()))
    NA
  else if (ms(capthist()))
    sum(sapply(capthist(), nrow))
  else
    nrow(capthist())
})
##############################################################################

ndetectors <- reactive ({
  if (is.null(traprv$data))
    NA
  else if (is.null(capthist()))
    nrow(traprv$data)
  else if (ms(capthist()))
    sapply(capthist(), function(x) dim(x)[3])
  else
    dim(capthist())[3]
  
})
##############################################################################

newdata <- reactive({
  if (inherits(fitrv$value, "secr")) {
    makeNewData(fitrv$value, all.levels = TRUE)
  }
  else NULL  ## no model
})
##############################################################################

noccasions <- reactive( {
  if (is.null(capthist()))
    NA
  else if (ms(capthist()))
    sapply(capthist(), ncol)
  else
    ncol(capthist())
})
##############################################################################

nsessions <- reactive({
  ## used to control conditional display of session index input
  if (is.null(capthist())) {
    NULL
  }
  else {    
    if (ms(capthist())) {
      length(capthist())
    }
    else {
      
      1
    }
  }
})
##############################################################################

pop <- reactive(
  {
    poprv$v
    input$Dshowpopn
    removeNotification("bigpop")
    core <- if (ms(traprv$data)) traprv$data[input$sess] else traprv$data
    if (is.null(core) || (density() == 0) || is.na(density())) {
      return (NULL)
    }
    if (density() * maskarea(mask()) > 10000) {
      showNotification("population exceeds 10000; try again",
                       type = "error", id = "bigpop")
      return(NULL)
    }
    Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
    
    dsurf <- if(ms(fitrv$dsurf)) fitrv$dsurf[[input$sess]] else fitrv$dsurf
    sim.popn (D = 'D.0', core = dsurf, model2D = "IHP", Ndist = Ndist)  
    
  }
)
##############################################################################

r <- reactive ({
  if (is.null(capthist()))
    NA
  else if (ms(capthist()))
    sum(sapply(capthist(), sum)) - n()
  else
    sum(capthist()) - n()
})
##############################################################################

RSE <- reactive ({
  if (inherits(fitrv$value, "secr") && (input$likelihoodbtn == "Full") && 
      !is.null(fitrv$value$beta.vcv)) {
    V <- vcov(fitrv$value)['D','D']   ## FAILS IF HAVE JUST SWITCHED BUTTON
    sqrt(exp(V)-1) * 100
  }
  else NULL
})
##############################################################################

se.density <- reactive( {
  if (inherits(fitrv$value, "secr")) {
    if (input$likelihoodbtn == "Full") {
      se.D <- predictparm('D', 'SE.estimate')
    }
    else {
      if (nsessions()==1)
        se.D <- derivedresult()['D', 'SE.estimate']
      else
        se.D <- derivedresult()[[input$sess]]['D', 'SE.estimate']
      
      
    }
    se.D
  }
  else NA
})
##############################################################################

se.detect0 <- reactive({
  req(detectrv$value)
  if (detectrv$value == 'lambda0')
    predictparm('lambda0','SE.estimate')
  else 
    predictparm('g0','SE.estimate')
})
##############################################################################

se.sigma <- reactive ({
  predictparm('sigma','SE.estimate')
})
##############################################################################

se.zw <- reactive ({
  predictparm('z','SE.estimate')
})
##############################################################################

usagepct <- reactive ({
  trps <- traprv$data
  uprob <- function(x) if (is.null(usage(x))) 1 else sum(usage(x) / length(usage(x)))
  if (is.null(trps))
    NA
  else if (ms(trps))
    100*sapply(trps, uprob)
  else
    100*uprob(trps)
})
##############################################################################
