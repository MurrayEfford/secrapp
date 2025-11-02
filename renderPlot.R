## renderPlot

## arrayPlot
## movesHist  
## detnPlot
## esaPlot
## pxyPlot
## DPlot
## powerPlot

##############################################################################

# 2020-08-14 
# increasing height from default 400 does not work because 
# other elements for conditionalPanel("output.capthistLoaded", ... remain fixed in position
# (i.e. placed assuming original tabPanel height)
# and plot does not centre

# this applies even if the new height is hardwired 
# e.g.  renderPlot(height = 500,

# plotheightfn <- function () {
#   if (input$resultsbtn == "hide") 600 else 400
# }
# 
# plotwidthfn <- function () {
#   if (input$resultsbtn == "hide") 600 else 400
# }
# 
# output$arrayPlot <- renderPlot(height = plotheightfn,
#   width = plotwidthfn, {

output$arrayPlot <- renderPlot( {
  
  removeNotification("error")
  boxtype <- if (input$arrayframe) "o" else "n"
  border <- max(input$arrayborder,0)
  par(mar = c(1,1,1,1), cex = 1.3, xpd = TRUE)
  
  if (inherits(capthist(), 'capthist')) {
    if (ms(capthist()))
      ch <- capthist()[[input$sess]]
    else
      ch <- capthist()
    add <- !is.null(mask()) && input$entireregionbox
    if (add) plot(mask(), col = 'grey97', dots = FALSE)
    plot (traps(ch), add = add, border = border, bty = boxtype, 
          xaxs = 'i', yaxs = 'i', gridlines = (input$gridlines != "None"), 
          detpar = list(cex = input$cex), gridspace = as.numeric(input$gridlines))
    plot(ch, varycol = input$varycol, rad = input$rad, cappar = list(cex = input$cex), 
         tracks = input$tracks, add = TRUE, 
         title = "", subtitle = "")
    if (nsessions()>1)
      mtext(side=3, line = 1, paste0("Session : ", session(capthist())[input$sess]), col = 'blue')
    if (!is.na(input$animal) && (input$animal>0)) {
      tracksi <- TRUE
      chi <- suppressWarnings(subset(ch, input$animal))
      selectcol1 <- list(pch = 16, col = 'yellow', cex = 2.5, lwd = 1)
      selectcol2 <- list(pch = 1, col = 'black', cex = 2.5, lwd = 1)
      plot(chi, tracks = tracksi, add = TRUE, varycol = FALSE, rad = input$rad, detpar = list(cex = input$cex), 
           cappar = selectcol1, trkpar=list(col='yellow', lwd = 3),
           title = "", subtitle = "")
      plot(chi, tracks = tracksi, add = TRUE, varycol = FALSE, rad = input$rad, detpar = list(cex = input$cex), 
           cappar = selectcol2, trkpar=list(col='black', lwd = 1),
           title = "", subtitle = "")
    }
    if (!is.null(fitrv$value) && input$fxi) {
      if (!(input$detector %in% c("multi","proximity","count")))
        showNotification("fxiContour requires point detector type", 
                         id = "warning", type = "warning", duration = warningseconds)
      else {
        if (input$animal>0) {
          tmp <- log_and_run(
            expr = fxiContour(fitrv$value, i = input$animal, 
                              sessnum = input$sess, border = input$buffer, add = TRUE), 
            fxicode())
        }
        else {
          # all
          tmp <- log_and_run(
            expr = fxiContour(fitrv$value, i = NULL, 
                                sessnum = input$sess,  border = input$buffer, add = TRUE), 
            fxicode())
        }
        if (is.null(tmp)) {
          showNotification("error in fxiContour; consider smaller mask spacing",
                           id = "error", type = "error", duration = errorseconds)
        }
      }
    }
    
  }
  else {
    if (ms(traprv$data))
      tmpgrid <- traprv$data[[input$sess]]
    else 
      tmpgrid <- traprv$data
    if (is.null(tmpgrid)) return (NULL)
    
    add <- !is.null(mask()) && input$entireregionbox
    if (add) plot(mask(), col = 'grey97', dots = FALSE)
    plot (tmpgrid, add = add, 
          border = border, 
          bty = boxtype, 
          xaxs = 'i', yaxs = 'i', 
          detpar = list(cex = input$cex), 
          gridlines = (input$gridlines != "None"), 
          gridspace = as.numeric(input$gridlines))
    usge <- usage(tmpgrid)
    if (!is.null(usge) && input$usageplot) {
      scale <- 1 / max(usge, na.rm = TRUE)
      usagePlot(tmpgrid, add = TRUE, fill = FALSE, metres = FALSE, 
                scale = scale, rad = input$rad)
      occasionKey(noccasions = ncol(usge), rad = input$rad * 1.3, 
                  title = "", cex = 0.7)
    }
  }
})
##############################################################################

output$detnPlot <- renderPlot( height = 290, width = 400, {
  invalidateOutputs()
  usez <- input$detectfnbox %in% c('HR', 'CLN','CG','HHR','HCG','HVP')
  np <- if(usez) 3 else 2
  pars <- c(detect0(), sigma(), zw())[1:np]
  if (any(is.na(pars))) {
    return (NULL)
  }
  else {
    par(mar=c(4,5,2,5))
    detectfnplot (detectfn = input$detectfnbox,
                  pars = pars,
                  xval = seq(0, 3 * sigma(), sigma()/40),
                  ylab = "",
                  hazard = detectrv$value == 'lambda0',     # changed from TRUE 2020-08-13      
                  ylim = c(0, detect0()*1.2),
                  las=1, col = 'red', lwd = linewidth,
                  xaxs='i', yaxs='i')
    if (detectrv$value == 'g0') 
      mtext(side = 2, line = 3.7, "Detection probability   g")
    else
      mtext(side = 2, line = 3.7, expression(paste("Detection hazard   ", lambda)))
    
    if (detect0() <= 0.7) 
      p <- seq(0,1,0.05)
    else 
      p <- seq(0,1,0.1)
    
    if (detectrv$value == 'g0') {
      axis(4, at = 1-exp(-p), label = p, xpd = FALSE, las = 1)
      mtext(side = 4, line = 3.7, expression(paste("Detection hazard   ", lambda)))
    }
    else {
      axis(4, at = -log(1 - p), label = p, xpd = FALSE, las = 1)
      mtext(side = 4, line = 3.7, "Detection probability    g")
    }
  }
})
##############################################################################

output$movesHist <- renderPlot( {
  invalidateOutputs()
  m <- unlist(movements())
  if (!is.null(m)) {
    if (input$nbar == 0) 
      nbar <- length(m) %/% 10
    else
      nbar <- input$nbar
    par(mar = c(3.2,4,3,2), mgp = c(2.1,0.6,0))  # reduce margins
    hist(m, breaks = nbar, xlab = "Movement  m", main = "")
  }
})
##############################################################################

output$esaPlot <- renderPlot( height = 290, width = 400, {
  invalidateOutputs()
  req(fitrv$value)
  par(mar=c(4,5,2,5))
  if (input$masktype == "Build") {
    spscale <- secr:::secr_spatialscale(fitrv$value, detectfn = input$detectfnbox, 
                                   sessnum = input$sess)
    max.buffer <- max(input$buffer*1.5, 5*spscale)  # 2020-08-13 *1.5
    esaPlot(fitrv$value, session = input$sess, max.buffer = max.buffer, thin = 1)
    abline(v = input$buffer, col = "red")
  }
  else {
    esaPlot(fitrv$value, session = input$sess)
  }
  mtext("Density estimate", side = 4, line = 1.5)
})
##############################################################################

output$maskPlot <- renderPlot({
  
  plotmsk <- function (add) {
    if (is.null(input$maskcov) | (input$maskcov == "none")) {
      plot (msk, add = add, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox,
            border = input$buffer*input$maskborder, covariate = NULL)
    }
    else {
      # debug cat(input$maskcov, "\n")
      nacovariate <- is.na(covariates(msk)[,input$maskcov])
      if (any(nacovariate)) {
        plot (subset(msk, !nacovariate), add = add, dots = input$dotsbox, border = input$buffer*input$maskborder,
              covariate = input$maskcov, legend = input$legend, inset = 0)
        plot (subset(msk, nacovariate), add = TRUE, dots = input$dotsbox, 
              col = 'red')
      }
      else {
        plot (msk, add = add, dots = input$dotsbox, border = input$buffer*input$maskborder,
              covariate = input$maskcov, legend = input$legend, inset = 0)
      }
    }
  }
  core <- traprv$data
  border <- input$buffer*input$maskborder
  msk <- mask()
  if (ms(core)) {
    core <- core[[input$masksess]]
    # 2025-10-29 assume read mask is for a single session
    if (input$masktype == "Build") {  
      msk <- msk[[input$masksess]]
    }
  }
  
  par(mar=c(2,1,2,5), xaxs='i', yaxs='i', xpd = !input$frame)
  if (input$masktype == "Build") {
    if (is.null(core)) return (NULL)
    plot (core, border = border, gridlines = FALSE)
    plotmsk(add = TRUE)
    plot (core, add = TRUE)
    if (!is.null(polyrv$data) && input$showpoly) {
      plot(polyrv$data, add = TRUE)
    }
  }
  
  else {
    if (!is.null(msk)) {
      plotmsk(add = FALSE)
      if (inherits(core, 'traps')) plot (core, add = TRUE)
    }
  }    
  if (!is.null(msk)) {
    if (input$maskedge2) {
      plotMaskEdge(msk, add = TRUE)
    }
    if (input$frame)
      box()
  }
  
  
})
##############################################################################

border <- function (multiple) {
  if (detector(traprv$data)[1] %in% c('polygon','polygonX','transect','transectX')) {
    spc <- max(diff(range(traprv$data$x)), diff(range(traprv$data$y)))/10
  }
  else {
    spc <- spacing(traprv$data)[1] 
  }
  if (is.null(spc) || is.na(spc)) spc <- sigma()
  multiple * spc
}

output$pxyPlot <- renderPlot({
  if (ms(traprv$data))
    core <- traprv$data[[input$sess]]
  else
    core <- traprv$data
  
  if (is.null(core)) return (NULL)
  invalidateOutputs()
  
  if (input$pxyfillbox) {
    cols <- terrain.colors(11)
    col <- cols[1]
    lev <- c(0.01, seq(0.1, 0.9, 0.1))
    par(mar=c(1,1,1,5)) # , xaxs='i', yaxs='i')
  }
  else {
    col <- "blue"
    cols <- NULL
    lev <- seq(0.1, 0.9, 0.1)
    par(mar=c(1,3,1,3)) # , xaxs='i', yaxs='i')
  }
  
  border <- border(input$pxyborder)
  plot(core, border = border, gridlines = FALSE, hidetr = TRUE)
  
  xr <- range(core[,1]) + c(-1,1) * border
  yr <- range(core[,2]) + c(-1,1) * border
  
  if (input$pxyfillbox) {
    # clunky way to give green background
    rect(xr[1],yr[1],xr[2],yr[2], col = cols[1], border = NA)
    drawlabels <- FALSE
  }
  else
    drawlabels <- input$pxylabelbox
  detectparlist <- list(detect0(), sigma(), zw())
  names(detectparlist) <- c(detectrv$value, 'sigma', 'z')
  pdotContour(core, border = border, nx = input$pxynx,
               detectfn = input$detectfnbox,
               detectpar = detectparlist,
               noccasions = noccasions()[input$sess], drawlabels = drawlabels,
               binomN = 0, 
               levels = lev, 
               poly = polyrv$data, 
               poly.habitat = input$includeexcludebtn == "Include",
               plt = TRUE, add = TRUE,
               col = col, fill = cols)
  plot (core, add = TRUE)
  if (!is.null(pxyrv$value)) {
    xy <- pxyrv$xy
    points(xy[1], xy[2], pch=16, cex=0.7)
    offset <- (par()$usr[2] - par()$usr[1])/15
    text(xy[1]+offset, xy[2], round(pxyrv$value,3), cex = 0.9, xpd = TRUE)
  }
  if (input$pxyframebox) {
    rect(xr[1],yr[1],xr[2],yr[2], col = NA, border = "black") 
  }
  if (input$pxyfillbox) {
    strip.legend("right", legend = c(0,lev[1:10],1), title = "p.(x)", xpd = TRUE,
                 legendtype='breaks', inset = 0.01, col = cols[1:11])
  }
  if (input$maskedge) {
    plotMaskEdge(mask(), add = TRUE)
  }
})
##############################################################################

output$DPlot <- renderPlot( {
  invalidateOutputs()
  par(mar=c(1,1,1,5))
  if (input$likelihoodbtn!='CL' && !is.null(fitrv$value)) {
    col <- tryCatch(
      expr = eval(parse(text = input$Dxycol)), 
      error = function(e) return(NULL),
      silent = TRUE)
    plot (fitrv$dsurf, border = 0, scale = 1, title="D(x)", col = col)
    if (input$Dshowdetectors) {
      plot(traps(capthist()), add = TRUE)
    }
    if (input$Dshowdetections) {
      plot(capthist(), varycol = FALSE, add = TRUE)
    }
    if (!is.null(Drv$value)) {
      xy <- Drv$xy
      points(xy[1], xy[2], pch=16, cex=0.7)
      offset <- (par()$usr[2] - par()$usr[1])/15
      text(xy[1]+offset, xy[2], signif(Drv$value,3), cex = 0.9, xpd = TRUE)
    }
    
    if (input$Dmaskedge) {
      plotMaskEdge(mask(), add = TRUE)
    }
    
    if (input$Dshowpopn) {
      tmppop <- pop()
      n <- if (is.null(tmppop)) 0 else nrow(tmppop)
      if (n>0) {
        plot(tmppop, add = TRUE, pch = 16, cex = 0.7, xpd = TRUE, frame = FALSE)
        if (input$showHRbox) {
          rad <- secr::circular.r(p = 0.95, detectfn = input$detectfnbox, sigma = sigma())
          col <- input$showHRcol
          symbols(tmppop$x, tmppop$y, circles = rep(rad, n),
                  inches = FALSE, fg = col, add = TRUE, xpd = FALSE)
        }
      }
    }
    
  }
})
##############################################################################

output$powerPlot <- renderPlot( height = 320, width = 360, {
  RSE <- input$RSEslider/100
  if (input$powertype) {    ## confidence interval
    par(mar=c(4,4,2,2), mgp=c(2.4,0.7,0))
    headroom <- (input$maxEffect-input$minEffect)/4
    powLU <- plotpowerCI(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                         estimatedRange = c(input$minEffect, input$maxEffect+headroom),
                         adjustRSE = input$adjustRSEbox, alpha = input$alpha)
    x <- input$xpos 
    y1 <- approx(x = as.numeric(dimnames(powLU$limits)[[1]]), y = powLU$limits[,1,1], xout = x)$y*100-100
    y2 <- approx(x = as.numeric(dimnames(powLU$limits)[[1]]), y = powLU$limits[,2,1], xout = x)$y*100-100
    segments(x, y1, x, y2, lwd= linewidth, col = "blue")
    text(rep(x,2)+5, c(y1+1, min(y2+1, par()$usr[4]*1.06)), round(c(y1, y2)), adj = 0, cex = 0.9, col = "blue", xpd = TRUE)
  }
  else {
    par(mar=c(4,4,3,1))
    powLU <- plotpower(RSE = RSE, effectRange=c(input$minEffect, input$maxEffect),
                       adjustRSE = input$adjustRSEbox, alpha = input$alpha,
                       testtype = input$testtype,
                       targetpower = input$target)
  }
})
##############################################################################
