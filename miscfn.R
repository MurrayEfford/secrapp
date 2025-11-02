## miscellaneous functions

## areastr    format area with units
## getcovnames
## lengthstr
## modelstring
## plotpower    
## plotpowerCI    
## preplus
## readpolygon
## shpfile       logical
## timefn

##############################################################################

areastr <- function (area) {
  if (area<1000) 
    dec <- 2
  else 
    dec <- 0
  if (isolate(input$areaunit) == "ha") {
    paste0(round(area, dec), " ha")
  }
  else {
    paste0(round(area/100, dec), " km^2")
  }
}
##############################################################################

getcovnames <- function (cov, ncov, prefix = 'X', quote = FALSE, character = TRUE) {
  if (ncov <= 0) {
    NULL
  }
  else {
    covnames <- paste0(prefix, 1:ncov)   # default
    if (cov != "") {
      cov <- gsub("(')|(\")", "", cov)   # remove quotes
      cov <- gsub(",", " ", cov)         # comma to space
      nam <- strsplit(stringr::str_squish(cov), '[ ,]')
      if (length(nam[[1]]) >= ncov) {
        covnames <- nam[[1]][1:ncov]
      }
    }
    if (character) {
      if (quote) {
        covnames <- paste0("'", covnames, "'")
      }
      if (length(covnames)>1) {
        covnames <- paste(covnames, collapse = ",")
        covnames <- paste0("c(", covnames, ")")
      }
    }
    # otherwise just return character vector
  }
  covnames
}
##############################################################################


lengthstr <- function (length, dec) {
  a.unit <- isolate(input$areaunit)
  lth <- if (a.unit == "ha") length else length/1000
  
  if (missing(dec)) {
    if (lth<1000) 
      dec <- 2
    else 
      dec <- 0
  }
  if (a.unit == "ha") {
    paste0(round(lth, dec), " m")
  }
  else {
    paste0(round(lth, dec), " km")
  }
}
##############################################################################

modelstring <- function () {
  # deal with internal comma in e.g. D~s(x,k=6)
  form <- strsplit(input$model, ",")[[1]]
  form <- stringr::str_trim(form)
  brokenL <- rev(which(grepl("(", form, fixed=TRUE)))
  brokenR <- rev(which(grepl(")", form, fixed=TRUE)))
  nbk <- length(brokenL)
  if (nbk>0) {
    for (i in 1:nbk) {
      left <- brokenL[i]
      right <- brokenR[i]
      form <- c(form[c(max(0,left-2), left-1)], 
                paste0(form[left:right], collapse = ','), 
                form[(right+1):length(form)])
    }
  }
  fn <- function(f) {
    chf <- tryCatch(parse(text = f), error = function(e) NULL)
    model <- eval(f)
    if (is.null(model))
      return ("")
    else {
      chf <- as.character(eval(chf))
      if (chf[3]=="1") "" else f
    }
  }
  out <- sapply(form, fn)
  out <- out[out != ""]
  if (length(out)==0) "~1" else paste0(out, collapse = ", ")
}
##############################################################################

plotpower <- function (RSE = 0.2, effectRange = c(-99,150), testtype = "two.sided",
                       effectIncr = 2, adjustRSE = FALSE, alpha = 0.05,
                       targetpower = 80, col = topo.colors(8)[2], add = FALSE, ...) {
  
  power <- function (D2D1, RSE, adjustRSE, testtype) {
    if (adjustRSE) {
      sdiff <- (log(1 + RSE^2) + log(1 + RSE^2 / D2D1))^0.5
      effect <- log(D2D1) - log(sqrt(1 + RSE^2 / D2D1)) + log(sqrt(1 + RSE^2))
    }
    else {
      sdiff <- (2 * log(1 + RSE^2))^0.5
      effect <- log(D2D1)
    }
    effect <- effect / sdiff
    if (testtype == "two.sided") {
      z.alpha <- qnorm(1-alpha/2)
      pnorm(effect - z.alpha, 0, 1, lower.tail = TRUE) +
        pnorm(-effect - z.alpha, 0, 1, lower.tail = TRUE)
    }
    else if (testtype == "decrease") {
      z.alpha <- qnorm(1-alpha)
      pnorm(-effect - z.alpha, 0, 1, lower.tail = TRUE)
    }
    else {
      z.alpha <- qnorm(1-alpha)
      pnorm(effect - z.alpha, 0, 1, lower.tail = TRUE)
    }
  }
  
  if (!add) {
    xlim <- effectRange
    if (xlim[1] < -98) xlim[1] <- -100
    plot(0,0,type='n', xlim = xlim, ylim=c(0,100), yaxs='i', xaxs = 'i',
         xlab = "", ylab = 'Power %', las=1)
    mtext(side=1, line=2.5, 'Population change %')
  }
  xval <- seq(effectRange[1], effectRange[2], effectIncr)
  ## get critical values
  zero <- which.min(abs(xval))
  dpower <- function (x, target = targetpower) {
    100 * power(x/100+1, RSE, adjustRSE, testtype) - target
  }
  if (100*power(xval[1]/100+1, RSE, adjustRSE, testtype) >= targetpower) {
    lower <- uniroot(dpower, interval = xval[c(1,zero)])$root
    polygon (c(-100,-100,lower, lower), c(0,100,100, 0), col = 'lightgreen')
    text (lower, 105, as.character(round(lower, 1)), cex=0.8, xpd = TRUE)
  }
  else lower <- NA
  if (100*power(xval[length(xval)]/100+1, RSE, adjustRSE, testtype) >= targetpower) {
    upper <- uniroot(dpower, interval = xval[c(zero, length(xval))])$root
    polygon (c(upper, upper, effectRange[2], effectRange[2]), c(0,100,100, 0), col = 'lightgreen')
    text (upper, 105, as.character(round(upper, 1)), cex=0.8, xpd = TRUE)
  }
  else upper <- NA
  
  ## text(x = (par()$usr[3]- par()$usr[1])*0.9, y = 7, testtype, cex=0.9)
  
  powerpct <- 100*power(xval/100+1, RSE = RSE, adjustRSE, testtype)
  lines (xval, powerpct, col = col, lwd = linewidth)
  abline(h = targetpower, lty = 2, xpd = FALSE)
  box()
  list(lower=lower, upper = upper)
}
##############################################################################
preplus <- function(x) paste0(symnum(x, c(-Inf, 0, Inf), c("", "+")), x)

plotpowerCI <- function (RSE = seq(0.05,0.25,0.05), effectRange = c(-99,150), 
                         estimatedRange = effectRange, adjustRSE = FALSE, 
                         alpha = 0.05, effectincr = 2, col = topo.colors(8), plt = TRUE, 
                         add = FALSE, ...) {
  
  powerCI <- function (D2D1, RSE, adjustRSE, alpha) {
    ## effect D2D1 is ratio of final and initial density estimates
    ## RSE is relative standard error of initial density estimate
    ##
    ## find SD and mean of effect size on log scale
    if (adjustRSE) {
      sdiff <- (log(1 + RSE^2) + log(1 + RSE^2 / D2D1))^0.5
      effect <- log(D2D1) - log(sqrt(1 + RSE^2 / D2D1)) + log(sqrt(1 + RSE^2))
    }
    else {
      sdiff <- (2 * log(1 + RSE^2))^0.5
      effect <- log(D2D1)
    }
    ## return back-transformed Wald interval for effect on log scale
    z.alpha <- qnorm(c(alpha/2, 1-alpha/2))
    exp(effect + sdiff * z.alpha)
  }
  
  if (!add) {
    
    xlim <- effectRange
    if (xlim[1] < -98) xlim[1] <- -100
    ylim <- xlim * c(1,1.5)
    plot(0,0,type='n', xlim = xlim, ylim = ylim, yaxs='i', xaxs = 'i',
         xlab = "", ylab = "", las=1)
    mtext (side=1, line=2.5, 'Population change %')
    mtext (side=2, line=3, 'Estimated population change %')
    abline(v=0, lty=2)
    abline(h=0, lty=2)
    abline(0,1, lty=2, col='blue')
    box()
  }
  xval <- seq(effectRange[1], effectRange[2], effectincr)
  nRSE <- length(RSE)
  ci <- array(dim=c(length(xval), 2, nRSE), dimnames = list(xval, c('lower','upper'),RSE))
  for (i in 1:nRSE) {
    ci[,,i] <- t(sapply(xval/100+1, powerCI, RSE = RSE[i], adjustRSE = adjustRSE, alpha = alpha))
    lines(xval, 100*(ci[,1,i]-1), col = col[i], ...)
    lines(xval, 100*(ci[,2,i]-1), col = col[i], ...)
  }
  
  list(RSE = RSE, effectRange = effectRange, adjustRSE = adjustRSE,
       alpha = alpha, limits = ci)
}
##############################################################################

readshapefile <- function (fileupload) {
  poly <- NULL
  
  if (!is.null(fileupload) & is.data.frame(fileupload))
  {
    if (!file.exists(fileupload[1,4])) {
      return(NULL)   ## protect against bad shapefile
    }
    if (!(any(grepl(".shp", fileupload[,1])) &&
          any(grepl(".dbf", fileupload[,1])) &&
          any(grepl(".shx", fileupload[,1])))) {
      showNotification("need shapefile components .shp, .dbf, .shx",
                       type = "error", id = "invalidinput", duration = invalidseconds)
    }
    else  if (!requireNamespace("sf"))
      showNotification("need package sf to read shapefile", 
                       type = "error", id = "error", duration = errorseconds)
    else {
      removeNotification(id = "invalidinput")
      removeNotification(id = "error")
      ## not working on restore bookmark 2019-01-24
      dsnname <- dirname(fileupload[1,4])
      ## make temp copy with uniform layername
      file.copy(from = fileupload[,4], 
                to = paste0(dsnname, "/temp.", tools::file_ext(fileupload[,4])),     
                overwrite = TRUE)
      poly <- sf::st_read(dsn = paste0(dsnname, '/temp.shp')) 
      
    }  
  }
  poly
}

## patched in revised version from secrdesignapp 2019-02-11    
readpolygon <- function (fileupload) {
  poly <- NULL
  if (!is.null(fileupload) & is.data.frame(fileupload))
  {
    if (!file.exists(fileupload[1,4])) {
      return(NULL)   ## protect against bad shapefile
    }
    ext <- tolower(tools::file_ext(fileupload[1,1]))
    if (ext == "txt") {
      coord <- read.table(fileupload[1,4])
      poly <- secr::boundarytoSF(coord[,1:2])
    }
    else if (ext %in% c("rdata", "rda", "rds")) {
      if (ext == "rds") {
        obj <- readRDS(fileupload[1,4])
      }
      else {
        objlist <- load(fileupload[1,4])
        obj <- get(objlist[1])
      }
      if (is.matrix(obj)) {
        poly <- secr::boundarytoSF(obj[,1:2])
      }
      else {
        if (inherits(obj, "sf")) obj <- sf::st_as_sfc(obj)
        if (inherits(obj, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
          poly <- secr::boundarytoSF(obj)
        }
        else {
          stop("unrecognised boundary object in ", objlist[1])
        }
      }
    }
    else {
      poly <- readshapefile(fileupload)
      if (!is.null(poly)) {
        poly <- sf::st_as_sfc(poly)
      }
    }
  }
  poly   
}
##############################################################################

shpfile <- function (filename) {
  if (is.null(filename)) 
    FALSE 
  else {
    if (is.data.frame(filename))
      filename <- filename[,1]
    length(grep(".shp", tolower(filename)))>0
  }
}
##############################################################################

# estimated fit time in minutes
# LL is the output from secr.fit() with details$LLonly = TRUE
timefn <- function(LL) {
  seconds <- attr(LL,'preptime') + attr(LL,'npar')^2 * attr(LL,'LLtime') * 10
  seconds/60
}
##############################################################################


# Function to wrap and log an expression
# It must be defined where it can access or be passed the reactiveValues object
log_and_run <- function(expr, callstr = "") {
  if (callstr != "") {
    if (substring(callstr, nchar(callstr), nchar(callstr)) != "\n") callstr <- paste0(callstr, "\n")
    shiny::isolate({
      log_data$messages <- c(log_data$messages, paste0("CALL: [", trunc(Sys.time()), "]: ", callstr))
    })
  }
  tryCatch (
    withCallingHandlers( 
      expr,
      message = function(m) {
        shiny::isolate({
          log_data$messages <- c(log_data$messages, paste0("MESSAGE [", trunc(Sys.time()), "]: ", m$message))
          invokeRestart("muffleMessage")
        })
      },
      warning = function(w) {
        shiny::isolate({
          log_data$messages <- c(log_data$messages, paste0("WARNING [", trunc(Sys.time()), "]: ", w$message, "\n"))
          invokeRestart("muffleWarning")
        })
      },
      error = function(e) {
        shiny::isolate({
          log_data$messages <- c(log_data$messages, paste0("ERROR [", trunc(Sys.time()), "]: ", e$message, "\n"))
        })
        # Errors halt execution unless caught by tryCatch
      }
    ),
    error = function(e) return(NULL)
  )
}