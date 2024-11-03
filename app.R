## imports stringr, readxl

## changes 2.0
##   requires secr >= 5.0
##   update for secr 5.0: fxiContour, esaPlot
##   blackbearCH
##   replace rgdal with sf:
##     st_read; st_as_sfc
##     inherits sfc_POLYGON
##   limited resultsPrint box height to 350px (scroll for estimates)

## yet to fix
## SpatialPolygons
## docs 
## possibly split following
## https://stackoverflow.com/questions/43002914/how-to-split-shiny-app-code-over-multiple-files-in-rstudio#43003577

## need to complete mask polygon entry

library(secr)
library(shinyjs)

source('globalvars.R',      local = TRUE)
source('tab-intro.R',       local = TRUE)
source('tab-main.R',        local = TRUE)
source('tab-habitatmask.R', local = TRUE)
source('tab-summary.R',     local = TRUE)
source('tab-options.R',     local = TRUE)
source('tab-help.R',        local = TRUE)
source('tab-about.R',       local = TRUE)

if (compareVersion(as.character(secrversion), '5.0.0') < 0)
  stop("secrapp 2.0 requires secr version 5.0.0 or later",
    call. = FALSE)

# for transfer to secrdesign
# designurl <- "http://127.0.0.1:4429/"    ## temporarily use 4429 local
designurl <- "https://www.stats.otago.ac.nz/secrdesignapp/"   # secrdesignapp 1.2 and above reads parameters

# requires package sf to read shapefiles
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)
# requires package stringr for some string operations
# requires package readxl for reading Excel files

# interrupt is hard -
# see http://htmlpreview.github.io/?https://github.com/fellstat/ipc/blob/master/inst/doc/shinymp.html

# Define UI 
ui <- function(request) {
  
  fluidPage(
    title = "secr app 2.0",
    includeCSS("secrstyle.css"),
    useShinyjs(),
    withMathJax(),
    br(),
    navlistPanel(id = "navlist", widths = c(2,10), well = TRUE, "secr app 2.0",
                tabintro, 
                tabmain,
                tabhabitat,
                tabsummary,
                taboptions,
                tabhelp,
                tababout
    )   # end navlistpanel
  )     # end fluidPage
}

############################################################################################
# Define server logic

server <- function(input, output, session) {
  
  summaryfields <- c("date", "time", "note", "traps", "captures", "filter", 
    "n", "r", "ndetectors", "noccasions", "usagepct", "maskbuffer", "masknrow", 
    "maskspace", "likelihood", "distribution", "model", "hcov", 
    "detectfn", "npar", "logLik", "AIC", "dAIC",
    "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", 
    "se.sigma", "z", "se.z", "k", "proctime"
  )
  
  fieldgroup1 <- 1:18
  fieldgroup2 <- 19:36
  
  ## for cycling through animals at one detector 2019-03-08
  lasttrap <- 0
  clickno <- 0
  
  # for remembering other function
  oldfn <- ""  
  
  # source here as these use input or assign to output
  source('miscfn.R',          local = TRUE)
  source('codestringfn.R',    local = TRUE)
  source('renderUI.R',        local = TRUE)
  source('renderPlot.R',      local = TRUE)
  source('renderPrint.R',     local = TRUE)
  source('renderTable.R',     local = TRUE)
  source('reactiveValues.R',  local = TRUE)
  source('reactive.R',        local = TRUE)
  source('observeEvent.R',    local = TRUE)
  source('observe.R',         local = TRUE)
  source('downloadHandler.R', local = TRUE)
  source('fitmodel.R',        local = TRUE)   # includes addtosummary()
  source('bookmark.R',        local = TRUE)
  
  disable("fitbtn")
  disable("captfilename")
  disable("captxlsname")
  disable("dummybookmarkbutton")

  output$selectingfields   <- renderText('false')
  output$selectinganalyses <- renderText('false')
  output$multisession      <- renderText('false')
  output$nontrap           <- renderText('false')

  outputOptions(output, "selectingfields",        suspendWhenHidden = FALSE)
  outputOptions(output, "selectinganalyses",      suspendWhenHidden = FALSE)
  outputOptions(output, "multisession",           suspendWhenHidden = FALSE)
  outputOptions(output, "nontrap",                suspendWhenHidden = FALSE)
  outputOptions(output, "maskready",              suspendWhenHidden = FALSE)
  outputOptions(output, "maskcovariatesready",    suspendWhenHidden = FALSE)
  outputOptions(output, "maskcovariatefileready", suspendWhenHidden = FALSE)
  outputOptions(output, "maskpolygonsready",      suspendWhenHidden = FALSE)
  outputOptions(output, "modelFitted",            suspendWhenHidden = FALSE)
  outputOptions(output, "capthistLoaded",         suspendWhenHidden = FALSE)
  outputOptions(output, "filterCapt",             suspendWhenHidden = FALSE)
  outputOptions(output, "filterMask",             suspendWhenHidden = FALSE)
  outputOptions(output, "usage",                  suspendWhenHidden = FALSE)

  showNotification(paste("secr", secrversion, secryear), id = "version",
                   closeButton = FALSE, type = "message", duration = seconds)
  
  ##############################################################################
  
  # tidy end of session - app closes in R
  # ?apparently incompatible with bookmarking 2019-01-17
  
  # session$onSessionEnded(function() {
  #     stopApp()
  # })

  ##############################################################################
}
  
##################################################################################
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "server")
##################################################################################
