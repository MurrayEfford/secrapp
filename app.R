## user-visible changes 2.0 November 2024
##   requires secr >= 5.1
##   blackbearCH
##   suggest width actionlink on Main
##   OVforestL left bank only
##   replace rgdal with sf:
##     st_read; st_as_sfc
##     sf_project in secrapp-tutorial
##   clear summaries actionLink on Summary

## other changes 2.0
##   update for secr 5.1: fxiContour, esaPlot
##   limited resultsPrint box height to 350px (scroll for estimates)
##   code split following:
##     https://stackoverflow.com/questions/43002914/how-to-split-shiny-app-code-over-multiple-files-in-rstudio#43003577
##   fix mask fail with polygon + multisession
##   fix bugs in select summary fields (tmp$dAIC etc.)
##   expected time calculated once in output$secrdesignurl <- renderUI ()

## changes after 2.0

## re-vamped notifications: 

# version      duration = seconds; startup only
# lastaction   duration = seconds
# warning      duration = warningseconds
# invalidinput duration = invalidseconds
# error        duration = errorseconds


library(secr)
library(shinyjs)

# requires package sf (polygon operations)
# requires package stringr (for some string operations)
# requires package readxl (for reading Excel files)
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)

source('globalvars.R',      local = TRUE)
source('tab-intro.R',       local = TRUE)
source('tab-main.R',        local = TRUE)
source('tab-habitatmask.R', local = TRUE)
source('tab-summary.R',     local = TRUE)
source('tab-options.R',     local = TRUE)
source('tab-help.R',        local = TRUE)
source('tab-about.R',       local = TRUE)

if (compareVersion(as.character(secrversion), '5.1.0') < 0)
  stop("secrapp 2.0 requires secr version 5.1.0 or later",
    call. = FALSE)

if (!requireNamespace('stringr')) stop("please install package 'stringr'")
if (!requireNamespace('sf')) stop("please install package 'sf'")
if (!requireNamespace('readxl')) stop("please install package 'readxl'")

# interrupt is hard -
# see http://htmlpreview.github.io/?https://github.com/fellstat/ipc/blob/master/inst/doc/shinymp.html

############################################################################################
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
    )   
  )     
}

############################################################################################
# Define server logic

server <- function(input, output, session) {

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
  source('fitmodel.R',        local = TRUE)   
  source('bookmark.R',        local = TRUE)
  
  disable("fitbtn")
  disable("captfilename")
  disable("captxlsname")
  disable("dummybookmarkbutton")
  disable ("habspacing")

  output$selectingfields   <- renderText('false')
  output$selectinganalyses <- renderText('false')
  output$summaries         <- renderText('false')
  output$multisession      <- renderText('false')
  output$nontrap           <- renderText('false')

  outputOptions(output, "selectingfields",        suspendWhenHidden = FALSE)
  outputOptions(output, "selectinganalyses",      suspendWhenHidden = FALSE)
  outputOptions(output, "summaries",              suspendWhenHidden = FALSE)
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

  showNotification(paste("secr", secrversion, secryear), 
                   id = "version", type = "message", duration = seconds, 
                   closeButton = FALSE)
  
  ##############################################################################
  
  # tidy end of session - app closes in R
  # apparently incompatible with bookmarking 2019-01-17
  
  session$onSessionEnded(function() {
      stopApp()
  })

  ##############################################################################
}
## end of server logic
##################################################################################

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "server")
##################################################################################
