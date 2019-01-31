library(openCR)

secrversion <- packageVersion('secr')
if (compareVersion(as.character(secrversion), '3.2.0') < 0)
    stop("secrapp 1.0 requires secr version 3.2.0 or later",
         call. = FALSE)
openCRversion <- packageVersion('openCR')

# requires package rgdal to read shapefiles
# requires package sp for bbox and plot method for SpatialPolygons
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)

# interrupt is hard -
# see http://htmlpreview.github.io/?https://github.com/fellstat/ipc/blob/master/inst/doc/shinymp.html

linewidth <- 2  # for various plots 
seconds <- 6   ## default duration for showNotification()

# Define UI 
ui <- function(request) {
    
    fluidPage(
        title = "secr app 1.0",
        includeCSS("secrstyle.css"),
        withMathJax(),
        tags$head(tags$style(".mypanel{margin-top:5px; margin-bottom:10px; padding-bottom: 5px;}")),
        tags$head(tags$style("#fitPrint{color:blue; font-size:12px; overflow-y:scroll; max-height: 200px; background: ghostwhite;}")),
        br(),
        navlistPanel(id = "navlist", widths = c(2,10), well=TRUE,
                     
                     "secr app 1.0",
                     
                     tabPanel("Main",
                              fluidRow(
                                  column (5, # offset = 0, style='padding:15px;',
                                          h2("Data input"),
                                          wellPanel(class = "mypanel", 
                                                    br(),
                                                    tabsetPanel(
                                                        type = "pills", id = "arrayinput", selected = "Detectors",
                                                        
                                                        tabPanel("Detectors", values = "Detectors",
                                                                 fluidRow(
                                                                     column(6, selectInput("detector", "Detector type",
                                                                                           # choices = c("multi","proximity","count", "single"),
                                                                                           choices = c("multi","proximity","count"),
                                                                                           selected = "proximity", width = 120)),
                                                                     column(6, uiOutput('detectorhelp'))
                                                                 ),
                                                                 
                                                                 br(),
                                                                 div(style="height: 80px;",
                                                                     fileInput("trapfilename", "",   # Detector layout file
                                                                               accept = "text/plain")),
                                                                 helpText(HTML(paste0("Requires text file with detector ID ",
                                                                                      "and x-y coordinates in three columns,",
                                                                                      " as for secr::read.traps"))),
                                                                 textInput("trapargs", "Optional arguments for read.traps()",
                                                                           value = "", placeholder = "e.g., skip = 1, sep = ','")
                                                                 
                                                        ),
                                                        tabPanel("Detections",
                                                                 br(),
                                                                 div(style="height: 80px;",
                                                                     fileInput("captfilename", "Detection file",
                                                                               accept = c(".csv", ".txt", ".rdata", 
                                                                                          ".rda", ".rds"))),
                                                                 helpText(HTML(paste0("Requires text file with columns ",
                                                                                      "Session, ID, Occasion, TrapID,",
                                                                                      " as for secr::read.capthist"))),
                                                                 radioButtons("fmt", label = "Format",
                                                                              choices = c("trapID", "XY")),
                                                                
                                                                 textInput("captargs", "Optional arguments for read.capthist()",
                                                                           value = "", placeholder = "e.g., skip = 1, sep = ','")
                                                                 
                                                        )
                                                    )
                                          ),
                                         h2("Model"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, selectInput("detectfn", "Detection function",
                                                                             choices = c("HHN","HEX"),
                                                                             selected = "HHN", width=110))
                                                   ),
                                                   fluidRow(
                                                       column(4, radioButtons("likelihood", "Likelihood", choices = c("Full", "Conditional"))),
                                                       column(4, radioButtons("distributionbtn", label = "Distribution of n",
                                                                              choices = c("Poisson", "Binomial"))),
                                                       column(4, radioButtons("packagebtn", label = "Function", 
                                                                              choices = c("secr.fit", "openCR.fit")))
                                                   )
                                         ),
                                         
                                         
                                         h2("Actions"),
                                         
                                         fluidRow(
                                             column(5, actionButton("fitbtn", "Fit model",  width = 130,
                                                                    title = "Fit spatially explicit capture-recapture model to estimate density and update Results")),
                                             column(6, actionButton("appendbtn", "Add to summary",  width = 130,
                                                                    title = "Append new analysis to Summary table"))
                                         ),
                                         
                                         br(),
                                         fluidRow(
                                             column(5, actionButton("resetbtn", "Reset all", width = 130, 
                                                                    title = "Reset all inputs to initial values")),
                                             column(6, bookmarkButton(width = 130)) 
                                         ),
                                         br(),
                                         fluidRow(
                                             column(7, helpText(HTML("F11 to toggle fullscreen")))
                                         ),
                                         fluidRow(
                                             column(11, textInput("title", "", value = "", 
                                                                  placeholder = "label for Summary")))
                                  ),
                                  
                                  column (6, # style='padding:0px;',
                                          h2("Results"),
                                          
                                          fluidRow(
                                              column(12, 
                                                     
                                                     verbatimTextOutput("fitPrint"))
                                          ),
                                          
                                          fluidRow(
                                              column(12,
                                                     br(),
                                                     tabsetPanel(type = "pills",
                                                                 id = "tabs",
                                                                 tabPanel("Array",
                                                                          fluidRow(
                                                                              column(9, style='padding:0px;', plotOutput("arrayPlot", 
                                                                                                                         click = clickOpts(id="arrayClick", clip = FALSE))),
                                                                              column(3, br(), conditionalPanel("input.gridlines != 'None'",
                                                                                                               uiOutput("uigridlines") ),
                                                                                     br(), uiOutput('xycoord'))
                                                                          ),
                                                                          fluidRow(
                                                                              column(11, style='padding:0px;', verbatimTextOutput("ntrapPrint"))
                                                                              ,
                                                                              column(1, br(), conditionalPanel("output.ntrapPrint!= ''",
                                                                                                               downloadLink("downloadArray", "Save")))
                                                                          )
                                                                 ),
                                                                 tabPanel("Detectfn", plotOutput("detnPlot", height = 320)),
                                                                 tabPanel("Popn", 
                                                                          plotOutput("popPlot", height = 320),
                                                                          
                                                                          fluidRow(
                                                                              column(4, checkboxInput("showmaskbox", "Display mask",
                                                                                                      value = FALSE,
                                                                                                      width = 180),
                                                                                     checkboxInput("onlymaskbox", "Restrict to mask",
                                                                                                   value = TRUE,
                                                                                                   width = 180)
                                                                              ),
                                                                              column(4, checkboxInput("showHRbox", "Display 95% HR",
                                                                                                      value = FALSE,
                                                                                                      width = 180),
                                                                                     uiOutput('uipopN')),
                                                                              column(4, actionButton("randompopbtn", "Randomize",
                                                                                                     title = "Pick another realisation of the population")
                                                                              )
                                                                          )
                                                                 ),
                                                                 tabPanel("Pxy",
                                                                          plotOutput("pxyPlot", height = 320, click = "pxyclick"),
                                                                          helpText(HTML("p.(x) is the probability an animal at point x will be detected at least once"))
                                                                 ),
                                                                 
                                                                 tabPanel("Power",
                                                                          fluidRow(
                                                                              column(11, plotOutput("powerPlot", height = 320, click = "CIclick"))
                                                                          ),
                                                                          br(),
                                                                          fluidRow(
                                                                              column(3, offset = 1,
                                                                                     br(), checkboxInput("powertype", "95% CI",
                                                                                                         value = TRUE,
                                                                                                         width = 130)),
                                                                              ## uiOutput('CIpct'),
                                                                              column(4, 
                                                                                     br(), checkboxInput("adjustRSEbox", "Adjust final RSE",
                                                                                                         value = TRUE,
                                                                                                         width = 130)),
                                                                              # helpText(HTML("Scales with population"))),
                                                                              column(4, conditionalPanel("input.powertype==true",
                                                                                                         numericInput("xpos", "% change", min = -100, max = 250, 
                                                                                                                      step = 1, value = 0, width = 70)
                                                                              )
                                                                              )
                                                                              
                                                                          ),
                                                                          fluidRow(
                                                                              column(12,
                                                                                     sliderInput("RSEslider", "",
                                                                                                 min = 1.0,
                                                                                                 max = 40,
                                                                                                 value = 1,
                                                                                                 step = 0.1,
                                                                                                 pre = "RSE ",
                                                                                                 post = "%",
                                                                                                 width = "90%"))
                                                                          )
                                                                 )
                                                     )
                                              )
                                          )
                                  )
                              )
                     ),
                     
                     #################################################################################################
                     
                     tabPanel("Summary",
                              br(),
                              fluidRow(
                                  column(3, 
                                         wellPanel(
                                             h2("Fields"),
                                             fluidRow(
                                                 column(4, actionButton("selectfieldsbtn", "Select", title = "Choose fields to display")), 
                                                 column(4, actionButton("selectnonebtn", "None", title = "Unselect all fields")), 
                                                 column(4, actionButton("selectallbtn", "All", title = "Select all fields"))
                                             ),
                                             br(),
                                             h2("Scenarios"),
                                             fluidRow(
                                                 column(6, actionButton("clearallbtn", "Clear all", title = "Delete all scenarios")), 
                                                 column(6, actionButton("clearlastbtn", "Delete last", title = "Delete last scenario"))
                                             ), 
                                             br(), 
                                             h2("Download"),
                                             fluidRow(
                                                 column(6, downloadButton("downloadSummaryrds", ".rds", 
                                                                          title = "Save as RDS file; restore in R with e.g., readRDS(`summary.rds')")), 
                                                 column(6, downloadButton("downloadSummary", ".csv", 
                                                                          title = "Save as comma-delimited text (csv) file"))
                                             )
                                         ),
                                         
                                         fluidRow(
                                             column(5, offset=1,
                                                    conditionalPanel("output.selectingfields == 'TRUE'",
                                                                     checkboxGroupInput("fields1", "",
                                                                                        choices = c("date", "time", "note", "detector", "source", "nx", "ny", "spacex", "spacey",
                                                                                                    "ndetectors", "noccasions", "nrepeats", "distribution", "detectfn",
                                                                                                    "D", "lambda0", "sigma"),
                                                                                        selected = c("date", "time", "note", "detector", "source", "nx", "ny", "spacex", "spacey",
                                                                                                     "ndetectors", "noccasions", "nrepeats", "distribution", "detectfn",
                                                                                                     "D", "lambda0", "sigma")
                                                                     ))),
                                             column(6, 
                                                    conditionalPanel("output.selectingfields == 'TRUE'",
                                                                     checkboxGroupInput("fields2", "",
                                                                                        choices = c("detperHR", "k", "En", "Er", "Em",
                                                                                                    "rotRSE", "CF", "route", "cost", "simfn",  "nrepl", "simtime",
                                                                                                    "simRSE", "simRSEse", "simRB", "simRBse"),
                                                                                        selected = c("detperHR", "k", "En", "Er", "Em",
                                                                                                     "rotRSE", "CF", "route", "cost", "simfn",  "nrepl", "simtime",
                                                                                                     "simRSE", "simRSEse", "simRB", "simRBse")
                                                                     )
                                                    )                                                  
                                             )
                                         )
                                         
                                  ),
                                  column(9, 
                                         # h2("Results"),
                                         #                                         div(tableOutput("summarytable"), style = "width:800px; overflow-x: scroll; font-size:80%")
                                         div(tableOutput("summarytable"), style = "width:800px; overflow-x: scroll")
                                  )
                              )
                     ),
                     #################################################################################################
                     
                     tabPanel("Options",
                              
                              fluidRow(
                                  column(3,
                                         
                                         h2("Detector array"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, radioButtons("areaunit", label = "Area units",
                                                                              choices = c("ha", "km^2"), 
                                                                              selected = "ha", inline = TRUE))
                                                   )
                                         ),
                                         
                                         selectInput("method", "Maximization method",
                                                     choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                                                     selected = "Newton-Raphson", width=160),

                                         h2("Habitat mask"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, 
                                                              numericInput("buffer", "Buffer width (m)",
                                                                           min = 0,
                                                                           max = 20,
                                                                           value = 100,
                                                                           step = 0.5,
                                                                           width = 250),
                                                              numericInput("habnx", "Mesh dimension nx",
                                                                           min = 10,
                                                                           max = 1000,
                                                                           value = 32,
                                                                           step = 1,
                                                                           width = 180)
                                                       ),
                                                       column(6, 
                                                              radioButtons("maskshapebtn", label = "Shape",
                                                                           choices = c("Rectangular", "Rounded"), 
                                                                           selected = "Rounded")
                                                       )
                                                   ),
                                                   br(),
                                                   div(style="height: 80px;",
                                                       fileInput("habpolyfilename", "Mask polygon file(s)",
                                                                 accept = c('.shp','.dbf','.sbn','.sbx',
                                                                            '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                                                                 multiple = TRUE)),
                                                   uiOutput("habitatfile"),
                                                   fluidRow(
                                                       column(10, offset = 1, div(style="height: 20px;",
                                                                                  checkboxInput("polygonbox", "Clip to polygon(s)", value = TRUE, width = 180)))
                                                   ),
                                                   fluidRow(
                                                       column(10, offset = 1, radioButtons("includeexcludebtn", label = "",
                                                                                           choices = c("Include", "Exclude"), 
                                                                                           selected = "Include", inline = TRUE))
                                                   )
                                         )
                                  ),
                                  
                                  column(3,
                                         
                                         h2("Array plot"),
                                         wellPanel(class = "mypanel", 
                                                   radioButtons("gridlines", label = "Grid spacing (m)",
                                                                choices = c("None", "100", "1000", "10000", "100000"),
                                                                selected = "None", inline = TRUE),
                                                   fluidRow(
                                                       column(6, checkboxInput("entireregionbox", "Show entire region", value = TRUE, width = 160)),
                                                       column(5, checkboxInput("snaptodetector", "Snap to detector", value = FALSE, width = 160))
                                                   )
                                         ),
                                         h2("Pxy contour plot"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, numericInput("pxyborder", "Border (spacing units)",
                                                                              min = 0,
                                                                              max = 10,
                                                                              value = 3,
                                                                              step = 0.5,
                                                                              width = 180),
                                                              numericInput("pxynx", "Mesh dimension nx",
                                                                           min = 32,
                                                                           max = 512,
                                                                           value = 64,
                                                                           step = 32,
                                                                           width = 180)),
                                                       column(6,
                                                              br(),
                                                              checkboxInput("pxyfillbox", "Fill contours",
                                                                            value = TRUE,
                                                                            width = 180),
                                                              checkboxInput("pxyframebox", "Show frame",
                                                                            value = FALSE,
                                                                            width = 180),
                                                              checkboxInput("pxylabelbox", "Label contours",
                                                                            value = TRUE,
                                                                            width = 180))
                                                   )
                                         )
                                  ),
                                  
                                  column(3,
                                         h2("Power plot"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, numericInput("alpha", "alpha",
                                                                              min = 0.001,
                                                                              max = 0.200,
                                                                              value = 0.05,
                                                                              step = 0.001,
                                                                              width = 120))
                                                   ),
                                                   fluidRow(
                                                       column(6, numericInput("minEffect", "xmin",
                                                                              min = -99,
                                                                              max = 0,
                                                                              value = -99,
                                                                              step = 1,
                                                                              width = 180)),
                                                       column (6, numericInput("maxEffect", "xmax",
                                                                               min = 0,
                                                                               max = 300,
                                                                               value = 150,
                                                                               step = 1,
                                                                               width = 180))
                                                   ),
                                                   
                                                   br(),
                                                   h4("Hypothesis test"),
                                                   # br(),
                                                   fluidRow(
                                                       column(6,selectInput("testtype", "Type",
                                                                            choices = c("two.sided", "decrease", "increase"),
                                                                            selected = "two.sided",
                                                                            width = 140)),
                                                       column(6, numericInput("target", "Target power %",
                                                                              min = 50,
                                                                              max = 99,
                                                                              value = 80,
                                                                              step = 1,
                                                                              width = 180))
                                                   )
                                         )
                                  )
                              )
                     ),
                     tabPanel("Help",
                              withMathJax(includeMarkdown("help.rmd"))
                     ),
                     tabPanel("About",
                              h2("secr app 1.0"), br(),
                              
                              h5(paste("This Shiny application provides an interface to the R package 'secr', version", 
                                       packageDescription("secr")$Version), "."),
                              br(),
                              h5("For further information see "), 
                              a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
                              a("CRAN.R-project.org/package=secr", href="https://CRAN.R-project.org/package=secr", target="_blank"),
                              br(), br(),
                              
                              h5("Citation"),
                              h5("[The preferred citation for this package is not finalised]")
                     )
                     
        )   # end navlistpanel
    )       # end fluidPage
}

############################################################################################
# Define server logic

server <- function(input, output, session) {
    
    desc <- packageDescription("secr")
    summaryfields <- c("date", "time", "note", "detector", "source", 
                       "ndetectors", "noccasions", "distribution", "detectfn", 
                       "D", "lambda0", "sigma", "detperHR", "k"
                       )
    fieldgroup1 <- 1:14
    fieldgroup2 <- 1:14

    showNotification(paste("secr", desc$Version, desc$Date),
                     closeButton = FALSE, type = "message", duration = seconds)
     output$selectingfields <- renderText('false')
     outputOptions(output, "selectingfields", suspendWhenHidden = FALSE)
    ##############################################################################
    
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
    
    output$persqkm <- renderUI({
        ## display density in animals/km^2
        Dkm <- density() * 100
        Dkmtext <- paste0(Dkm, '&nbsp; animals / km<sup>2</sup>')
        helpText(HTML(Dkmtext))
    })
    
    ##############################################################################
    
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
        helpText(HTML(helptext))
    })
    ##############################################################################
    
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
    ##############################################################################
    
    output$habitatfile <- renderUI({
        helptext <- ""
        if (!is.null(input$habpolyfilename)) {
            pos <- grep(".shp", tolower(input$habpolyfilename[,1]))
            if (length(pos)>0)
                helptext <- paste0(input$habpolyfilename[pos,1])
            pos <- grep(".rda", tolower(input$habpolyfilename[,1]))  # .rda, .rdata
            if (length(pos)>0) {
                objlist <- load(input$habpolyfilename[1,4])
                helptext <- paste0(objlist[1])
            }
            pos <- grep(".rds", tolower(input$habpolyfilename[,1])) 
            if (length(pos)>0) {
                helptext <- paste0(input$habpolyfilename[pos,1])
            }
        }
        helpText(HTML(helptext))
    })
    ##############################################################################
    
    output$uigridlines <- renderUI({
        if(input$gridlines=="None")
            helpText("")
        else if (input$gridlines=="100")
            helpText(span(style="color:gray", HTML("100-m grid")))
        else 
            helpText(span(style="color:gray", HTML(paste0(round(as.numeric(input$gridlines)/1000,1), "-km grid"))))
    })
    
    output$xycoord <- renderUI({
        xy <- c(input$arrayClick$x, input$arrayClick$y)
        tmpgrid <- isolate(detectorarray())
        if (is.null(xy)) 
            helpText("")
        else {
            if (input$snaptodetector) {
                nearest <- nearesttrap(xy, tmpgrid)
                xy <- tmpgrid[nearest,]
                id <- paste0(rownames(tmpgrid)[nearest], ":")
            }
            else {
                id <- ""
            }
            helpText(HTML(paste(id, paste(round(xy), collapse = ", "))))
        }
    })
    
    ##############################################################################
    ## miscellaneous functions
    
    ## plotpower    
    ## plotpowerCI    
    ## readpolygon
    ## addtosummary
    ## arraycode
    ## maskcode
    ## fitcode    
    ## areastr    format area with units
    ## density    density in animals / ha
    
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

    density <- function() {
        ## return density in animals / hectare
        if (input$areaunit == "ha")
            1 ## input$D
        else
            100 ## input$D/100  ## per sq. km
    }
    ##############################################################################
    sigma <- function() {
        ## return sigma in metres
        25
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
                poly <- secr:::boundarytoSP(coord[,1:2])
            }
            else if (ext %in% c("rdata", "rda", "rds")) {
                if (ext == "rds") {
                    obj <- readRDS(fileupload[1,4])
                }
                else {
                    objlist <- load(fileupload[1,4])
                    obj <- get(objlist[1])
                }
                if (is.matrix(obj))
                    poly <- secr:::boundarytoSP(obj[,1:2])
                else if (inherits(obj, "SpatialPolygons"))
                    poly <- obj
                else stop("unrecognised boundary object in ", objlist[1])
            }
            else {
                ## not working on restore bookmark 2019-01-24
                dsnname <- dirname(fileupload[1,4])
                for ( i in 1:nrow(fileupload)) {
                    file.rename(fileupload[i,4], paste0(dsnname,"/",fileupload[i,1]))
                }
                filename <- list.files(dsnname, pattern="*.shp", full.names=FALSE)
                layername <- tools::file_path_sans_ext(basename(filename))
                if (is.null(filename) || 
                    !(any(grepl(".shp", fileupload[,1])) &&
                      any(grepl(".dbf", fileupload[,1])) &&
                      any(grepl(".shx", fileupload[,1])))) {
                    showNotification("need shapefile components .shp, .dbf, .shx",
                                     type = "error", id = "nofile", duration = seconds)
                }
                else  if (!requireNamespace("rgdal"))
                    showNotification("need package rgdal to read shapefile", type = "error", id = "norgdal", duration = seconds)
                else {
                    removeNotification(id = "nofile")
                    removeNotification(id = "norgdal")
                    poly <- rgdal::readOGR(dsn = dsnname, layer = layername)
                }
            }
        }
        poly   
    }
    ##############################################################################

    addtosummary <- function() {
        ## input$fields is character vector of selected fields
        
        ## tentatively suppress 2019-01-18 
        ## invalidateOutputs()
        df <- data.frame(
            date = format(Sys.time(), "%Y-%m-%d"),
            time = format(Sys.time(), "%H:%M:%S"),
            note = input$title,
            detector = input$detector,
            source = "", # input$arrayinput,
            ndetectors = dim(capthist())[3],
            noccasions = isolate(noccasions()),
            distribution = input$distributionbtn,
            detectfn = input$detectfn,
            # D = density(),
            # lambda0 = input$lambda0,
            # sigma = input$sigma,
            k = if (input$detectfn=="HHN") round(density()^0.5 * sigma() / 100,3) else NA
        )

        newfields <- ""        
        
        sumrv$value <- rbind (sumrv$value, df)
        # updateCheckboxGroupInput("fields2", selected = newfields)  ## FAILS 2019-01-23
        rownames(sumrv$value) <- paste0("Fit", 1:nrow(sumrv$value))
    }
    ##############################################################################
    
    getSPcode <- function (inputfilename, varname, apply = TRUE) {
        filename <- inputfilename[1,1]
        if (is.null(filename) || !apply) {
            return("")
        }
        else {
            ext <- tolower(tools::file_ext(filename))
            if (ext == "txt") {
                code <- paste0( 
                    "# coordinates from text file\n",
                    "coord <- read.table('", filename, "')   # read boundary coordinates\n",
                    varname, " <- secr:::boundarytoSP(coord)  # convert to SpatialPolygons\n")
            }
            else if (ext %in% c("rdata", "rda")) {
                objlist <- load(inputfilename[1,4])
                code <- paste0( 
                    "# SpatialPolygons from RData file\n",
                    "objlist <- load('", filename, "')\n",
                    varname, " <- get(objlist[1]) \n")
            }
            else if (ext == "rds") {
                code <- paste0( 
                    "# SpatialPolygons from RDS file\n",
                    varname, " <- readRDS('", filename, "') \n")
            }
            else {
                code <- paste0(
                    "# ESRI polygon shapefile\n",
                    varname, " <- rgdal::readOGR(dsn = '", 
                    tools::file_path_sans_ext(basename(filename)), 
                    ".shp')\n"
                )
            }
            code
        }
    }    
    
    arraycode <- function (comment = FALSE) {
        # returns the R code needed to generate the specified array, 
        # as a character value
        code <- ""  
        if (!is.null(detectorarray())) {
            #     args <- input$args
            #     if (args != "")
            #         args <- paste0(", ", args)
            #     code <- paste0("array <- read.traps ('", 
            #                    input$trapfilename[1,"name"],
            #                    "', detector = '", input$detector, "'", args, ")\n")
            #     if (input$scalefactor != 1.0) {
            #         code <- paste0(code, 
            #                        "# optional scaling about centroid\n",
            #                        "meanxy <- apply(array,2,mean)\n",
            #                        "array[,1] <- (array[,1]- meanxy[1]) * ", input$scalefactor, " + meanxy[1]\n",
            #                        "array[,2] <- (array[,2]- meanxy[2]) * ", input$scalefactor, " + meanxy[2]\n")
            #         #"array[,] <- array[,] * ", input$scalefactor, "\n")
            #     }
            # }
            #     
            # if (comment) {
            #     tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
            #     tmp$sep <- "\n"
            #     code <- do.call(paste, tmp)
            # }
        }
        code        
    }
    ##############################################################################
    
    maskcode <- function (arrayname) {
        type <- if (input$maskshapebtn == 'Rectangular') 'traprect' else 'trapbuffer'
        buffer <- as.character(round(input$buffer,2))
        polycode <- ""
        polyhabitat <- ""
        
        if (input$polygonbox && !is.null(input$habpolyfilename)) { 
            polyhabitat <- input$includeexcludebtn == "Include"
            polycode <- getSPcode(input$habpolyfilename, "poly", input$polygonbox)
        }
        paste0(polycode,
               "mask <- make.mask (", arrayname, 
               ", buffer = ", buffer, 
               ", nx = ", input$habnx, 
               ", type = '", type, "'",  
               if (polycode == "") "" else ",\n    poly = poly",
               if (polycode == "") "" else ", poly.habitat = ", polyhabitat,
               ")\n")
    }
    ##############################################################################
    
    fitcode <- function() {
        detfn <- input$detectfn
        if (is.character(detfn)) detfn <- paste0("'", detfn, "'")
        code <- ""
        # code <- paste0(
        #     "# R code to generate main results\n",
        #     "library(secr)\n\n",
        #     arraycode(),
        #     "\n",
        #     maskcode("array"),
        #     "\n",
        #     "ch <- read.capthist()\n",
        #     "fit <- secr.fit(ch, mask = mask, detectfn = ", detfn, ")\n\n"
        # )
        code
    }
    ##############################################################################
    
    fitmodel <- function() {
        ## isolate(fit <- secr.fit (capthist(), mask = mask(), detectfn = input$detectfn))
        if (input$likelihood == "Full") {
            type <- "secrD"
            CL <- FALSE
        }
        else {
            type <- "secrCL"
            CL <- TRUE
        }
        fit <- NULL
        if (input$packagebtn == "openCR.fit")
            isolate(fit <- openCR.fit (capthist(), type = type, 
                                       mask = mask(), 
                                       detectfn = input$detectfn, 
                                       distribution = tolower(input$distributionbtn)))
        else if (input$packagebtn == "secr.fit")
            isolate(fit <- secr.fit (capthist(), CL = CL, trace = FALSE,
                                     mask = mask(), 
                                     detectfn = input$detectfn, 
                                     details = list(distribution = input$distributionbtn)))
        fit
    }
    ##############################################################################

    maskOK <- function () {
        if (!is.null(poly())) {
            sum(pointsInPolygon(detectorarray(), poly())) > 0
        }
        else TRUE
    }
    ##############################################################################
    
    
    ## reactive
    
    ## invalidateOutputs
    ## array
    ## poly
    ## region
    ## mask
    ## pop
    ## Pxy
    ## nrm
    ## validspacing
    
    ##############################################################################
    
    invalidateOutputs <- reactive({
        pxyrv$value <- NULL
        updateNumericInput(session, "D", step = 10^trunc(log10(density()/50)))
    })
    
    ##############################################################################
    
    readtrapfile <- function () {
        inFile <- input$trapfilename
        trps <- NULL
        if (!is.null(inFile)) {
            filename <- input$trapfilename[1,"datapath"]
            if (is.null(filename))
                stop("provide valid filename")
            args <- input$trapargs
            if (args != "")
                args <- paste0(", ", args)
            readtrapscall <- paste0("read.traps (filename, detector = input$detector", args, ")")
            trps <- try(eval(parse(text = readtrapscall)))
            if (!inherits(trps, "traps")) {
                showNotification("invalid trap file or arguments; try again",
                                 type = "warning", id = "badarray", duration = seconds)
            }
        }    
        trps
    }
    
    detectorarray <- reactive(
        {
            pxyrv$value <- NULL
            trps <- NULL
            removeNotification("badarray")
            trps <- readtrapfile()
            if (!is.null(trps)) {
                attr(trps, "arrayspan") <- suppressWarnings(pmax(0, max(dist(trps))))
            }
            # if (!is.null(trps) && (nrow(trps) > input$maxdetectors)) {
            #     showNotification(paste0("more than ", input$maxdetectors, " detectors; try again"),
            #                      type = "warning", id = "bigarray", duration = seconds)
            #     trps <- NULL
            # }
            
            if (!is.null(trps) && (nrow(trps) == 0)) {
                showNotification(paste0("no detectors; try again"),
                                 type = "warning", id = "zeroarray", duration = seconds)
                trps <- NULL
            }
            trps
        }
    )
    ##############################################################################

    readcaptfile <- function () {
        inFile <- input$captfilename
        captdf <- NULL
        if (!is.null(inFile)) {
            filename <- input$captfilename[1,"datapath"]
            if (is.null(filename))
                stop("provide valid filename")
            args <- input$captargs
            if (args != "")
                args <- paste0(", ", args)
            readcaptcall <- paste0("read.table (filename", args, ")")
            captdf <- try(eval(parse(text = readcaptcall)))
            if (!inherits(captdf, "data.frame")) {
                showNotification("invalid capture file or arguments; try again",
                                 type = "warning", id = "badcapt", duration = seconds)
            }
        }    
        captdf
    }
    
    capthist <- reactive( {
        make.capthist(readcaptfile(), readtrapfile(), fmt = input$fmt) #, noccasions = input$noccasions)
    })
    
    noccasions <- reactive( {ncol(capthist())})
    
    poly <- reactive( {
        if (input$polygonbox) {
            readpolygon(input$habpolyfilename)
        }
        else {
            NULL
        }
    }
    )
    ##############################################################################
    
    mask <- reactive( {
        pxyrv$value <- NULL
        if (!maskOK()) showNotification("no detectors in habitat polygon(s)",
                                        type = "warning", id = "notrapsinpoly",
                                        duration = seconds)
        msk <- make.mask (detectorarray(),
                          buffer = input$buffer,
                          nx = input$habnx,
                          type = if (input$maskshapebtn=='Rectangular') 'traprect' else 'trapbuffer',
                          poly = poly(),
                          poly.habitat = input$includeexcludebtn == "Include",
                          keep.poly = FALSE)
        msk
    }
    )
    ##############################################################################
    
    pop <- reactive(
        {
            poprv$v
            core <- detectorarray()
            if (is.null(core) || (input$D == 0)) {
                return (NULL)
            }
            if (density() * maskarea(mask()) > 10000) {
                showNotification("population exceeds 10000; try again",
                                 type = "error", id = "bigpop", duration = seconds)
                return(NULL)
            }
            else removeNotification("bigpop")
            Ndist <- if (input$distributionbtn == 'Poisson') 'poisson' else 'fixed'
            if (input$onlymaskbox) {
                if (nrow(mask())==0) {
                    showNotification("incompatible mask",
                                     type = "error", id = "badmask", duration = seconds)
                    pop <- NULL
                }
                else {
                    pop <- sim.popn (D = density(), core=mask(), model2D="IHP", Ndist = Ndist)
                }
            }
            else { # rectangular area
                pop <- sim.popn (D = density(), core=core, Ndist = Ndist,
                                 buffer = input$buffer)
            }
            pop
        }
    )
    ##############################################################################
    
    Pxy <- reactive({

        # invalidateOutputs()
        # trps <- detectorarray()
        # # msk <- make.mask(trps, buffer = input$sigma * input$pxyborder, nx = input$pxynx)
        # msk <- make.mask(trps, buffer = border(input$pxyborder), nx = input$pxynx)
        # Pxy <- pdot(msk, trps, detectfn = input$detectfn,
        #             detectpar=list(lambda0=input$lambda0, sigma = input$sigma),
        #             noccasions = input$noccasions)
        # sumPxy <- sum(Pxy)
        # EPxy <- sum(Pxy^2) / sumPxy
        # EPxy2 <- sum(Pxy^3) /sumPxy
        # varPxy <- EPxy2 - EPxy^2
        # sinuosity <- if (nrow(trps)<=1) NA else attr(trps, "arrayspan") / (spacing(trps) * (nrow(trps)-1))
        # list(CVPxy = sqrt(varPxy)/EPxy, sinuosity = sinuosity, esa = sumPxy * attr(msk, "area"))
    })
    ##############################################################################

    ## reactiveValues
    
    ## simrv, rotrv, RSErv, pxyrv : logical
    ## sumrv : summary table
    ## poprv
    ## manualroute
    
    ##############################################################################
    
    RSErv <- reactiveValues(current = FALSE, value = NULL, adjRSE = NULL)
    pxyrv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
    poprv <- reactiveValues(v = 0)  # used to invalidate and re-plot popn
    arrrv <- reactiveValues(v = 0)  # used to invalidate and re-plot detectorarray
    current <- reactiveValues(unit = "ha")
    selecting <- reactiveValues(v=FALSE)
    sumrv <- reactiveValues(
        value = read.csv(text = paste(summaryfields, collapse = ", "))
    )
    fitrv <- reactiveValues(value = NULL)
    ##############################################################################
    
    ## observe
    
    ##############################################################################
    
    ## Modal dialogue to confirm simulation if it might take a long time
    
    OKModal <- function(time) {
        modalDialog(
            paste("Fitting is predicted to take ", round(time,1), " minutes"),
            size = "s",
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("okbtn", "Continue")
            )
        )
    }
    
    ##############################################################################
    
    ## observeEvent
    
    # resetbtn
    # CFslider
    # fitbtn
    # okbtn
    # clearallbtn
    # clearlastbtn
    # arrayinput
    # click
    # arrayclick
    # pxyclick
    # appendbtn
    # areaunit

    ##############################################################################
    
    observeEvent(input$resetbtn, {
        
        ## DOES NOT RESET FILE INPUTS
        ## SEE E.G. https://groups.google.com/forum/#!topic/shiny-discuss/HbTa4v612FA

        current$unit <- "ha"
        
        ## array

        ## grid
        updateSelectInput(session, "detector", selected = "proximity")
        updateTabsetPanel(session, "arrayinput", selected = "Grid")

        ## file
        updateTextInput(session, "trapargs", 
                        value = "", placeholder = "e.g., skip = 1, sep = ','")
        updateTextInput(session, "captargs", 
                        value = "", placeholder = "e.g., skip = 1, sep = ','")
        updateRadioButtons(session, "fmt", selected = "TrapID")

        ## parameters
        updateSelectInput(session, "detectfn", selected = "HHN")

        ## general
        updateTextInput(session, "title", "", value = "",
                        placeholder = "label for Summary")
        updateRadioButtons(session, "distributionbtn", selected = "Poisson")
        updateCheckboxInput(session, "autorefresh", value = TRUE)

        ## pop plot
        updateCheckboxInput(session, "showHRbox", "Display 95% home range", value = FALSE)
        updateCheckboxInput(session, "showmaskbox", "Display mask", value = FALSE)
        updateCheckboxInput(session, "onlymaskbox", "Restrict to mask", value = TRUE)

        ## power plot
        updateCheckboxInput(session, "adjustRSEbox", value = TRUE)
        updateCheckboxInput(session, "powertype", "95% CI", value = TRUE)
        updateNumericInput(session, "xpos", value = 0)

        ## options

        ## detector array
        updateRadioButtons(session, "areaunit", selected = "ha")

        ## habitat
        updateNumericInput(session, "buffer", value = 100)
        updateNumericInput(session, "habnx", value = 32)
        updateRadioButtons(session, "maskshapebtn", selected = "Rounded")
        updateCheckboxInput(session, "polygonbox", value = TRUE)
        updateCheckboxInput(session, "exclusionbox", value = TRUE)
        updateRadioButtons(session, "includeexcludebtn", selected = "Include")

        ## array plot
        updateCheckboxInput(session, "entireregionbox", value = TRUE)
        updateCheckboxInput(session, "snaptodetector", value = FALSE)
        updateRadioButtons(session, "gridlines", selected = "None")

        ## pxy plot
        updateNumericInput(session, "pxyborder", value = 3)
        updateNumericInput(session, "pxynx", value = 64)
        updateCheckboxInput(session, "pxyfillbox", value = TRUE)
        updateCheckboxInput(session, "pxyframebox", value = FALSE)
        updateCheckboxInput(session, "pxylabelbox", value = TRUE)

        updateSliderInput(session, "CFslider", value = 1.0)
        updateCheckboxInput(session, "updateCFbox", value = TRUE)

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
    })
    
    ##############################################################################
    
    observeEvent(input$CFslider, {
        rotrv$current <- FALSE
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

    observeEvent(input$alpha, {
        updateCheckboxInput(session, "powertype", label = paste0(
            round(100 *(1-input$alpha), 1), "% CI"))
    })
    
    ##############################################################################
    
    observeEvent(input$fitbtn, ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
        if (!is.null(detectorarray())) {
            progress <- Progress$new(session, min = 1, max = 15)
            on.exit(progress$close())
            progress$set(message = 'Fitting...',
                         detail = '')
            methodfactor <- 1 + ((input$method != "none") * 4)
            functionfactor <- switch(input$packagebtn, secr.fit = 4, openCR.fit = 1, 0.1)
            detectorfactor <- switch(input$detector, proximity = 1, single = 0.6, multi = 0.6, count = 4)
            time <- nrow(mask()) * nrow(detectorarray()) / 4.5e9 * ## blocked 2019-01-14 nrepeats() * 
                noccasions() * 
                methodfactor * functionfactor * detectorfactor
            if (time > 0.2)
                showModal(OKModal(time))
            else {
                fitrv$value <- fitmodel()
            }
        }
    })
    
    ##############################################################################
    
    observeEvent(input$okbtn, {
        ## user happy: proceed with long simulation
        removeModal()
        fitmodel()
    })
    
    ##############################################################################

    observeEvent(input$randompopbtn, ignoreInit = TRUE, {
        # invalidates pop when button pressed
        poprv$v <- poprv$v + 1
    })
    
    ##############################################################################

    observeEvent(input$selectfieldsbtn, {
        selecting$v <- ! selecting$v
        output$selectingfields <- renderText(selecting$v)
            
    }   )
    
    observeEvent(input$selectallbtn, {
        updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
        updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])
    }   )
    
    observeEvent(input$selectnonebtn, {
        updateCheckboxGroupInput(session, "fields1", selected = "")
        updateCheckboxGroupInput(session, "fields2", selected = "")
    }   )
    
    ##############################################################################
    observeEvent(input$clearallbtn, {
        sumrv$value <- sumrv$value[0,]
    }   )
    
    ##############################################################################
    
    observeEvent(input$clearlastbtn, {
        if (nrow(sumrv$value)>0)
            sumrv$value <- sumrv$value[-nrow(sumrv$value),]
    }   )
    
    ##############################################################################
    
    observeEvent(input$pxyclick, {
        invalidateOutputs()
        trps <- detectorarray()
        
        border <- border(input$pxyborder)
        
        xy <- c(input$pxyclick$x, input$pxyclick$y)
        if ((xy[2] < (min(trps$y) - border)) ||
            (xy[2] > (max(trps$y) + border)) ||
            (xy[1] < (min(trps$x) - border)) ||
            (xy[1] > (max(trps$x) + border)) ) {
            pxyrv$value <- NULL
        }
        else {
            Pxy <- pdot (xy, trps,
                         detectfn = input$detectfn,
                         detectpar = list(lambda0 = input$lambda0, sigma = input$sigma),
                         noccasions = input$noccasions)
            pxyrv$xy <-xy
            pxyrv$value <- Pxy}
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
    
    observeEvent(input$appendbtn, {
        if (!is.null(detectorarray()))
            addtosummary()
    })
    
    ##############################################################################

    # observeEvent(input$summarybtn, {
    #     ap <- isolate(input$appendbox)
    #     filename <- isolate(input$savefilename)
    #     ex <- file.exists(filename)
    #     write.table(sumrv$value[c(input$fields1, input$fields2),],
    #                 append = ap & ex,
    #                 file = filename,
    #                 col.names = !ap | !ex,
    #                 row.names = FALSE,
    #                 quote = FALSE)
    # }
    # )
    # ##############################################################################
    # 
    ## renderText
    
    # ntrapPrint 
    # fitPrint 
    
    ##############################################################################
    
    output$ntrapPrint <- renderText({
        gr <- detectorarray()
        glength <- attr(gr, "arrayspan")
        if (!is.null(gr)) {
            if (glength/sigma() > 100) 
                ratio <- round(glength/input$sigma)
            else 
                ratio <- round(glength/input$sigma,1)
            paste0(nrow(gr), " ", input$detector, " detectors", 
                   "; ", "diameter ", lengthstr(glength), " (", ratio, " sigma)")
        }
        else ""
    })
    ##############################################################################
    
    output$fitPrint <- renderPrint({
        if (is.null(fitrv$value))
            invisible()
        else
            summary(fitrv$value)
    })
    ##############################################################################
    
    ## renderPlot
    
    ## arrayPlot
    ## routePlot
    ## detnPlot
    ## popPlot
    ## pxyPlot
    ## powerPlot
    ## RSEPlot
    ## nrmPlot
    ## costPlot
    
    ##############################################################################
    
    output$arrayPlot <- renderPlot( { # height = 340, width = 340, {
        tmpgrid <- detectorarray()
        ch <- capthist()
        if (is.null(tmpgrid)) return (NULL)
        par(mar = c(1,1,1,1), xpd = TRUE)
        plot (tmpgrid, border = border(1), bty='o', xaxs = 'i', yaxs = 'i',
                   gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
        
        if (!is.null(ch)) plot(ch, add = TRUE)
    })
    ##############################################################################
    
    output$detnPlot <- renderPlot( height = 290, width = 400, {
        ## inp <- oS2()
        invalidateOutputs()
        
        par(mar=c(4,5,2,5))
        detectfnplot (detectfn = input$detectfn,
                      pars = c(input$lambda0, input$sigma),
                      xval = 0:(3 * input$sigma),
                      ylab = "",
                      hazard = TRUE,       ## 2017-08-28
                      ylim = c(0, input$lambda0*1.2),
                      las=1, col = 'red', lwd = linewidth,
                      xaxs='i', yaxs='i')
        mtext(side = 2, line = 3.7, expression(paste("Detection hazard   ", lambda [0])))
        
        if (input$lambda0 <= 0.7) 
            p <- seq(0,1,0.05)
        else 
            p <- seq(0,1,0.1)
        
        axis(4, at = -log(1 - p), label = p, xpd = FALSE, las = 1)
        mtext(side = 4, line = 3.7, "Detection probability")
    })
    ##############################################################################
    
    output$popPlot <- renderPlot( height = 300, width = 380, {
        core <- detectorarray()
        if (is.null(core)) return (NULL)
        border <- input$buffer  # consistent with mask()
        tmppop <- pop()
        n <- if (is.null(tmppop)) 0 else nrow(tmppop)
        tmpsig <- sigma()
        #par(mar=c(0,1,0,1))
        if (input$pxyfillbox) {
            par(mar=c(0,1,0,5)) # , xaxs='i', yaxs='i')
        }
        else {
            par(mar=c(0,3,0,3), xaxs='i', yaxs='i', xpd = FALSE)
        }
        
        if (input$showmaskbox) {
            plot (core, border = border, gridlines = FALSE)
            plot (mask(), add = TRUE, col = grey(0.9), dots=F)
            if (n>0) plot(tmppop, add = TRUE, pch = 16, cex = 0.7, xpd = TRUE, frame = FALSE)
            plot (core, add = TRUE)
        }
        else {
            plot (core, border = border, gridlines = FALSE)
            if (n>0) plot(tmppop, pch = 16, cex = 0.7, xpd = TRUE, add = TRUE, frame = FALSE)
            plot (core, add = TRUE)
            bbox <- sweep(apply(core, 2, range), MAR=1, STATS = c(-border,+border), FUN="+")
            bbox <- expand.grid(data.frame(bbox))[c(1,2,4,3),]
            polygon(bbox)
        }
        if (input$showHRbox & (n>0)) {
            # rad <- rep(2.45 * tmpsig, nrow(tmppop))
            rad <- secr::circular.r(p = 0.95, detectfn = input$detectfn, sigma = tmpsig)
            symbols(tmppop$x, tmppop$y, circles = rep(rad, n),
                    inches = FALSE, fg = grey(0.7), add = TRUE, xpd = FALSE)
        }
    })
    ##############################################################################
    
    border <- function (multiple) {
        spc <- spacing(detectorarray()) 
        if (is.null(spc) || is.na(spc)) spc <- input$sigma
        multiple * spc
    }
    
    output$pxyPlot <- renderPlot( height = 300, width = 380, {
        core <- detectorarray()
        if (is.null(core)) return (NULL)
        invalidateOutputs()
        
        if (input$pxyfillbox) {
            cols <- terrain.colors(11)
            col <- cols[1]
            lev <- c(0.01, seq(0.1, 0.9, 0.1))
            par(mar=c(0,1,0,5)) # , xaxs='i', yaxs='i')
        }
        else {
            col <- "blue"
            cols <- NULL
            lev <- seq(0.1, 0.9, 0.1)
            par(mar=c(0,3,0,3)) # , xaxs='i', yaxs='i')
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
        
        pdot.contour(core, border = border, nx = input$pxynx,
                     detectfn = input$detectfn,
                     detectpar = list(sigma = input$sigma, lambda0 = input$lambda0),
                     noccasions = input$noccasions, drawlabels = drawlabels,
                     binomN = NULL, levels = lev, poly = poly(), 
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
            strip.legend("right", legend = seq(0,1,0.1), title = "p.(x)", xpd = TRUE,
                         legendtype='breaks', inset = 0.01, col = cols[3:12])
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
        ## text (110, 10, paste0("RSE = ", round(100*RSE,1), "%") )
    })
    ##############################################################################
    
    ## rendertable
    
    ##############################################################################
    
    output$summarytable <- renderTable({
        fields <- c(input$fields1, input$fields2)
        tmp <- t(sumrv$value[,fields])
        if (ncol(tmp)>0) colnames(tmp) <- paste0('Scenario', 1:ncol(tmp))
        tmp <- cbind(Field = fields, tmp)
        tmp } , spacing = "xs"
    )
    
    ##############################################################################
    
    ## downloadhandler
    
    ##############################################################################
    
    output$downloadSummary <- downloadHandler(
        filename = "summary.csv",
        content = function(file) {
            write.csv(sumrv$value, file, row.names = TRUE)
        }
    )
    
    output$downloadSummaryrds <- downloadHandler(
        filename = "summary.rds",
        content = function(file) {
            saveRDS(sumrv$value, file)
        }
    )
    
    output$downloadfitcode <- downloadHandler(
        filename = "fitcode.R",
        content = function(file) {
            fittext <- fitcode()
            cat(fittext, file = file)
        }
        , contentType = "text/R"
    )
   
    ##############################################################################
    # tidy end of session - app closes in R
    # ?apparently incompatible with bookmarking 2019-01-17
    
    # session$onSessionEnded(function() {
    #     stopApp()
    # })
    
    ##############################################################################

    setBookmarkExclude(c("fitbtn", "appendbtn",
                         "clearallbtn", "clearlastbtn", "selectnonebtn", "selectallbtn",
                         "resetbtn", 
                         "selectfieldsbtn", "selecting"))
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        shp <- sapply(input$habpolyfilename, shpfile)
        if (any(shp)) {
            showNotification("ESRI shapefile will not be bookmarked", type = "error", id = "noshapefile")
        }
        state$values$shp <- shp
        state$values$sumrv <- sumrv$value            # works
        state$values$fit <- fitrv$value
        state$values$port <- session$clientData$url_port
        ## can manually recover with e.g.
        ## readRDS('d:/density secr 3.2/secrapp/shiny_bookmarks/9c88715bacc260cf/values.rds')$port
    })    
    # Read values from state$values when we restore
    onRestore(function(state) {
        sumrv$value <- state$values$sumrv
        fitrv$value <- state$values$fit
        current$unit <- input$areaunit
        if (any(state$values$shp)) {
            showNotification("Cannot restore ESRI shapefile(s); re-select", type = "error", id = "noshapefile2")
        }
    })
    ##############################################################################
}

##################################################################################
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "server")
##################################################################################
