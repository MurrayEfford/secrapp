library(openCR)
library(shinyjs)

secrversion <- packageVersion('secr')
if (compareVersion(as.character(secrversion), '3.2.0') < 0)
    stop("secrapp 1.0 requires secr version 3.2.0 or later",
         call. = FALSE)
openCRversion <- packageVersion('openCR')

# for transfer to secrdesign
# designurl <- "http://127.0.0.1:4429/"    ## temporarily use 4429 local
designurl <- "https://www.stats.otago.ac.nz/secrdesignapp/"   # secrdesignapp 1.2 and above reads parameters

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
        useShinyjs(),
        tags$head(tags$style(".mypanel{margin-top:5px; margin-bottom:10px; padding-bottom: 5px;}")),
        tags$head(tags$style("#resultsPrint{color:blue; font-size:12px; overflow-y:scroll; min-height: 250px; max-height: 250px; background: ghostwhite;}")),
        tags$head(tags$style("#maskPrint{color:blue; font-size:12px; background: ghostwhite;}")),
        tags$head(tags$style(type="text/css", "input.shiny-bound-input { font-size:14px; height:30px}")),
        br(),
        navlistPanel(id = "navlist", widths = c(2,10), well=TRUE,
                     
                     "secr app 1.0",
                     
                     tabPanel("Main screen",
                              fluidRow(
                                  column (5, # offset = 0, style='padding:15px;',
                                          h2("Data input"),
                                          fluidRow(
                                              column(6,
                                                     wellPanel(class = "mypanel", 
                                                                  div(style="height: 80px;",
                                                                      # trick from Felipe Gerard 2019-01 to allow reset
                                                                      # https://stackoverflow.com/questions/44203728/how-to-reset-a-value-of-fileinput-in-shiny
                                                                      # uiOutput('trapfile_ui')),
                                                                      fileInput("trapfilename", "Trap layout",   # Detector layout file
                                                                                 accept = "text/plain")),
                                                                  helpText(HTML(paste0("trapID, X, Y"))),
                                                                  textInput("trapargs", "",
                                                                            value = "", placeholder = "other args e.g., skip = 1"),
                                                               radioButtons("detector", "Detector type", inline = TRUE,
                                                           choices = c("multi","proximity","count"),
                                                           selected = "multi")
                                                                  
                                              )),
                                              column(6, 
                                                     wellPanel(class = "mypanel", 
                                                                  div(style="height: 80px;",
                                                                      # uiOutput('captfile_ui')),
                                                                      fileInput("captfilename", "Captures",
                                                                                accept = c(".csv", ".txt", ".rdata",
                                                                                           ".rda", ".rds"))),
                                                                  uiOutput("captfilehelp"),
                                                                  
                                                                  textInput("captargs", "",
                                                                            value = "", placeholder = "other args e.g., skip = 1"),
                                                               radioButtons("fmt", label = "Format", inline = TRUE,
                                                                               choices = c("trapID", "XY"))
                                                                  
                                              )
                                              )
                                          ),
                                          h2("Model"),
                                          wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(3, radioButtons("detectfnbtn", "Detectfn",
                                                                             choices = c("HHN","HEX"),
                                                                             selected = "HHN")),
                                                       column(3, radioButtons("likelihoodbtn", "Likelihood", choices = c("Full", "Conditional"))),
                                                       column(3, radioButtons("distributionbtn", label = "Distribution of n",
                                                                              choices = c("Poisson", "Binomial"))),
                                                       column(3, radioButtons("packagebtn", label = "Function", 
                                                                              choices = c("secr.fit", "openCR.fit")))
                                                   ),
                                                   fluidRow(
                                                       column(12, textInput("model", "", value = "D~1, lambda0~1, sigma~1"))
                                                   ),
                                                   fluidRow(
                                                       column(12, textInput("otherargs", "", value = "", placeholder = "other args e.g., details"))
                                                   )
                                         ),
                                         
                                         
                                         h2("Actions"),
                                         fluidRow(
                                             column(4, actionButton("fitbtn", "Fit model",  width = 130,
                                                                    title = "Fit spatially explicit capture-recapture model to estimate density and update Results")),
                                             column(4, actionButton("appendbtn", "Add to summary",  width = 130,
                                                                    title = "Append new analysis to Summary table")),
                                             column(4, helpText(HTML("F11 to toggle fullscreen")))
                                         ),
                                         
                                         br(),
                                         fluidRow(
                                             column(4, actionButton("resetbtn", "Reset all", width = 130, 
                                                                    title = "Reset all inputs to initial values")),
                                             column(4, bookmarkButton(width = 130)),
                                             
                                             column(4, uiOutput("secrdesignurl"))  ## switch to secrdesign, with parameters
                                                    
                                         ),
                                         br(),
                                         fluidRow(
                                             column(11, textInput("title", "", value = "", 
                                                                  placeholder = "label for Summary")))
                                  ),
                                  
                                  column (6, # style='padding:0px;',
                                          h2("Results"),
                                                   fluidRow(
                                                       column(5, radioButtons("resultsbtn", label = "", 
                                                                               inline = TRUE, choices = c("summary", "derived", "other"))),
                                                       column(5, textInput("otherfunction", label="", placeholder = "e.g., vcov(fit)")),
                                                       conditionalPanel("output.modelFitted", column(2, br(), downloadLink("savebtn", "Save fit")))
                                                   ),
                                          
                                          fluidRow(
                                              column(12, 
                                                     
                                                     verbatimTextOutput("resultsPrint"))
                                          ),
                                          
                                          fluidRow(
                                              column(12,
                                                     br(),
                                                     tabsetPanel(type = "pills",
                                                                 id = "plottabs",
                                                                 tabPanel("Array",
                                                                          br(),
                                                                          fluidRow(
                                                                              column(9, style='padding:0px;', plotOutput("arrayPlot", 
                                                                                                                         click = clickOpts(id="arrayClick", clip = FALSE))),
                                                                              column(3, br(), conditionalPanel("input.gridlines != 'None'",
                                                                                                               uiOutput("uigridlines") ),
                                                                                     br(), uiOutput('xycoord'))
                                                                          ),
                                                                          fluidRow(
                                                                              column(2, offset = 1, checkboxInput("tracks", "All tracks", FALSE)),
                                                                              column(2, checkboxInput("varycol", "Vary colours", FALSE)),
                                                                              column(2, numericInput("animal", "Select animal", min = 0, max = 2000, 
                                                                                                     step = 1, value = 1)),
                                                                              column(2, br(), conditionalPanel("input.animal>0", uiOutput("uianimalID"))),
                                                                              column(2, conditionalPanel("output.multisession=='true'",
                                                                                            numericInput("sess", "Session", min = 1, max = 2000, 
                                                                                                     step = 1, value = 1)))
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
                                                                          plotOutput("pxyPlot", height = 350, click = "pxyclick"),
                                                                          #plotOutput("pxyPlot", click = "pxyclick"),
                                                                          helpText(HTML("p.(x) is the probability an animal at point x will be detected at least once")),
                                                                          fluidRow(
                                                                              column(3, checkboxInput("maskedge", "show mask edge", value = FALSE))
                                                                          )
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
                     
                     tabPanel("Habitat mask",
                              
                              fluidRow(
                                  column(4,
                                         

                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, 
                                                              numericInput("buffer", "Buffer width (m)",
                                                                           min = 0,
                                                                           max = 100000,
                                                                           value = 100,
                                                                           step = 5,
                                                                           width = 180),
                                                              numericInput("habnx", "Mesh dimension nx",
                                                                           min = 10,
                                                                           max = 1000,
                                                                           value = 32,
                                                                           step = 1,
                                                                           width = 180)
                                                       ),
                                                       column(6,
                                                              br(),
                                                              actionButton("suggestbuffer", "Suggest width", width = 130,
                                                                           title = "Based on either fitted model or RPSV"))
                                                   ),
                                                   fluidRow(
                                                        column(12, 
                                                               radioButtons("maskshapebtn", label = "Shape",
                                                                           choices = c("Rectangular", "Trap buffer"), 
                                                                           selected = "Trap buffer", inline = TRUE)
                                                        )
                                                   )
                                         ),
                                         wellPanel(class = "mypanel", 
                                                   br(),
                                                   div(style="height: 80px;",
                                                       fileInput("polyfilename", "Mask polygon file(s)",
                                                                 accept = c('.shp','.dbf','.sbn','.sbx',
                                                                            '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                                                                 multiple = TRUE)),
                                                   uiOutput("habitatfile"),
                                                   fluidRow(
                                                       column(10, 
                                                              checkboxInput("polygonbox", "Clip to polygon(s)", value = TRUE),
                                                              radioButtons("includeexcludebtn", label = "",
                                                                           choices = c("Include", "Exclude"), 
                                                                           selected = "Include", inline = TRUE)
                                                       )
                                                   )
                                         )
                                  ),
                                  column(4, plotOutput("maskPlot"),
                                         conditionalPanel ("output.trapsUploaded", fluidRow(
                                          column(3, offset = 1, checkboxInput("dotsbox", "dots", value = FALSE, width = 180)),
                                          column(3, offset = 1, checkboxInput("xpdbox", "xpd", value = FALSE, width = 180)),
                                          column(4, checkboxInput("maskedge2", "show mask edge", value = FALSE))
                                          ))   
                                         ),
                                  column(3, 
                                         h2("Summary"),
                                         fluidRow(
                                             column(12, 
                                                    verbatimTextOutput("maskPrint"))
                                         )
                                  )
                              )
                     ),
                                          
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
                                             h2("Analyses"),
                                             fluidRow(
                                                 column(6, actionButton("clearallbtn", "Clear all", title = "Delete all analyses")), 
                                                 column(6, actionButton("clearlastbtn", "Delete last", title = "Delete last analysis"))
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
                                                                                        choices = c("date", "time", "note", "traps", "captures", 
                                                                                                    "n", "r", "ndetectors", "noccasions",
                                                                                                    "usagepct", "maskbuffer", "masknrow", "maskspace"
                                                                                                    
                                                                                        ),
                                                                                        selected = c("date", "time", "note", "traps", "captures", 
                                                                                                     "n", "r", "ndetectors", "noccasions",
                                                                                                     "usagepct", "maskbuffer", "masknrow", "maskspace"
                                                                                                     
                                                                                        )
                                                                     ))),
                                             column(6,
                                                    conditionalPanel("output.selectingfields == 'TRUE'",
                                                                     checkboxGroupInput("fields2", "",
                                                                                        choices = c("likelihood", "distribution", "model", "detectfn", 
                                                                                                    "fitfunction", "npar", "logLik", "AIC",
                                                                                                    "D", "se.D", "RSE.D", "lambda0", "se.lambda0", "sigma", "se.sigma",
                                                                                                    "k", "proctime"
                                                                                        ),
                                                                                        selected = c("likelihood", "distribution", "model", "detectfn",
                                                                                                     "fitfunction", "npar", "logLik", "AIC",
                                                                                                     "D", "se.D", "RSE.D", "lambda0", "se.lambda0", "sigma", "se.sigma",
                                                                                                     "k", "proctime"
                                                                                        )
                                                                     )
                                                    )
                                             )
                                         )
                                         
                                  ),
                                  column(9, 
                                         # h2("Results"),
                                         div(tableOutput("summarytable"), style = "width:800px; overflow-x: scroll")
                                  )
                              )
                     ),
                     #################################################################################################

                     tabPanel("Options",
                              
                              fluidRow(
                                  column(3,
                                         
                                         h2("Detector array"),
                                         # wellPanel(class = "mypanel", 
                                         #           fluidRow(
                                         #               column(6, radioButtons("areaunit", label = "Area units",
                                         #                                      choices = c("ha", "km^2"), 
                                         #                                      selected = "ha", inline = TRUE))
                                         #           )
                                         # ),
                                         h2("Model fitting"),
                                         wellPanel(class = "mypanel",
                                                   fluidRow(
                                                       column(12, selectInput("method", "Maximization method",
                                                                   choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                                                                   selected = "Newton-Raphson", width=160))
                                                   )
                                         ),
                                         h2("Summary"),
                                         wellPanel(class = "mypanel",
                                                   fluidRow(
                                                       column(12, numericInput("dec", "Decimal places", min = 0, max = 8, value = 4, width=160))
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
                                                   ),
                                                   br(),
                                                   fluidRow(
                                                       column(6, numericInput("rad", "Radial displ. (m)", 
                                                                               value = 5, min = 0, max = 5000, step = 1, width = 160)),
                                                       column(6, numericInput("cex", "Point size (cex)", value = 1, 
                                                                               min = 0.1, max = 5, step = 0.1, width = 160))
                                                       )
                                                   
                                         ),
                                         h2("Pxy contour plot"),
                                         wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(6, numericInput("pxyborder", "Border (spacing units)",
                                                                              min = 0,
                                                                              max = 10,
                                                                              value = 5,
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
                              h5("Copyright 2019 Murray Efford"),
                              "The application is released under the ",
                              a("GNU General Public License Version 3.0", href="https://www.gnu.org/licenses/gpl-3.0.txt", target="_blank"), br(),
                              br(),
                              h5("For further information see "), 
                              a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
                              a("CRAN.R-project.org/package=secr", href="https://CRAN.R-project.org/package=secr", target="_blank"), br(),
                              a("https://github.com/MurrayEfford/secrapp", href="https://github.com/MurrayEfford/secrapp", target="_blank"), br(),
                              br(),
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
    summaryfields <- c("date", "time", "note", "traps", "captures", "n", "r",
                       "ndetectors", "noccasions", "usagepct", "maskbuffer", "masknrow", "maskspace",
                       "likelihood", "distribution", "model", 
                       "fitfunction", "detectfn", "npar", "logLik", "AIC",
                       "D", "se.D", "RSE.D", "lambda0", "se.lambda0", "sigma", "se.sigma", 
                       "k", "proctime"
                       )
    
    fieldgroup1 <- 1:13
    fieldgroup2 <- 14:30

    ## for cycling through animals at one detector 2019-03-08
    lasttrap <- 0
    clickno <- 0
    
    showNotification(paste("secr", desc$Version, desc$Date),
                     closeButton = FALSE, type = "message", duration = seconds)
     output$selectingfields <- renderText('false')
     output$multisession <- renderText('false')
     outputOptions(output, "selectingfields", suspendWhenHidden = FALSE)
     outputOptions(output, "multisession", suspendWhenHidden = FALSE)

     output$trapsUploaded <- reactive({
         return(!is.null(traprv$data))
     })
     
     output$modelFitted <- reactive({
         return(!is.null(fitrv$value))
     })
     
     outputOptions(output, "trapsUploaded", suspendWhenHidden=FALSE)
     outputOptions(output, "modelFitted", suspendWhenHidden=FALSE)

     
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
    
    output$secrdesignurl <- renderUI ({
        
        # only show after model fitted
        req(fitrv$value)
        
         parm <- c(
             paste0("detectfnbtn=", input$detectfnbtn),
             paste0("distributionbtn=", input$distributionbtn),
             paste0("detector=", input$detector)
         )
         
         if (!is.null(input$trapfilename)) {
             parm <- c(parm,
                       #paste0("trapfilename=", input$trapfilename),
                       paste0("trapargs=", input$trapargs))
         }

         if (!is.null(input$captfilename)) {
             parm <- c(parm,
                       paste0("noccasions=", as.character(noccasions())))
         }
         
         if (!is.null(fitrv$value)) {
             parm <- c(parm,
                       paste0("D=", as.character(round(density(), input$dec))),
                       paste0("lambda0=", as.character(round(lambda0(), input$dec))),
                       paste0("sigma=", as.character(round(sigma(), input$dec))))
         }
         
         parm <- paste(parm, collapse = "&")
         # open secrdesignapp in a new tab, with parameters from secrapp
         # designurl is set at top of this file
         tags$a(href =  paste0(designurl, "?", parm), "Switch to secrdesign", target="_blank")  
     })
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
    
    output$captfilehelp <- renderUI({
        helptext <- ""
     if (input$fmt == "trapID")
         helptext <- "Session, ID, Occasion, TrapID"
     else
         helptext <- "Session, ID, Occasion, X, Y"
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
        if (!is.null(input$polyfilename)) {
            pos <- grep(".shp", tolower(input$polyfilename[,1]))
            if (length(pos)>0)
                helptext <- paste0(input$polyfilename[pos,1])
            pos <- grep(".rda", tolower(input$polyfilename[,1]))  # .rda, .rdata
            if (length(pos)>0) {
                objlist <- load(input$polyfilename[1,4])
                helptext <- paste0(objlist[1])
            }
            pos <- grep(".rds", tolower(input$polyfilename[,1])) 
            if (length(pos)>0) {
                helptext <- paste0(input$polyfilename[pos,1])
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
    
    output$uianimalID <- renderUI({
        if(is.na(input$animal) || input$animal<=0 || is.null(nsessions()))
            helpText("")
        else {
            if (nsessions()>1) {
                ch <- capthist()[[input$sess]]
            }    
            else {
                ch <- capthist()
            }
            
            covar <- covariates(ch)[input$animal,,drop = FALSE]
            if (length(covar)>0)
                covar <- paste(lapply(1:length(covar), function(i) 
                    paste(names(covar)[i], as.character(covar[[i]]))), collapse = ', ')
            helpText(paste("ID", rownames(ch)[input$animal], ' ', covar))
        }
    })
    ##############################################################################
    
    output$xycoord <- renderUI({
        xy <- c(input$arrayClick$x, input$arrayClick$y)
        tmpgrid <- isolate(traprv$data)
        if (is.null(xy)) 
            helpText("")
        else {
            nearest <- nearesttrap(xy, tmpgrid)
            #-----------------------------------------------------
            ## machinery to cycle through animals at this detector
            if (lasttrap != nearest) clickno <<- 0
            clickno <<- clickno + 1
            lasttrap <<- nearest
            at.xy <- apply(capthist()[,,nearest, drop = FALSE],1,sum)
            at.xy <- which(at.xy>0)
            clickno <<- ((clickno-1) %% length(at.xy)) + 1
            #-----------------------------------------------------
            if (length(at.xy)>0) {
                updateNumericInput(session, "animal", value = as.numeric(at.xy[clickno]))
            }
            if (input$snaptodetector) {
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
    
    ## modelstring
    
    ##############################################################################
    
    modelstring <- function () {
        form <- strsplit(input$model, ",")[[1]]
        fn <- function(f) {
            chf <- as.character(eval(parse(text=f)))
            if (chf[3]=="1") "" else f
        }
        out <- sapply(form, fn)
        out <- out[out != ""]
        if (length(out)==0) "~1" else paste0(out, collapse = ", ")
    }
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

    # readpolygon <- function (fileupload) {
    #     poly <- NULL
    #     if (!is.null(fileupload) & is.data.frame(fileupload))
    #     {
    #         if (!file.exists(fileupload[1,4])) {
    #             return(NULL)   ## protect against bad shapefile
    #         }
    #         ext <- tolower(tools::file_ext(fileupload[1,1]))
    #         if (ext == "txt") {
    #             coord <- read.table(fileupload[1,4])
    #             poly <- secr:::boundarytoSP(coord[,1:2])
    #         }
    #         else if (ext %in% c("rdata", "rda", "rds")) {
    #             if (ext == "rds") {
    #                 obj <- readRDS(fileupload[1,4])
    #             }
    #             else {
    #                 objlist <- load(fileupload[1,4])
    #                 obj <- get(objlist[1])
    #             }
    #             if (is.matrix(obj))
    #                 poly <- secr:::boundarytoSP(obj[,1:2])
    #             else if (inherits(obj, "SpatialPolygons"))
    #                 poly <- obj
    #             else stop("unrecognised boundary object in ", objlist[1])
    #         }
    #         else {
    #             ## not working on restore bookmark 2019-01-24
    #             dsnname <- dirname(fileupload[1,4])
    #             for ( i in 1:nrow(fileupload)) {
    #                 file.rename(fileupload[i,4], paste0(dsnname,"/",fileupload[i,1]))
    #             }
    #             filename <- list.files(dsnname, pattern="*.shp", full.names=FALSE)
    #             layername <- tools::file_path_sans_ext(basename(filename))
    #             if (is.null(filename) || 
    #                 !(any(grepl(".shp", fileupload[,1])) &&
    #                   any(grepl(".dbf", fileupload[,1])) &&
    #                   any(grepl(".shx", fileupload[,1])))) {
    #                 showNotification("need shapefile components .shp, .dbf, .shx",
    #                                  type = "error", id = "nofile", duration = seconds)
    #             }
    #             else  if (!requireNamespace("rgdal"))
    #                 showNotification("need package rgdal to read shapefile", type = "error", id = "norgdal", duration = seconds)
    #             else {
    #                 removeNotification(id = "nofile")
    #                 removeNotification(id = "norgdal")
    #                 poly <- rgdal::readOGR(dsn = dsnname, layer = layername, verbose = FALSE)
    #             }
    #         }
    #     }
    #     poly   
    # }
    # ##############################################################################

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

                if (!(any(grepl(".shp", fileupload[,1])) &&
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
                    ## not working on restore bookmark 2019-01-24
                    dsnname <- dirname(fileupload[1,4])
                    ## make temp copy with uniform layername
                    file.copy(from = fileupload[,4], 
                              to = paste0(dsnname, "/temp.", tools::file_ext(fileupload[,4])),     
                              overwrite = TRUE)
                    layername <- "temp"
                    poly <- rgdal::readOGR(dsn = dsnname, layer = layername)
                }
            }
        }
        poly   
    }
    ##############################################################################


    addtosummary <- function() {
        ## input$fields is character vector of selected fields
        
        df <- data.frame(
            date = format(Sys.time(), "%Y-%m-%d"),
            time = format(Sys.time(), "%H:%M:%S"),
            note = input$title,
            traps = if (is.null(traprv$data)) "" else input$trapfilename$name[1],
            captures = if (is.null(captrv$data)) "" else input$captfilename$name[1],
            ndetectors = ndetectors()[input$sess],
            noccasions = noccasions()[input$sess],
            usagepct = usagepct()[input$sess],
            maskbuffer = input$buffer,
            masknrow = masknrow()[input$sess],
            maskspace = round(maskspace()[input$sess], 1),
            n = n(),
            r = r(),
            likelihood = input$likelihoodbtn,
            distribution = input$distributionbtn,
            model = modelstring(), # input$model,
            detectfn = input$detectfnbtn,
            fitfunction = "",
            npar = NA,
            logLik = NA,
            AIC = NA,
            D = NA, 
            se.D = NA, 
            RSE.D = NA,
            lambda0 = NA, 
            se.lambda0 = NA,
            sigma = NA, 
            se.sigma = NA,
            k = NA,
            proctime = NA
        )
        if (inherits(fitrv$value, c("secr", "openCR"))) {
            fitsum <- summary(fitrv$value)
            df$fitfunction <- input$packagebtn
            df$npar <- fitsum$AICtable$npar
            df$logLik <- fitsum$AICtable$logLik
            df$AIC <- fitsum$AICtable$AIC
            df$D <- density()
            df$se.D <- se.density()
            df$RSE.D <- se.density() / density()
            df$lambda0 <- lambda0()
            df$se.lambda0 <- se.lambda0()
            df$sigma <- sigma()
            df$se.sigma <- se.sigma()
            if (input$detectfnbtn=="HHN")
                df$k <- density()^0.5 * sigma() / 100
            else 
                df$k <- NA*1  # force numeric NA
            df$proctime <- fitrv$value$proctime
            df[,20:30] <- ifelse (sapply(df[,20:30], is.numeric), round(df[,20:30], input$dec), df[,20:30])
        }
            
        sumrv$value <- rbind (sumrv$value, df)
        rownames(sumrv$value) <- paste0("Analysis", 1:nrow(sumrv$value))
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
        if (!is.null(traprv$data)) {
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
        
        if (input$polygonbox && !is.null(input$polyfilename)) { 
            polyhabitat <- input$includeexcludebtn == "Include"
            polycode <- getSPcode(input$polyfilename, "poly", input$polygonbox)
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
        detfn <- input$detectfnbtn
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
        
        if (input$likelihoodbtn == "Full") {
            type <- "secrD"
            CL <- FALSE
        }
        else {
            type <- "secrCL"
            CL <- TRUE
        }
        model <- eval(parse(text = paste0("list(", input$model, ")")))

        otherargs <- try(eval(parse(text = paste0("list(", input$otherargs, ")"))))
        if (inherits(otherargs, "try-error")) {
            showNotification("model fit failed - check other arguments",
                             type = "error", id = "nofit", duration = seconds)
            fit <- NULL
        }
        else {
            args <- c(list(capthist = capthist(), 
                           trace = FALSE,
                           mask = mask(), 
                           model = model,
                           detectfn = input$detectfnbtn),
                      otherargs)
            if (input$packagebtn == "openCR.fit") {
                args$type <- type
                args$distribution <-  tolower(input$distributionbtn)
                args$details <- list(multinom = TRUE)
            }
            else if (input$packagebtn == "secr.fit") {
                args$CL <- CL 
                args$details <- as.list(replace (args$details, "distribution", input$distributionbtn))
            }
            
            isolate(fit <- try(do.call(input$packagebtn, args)))
            if (inherits(fit, "try-error")) {
                showNotification("model fit failed - check data, formulae and mask",
                                 type = "error", id = "nofit", duration = seconds)
                fit <- NULL
            }
        }
        fitrv$value <- fit
        addtosummary()
    }
    ##############################################################################

    maskOK <- function () {
        if (!input$polygonbox || is.null(polyrv$data) || is.null(traprv$data)) {
            TRUE
        }
        else {
            sum(pointsInPolygon(traprv$data, polyrv$data)) > 0
        }
    }
    ##############################################################################
    
    ## Modal dialogue to confirm fitting if it might take a long time
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
    
    # readtrapfile <- function () {
    #     inFile <- input$trapfilename
    #     trps <- NULL
    #     if (!is.null(inFile)) {
    #         filename <- input$trapfilename[1,"datapath"]
    #         if (is.null(filename))
    #             stop("provide valid filename")
    #         args <- input$trapargs
    #         if (args != "")
    #             args <- paste0(", ", args)
    #         readtrapscall <- paste0("read.traps (filename, detector = input$detector", args, ")")
    #         trps <- try(eval(parse(text = readtrapscall)))
    #         if (!inherits(trps, "traps")) {
    #             showNotification("invalid trap file or arguments; try again",
    #                              type = "error", id = "badarray", duration = seconds)
    #         }
    #     }    
    #     trps
    # }
    # ##############################################################################

    # readcaptfile <- function () {
    #     inFile <- input$captfilename
    #     captdf <- NULL
    #     if (!is.null(inFile)) {
    #         filename <- input$captfilename[1,"datapath"]
    #         if (is.null(filename))
    #             stop("provide valid filename")
    #         args <- input$captargs
    #         if (args != "")
    #             args <- paste0(", ", args)
    #         readcaptcall <- paste0("read.table (filename", args, ")")
    #         captdf <- try(eval(parse(text = readcaptcall)))
    #         if (!inherits(captdf, "data.frame")) {
    #             showNotification("invalid capture file or arguments; try again",
    #                              type = "error", id = "badcapt", duration = seconds)
    #             captdf <- NULL
    #         }
    #     }    
    #     captdf
    # }
    # ##############################################################################
    
    ## reactive

    # capthist 
    # density 
    # derivedresult 
    # detectorarray
    # invalidateOutputs 
    # lambda0 
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
    # se.lambda0 
    # se.sigma 
    # sigma 
    # usagepct 

    ##############################################################################
    
    ## using advice of Joe Cheng 2018-03-23 to allow resetting of fileInputs
    ## https://stackoverflow.com/questions/49344468/resetting-fileinput-in-shiny-app
    
    captrv <- reactiveValues(
        data = NULL,
        clear = FALSE
    )
    
    traprv <- reactiveValues(
        data = NULL,
        clear = FALSE
    )
    
    polyrv <- reactiveValues(
        data = NULL,
        clear = FALSE
    )

    ##############################################################################

    ## read trap file
    observe({
        req(input$trapfilename)
        req(!traprv$clear)
        filename <- input$trapfilename[1,"datapath"]
        if (is.null(filename))
            stop("provide valid filename")
        args <- input$trapargs
        if (args != "")
            args <- paste0(", ", args)
        readtrapcall <-  paste0("read.traps (filename, detector = input$detector", args, ")")
        traprv$data <- try(eval(parse(text = readtrapcall)))
        if (!inherits(traprv$data, "traps")) {
            showNotification("invalid trap file or arguments; try again",
                             type = "error", id = "badtrap", duration = seconds)
            traprv$data <- NULL
        }
    })
    ##############################################################################
    
    ## read capture file
    observe({
        req(input$captfilename)
        req(!captrv$clear)
        filename <- input$captfilename[1,"datapath"]
        if (is.null(filename))
            stop("provide valid filename")
        args <- input$captargs
        if (args != "")
            args <- paste0(", ", args)
        readcaptcall <- paste0("read.table (filename", args, ")")
        captrv$data <- try(eval(parse(text = readcaptcall)))
        if (!inherits(captrv$data, "data.frame")) {
            showNotification("invalid capture file or arguments; try again",
                             type = "error", id = "badcapt", duration = seconds)
            captrv$data <- NULL
        }
    })
    ##############################################################################

    ## read mask polygon file
    observe({
        req(input$polyfilename)
        req(!polyrv$clear)
        filename <- input$polyfilename[1,"datapath"]
        
        polyrv$data <- readpolygon(input$polyfilename)
        
        if (!inherits(polyrv$data, "SpatialPolygons")) {
            showNotification("invalid polygon file; try again",
                             type = "error", id = "badpoly", duration = seconds)
            polyrv$data <- NULL
        }
    })
    ##############################################################################

    observeEvent(input$trapfilename, {
        traprv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$captfilename, {
        captrv$clear <- FALSE
    }, priority = 1000)
    
    
    observeEvent(input$polyfilename, {
        polyrv$clear <- FALSE
    }, priority = 1000)
    
    ##############################################################################
    
    capthist <- reactive( {
        if (is.null(traprv$data) || is.null(captrv$data)) {
            updateNumericInput(session, "animal", max = 0)
            NULL
        }
        else {
            ch <- try(suppressWarnings(make.capthist(captrv$data, traprv$data, fmt = input$fmt))) 
            if (inherits(ch, 'try-error')) {
                showNotification("invalid capture file or arguments; try again",
                                 type = "error", id = "badcapt", duration = seconds)
                showNotification(ch, type = "error", id = "capterror", duration = seconds)
                ch <- NULL
            }
            else {
                if (ms(ch)) {
                    showNotification("multisession data - may cause problems", 
                                     type = "warning", id = "mswarning", duration = seconds)
                    updateNumericInput(session, "animal", max = nrow(ch[[input$sess]]))
                     output$multisession <- renderText("true")
                }
                else {
                    updateNumericInput(session, "animal", max = nrow(ch))
                     output$multisession <- renderText("false")
                }
                updateNumericInput(session, "sess", max = length(ch))
            }
            ch
        }
    })
    ##############################################################################

    density <- reactive( {
        if (!inherits(fitrv$value, c("secr", "openCR"))) {
            NA
        }
        else {
            newd <- newdata()[1,,drop = FALSE]
            if (input$likelihoodbtn == "Full") {
                if (input$packagebtn == 'secr.fit') {
                    D <- predict(fitrv$value, newdata = newd)['D', 'estimate']
                }
                else {
                    D <- predict(fitrv$value, newdata = newd)[['superD']][1,'estimate']
                }
            }
            else {
                if (input$packagebtn == 'secr.fit') {
                    if (nsessions()==1)
                        D <- derivedresult()['D', 'estimate']
                    else
                        D <- derivedresult()[[input$sess]]['D', 'estimate']
                }
                else {
                    D <- NA ## derivedresult()['superD','estimate']
                }
                
            }
            D
        }
    })
    ##############################################################################

    derivedresult <- reactive({
        if (input$packagebtn == "openCR.fit") {
             showNotification("no derived method for openCR closed models",
                             type = "warning", id = "noderived", duration = seconds)
         
            NULL
            }
        else {
            progress <- Progress$new(session, min = 1, max = 15)
            on.exit(progress$close())
            progress$set(message = 'Computing derived estimates ...', detail = '')
            der <- derived(fitrv$value, distribution = tolower(input$distributionbtn))
            round(der, input$dec)
        }
    })
    ##############################################################################

    # detectorarray <- reactive(
    #     {
    #         pxyrv$value <- NULL
    #         traprv$data
    #     }
    # )
    ##############################################################################

    invalidateOutputs <- reactive({
        pxyrv$value <- NULL
        # updateNumericInput(session, "D", step = 10^trunc(log10(density()/50)))
    })
    
    ##############################################################################
    
    lambda0 <- reactive({
        if (!inherits(fitrv$value, c("secr", "openCR")))
            NA
        else {
            if (input$packagebtn == 'secr.fit')
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])['lambda0','estimate']
                # detectpar(fitrv$value)[['lambda0']]
            else
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])$lambda0[1,'estimate']
        }
    })
    ##############################################################################
    
    mask <- reactive( {
        pxyrv$value <- NULL
        if (is.null(traprv$data))
            NULL
        else {
            if (!maskOK()) showNotification("no detectors in habitat polygon(s)",
                                            type = "warning", id = "notrapsinpoly",
                                            duration = seconds)
            msk <- make.mask (traprv$data,
                              buffer = input$buffer,
                              nx = input$habnx,
                              type = if (input$maskshapebtn=='Rectangular') 'traprect' else 'trapbuffer',
                              poly = if (input$polygonbox) polyrv$data else NULL,
                              poly.habitat = input$includeexcludebtn == "Include",
                              keep.poly = FALSE)
            if (nrow(msk) > 10000) {
                showNotification(paste0(nrow(msk), " mask rows is excessive; reduce nx"),
                                            type = "warning", id = "maskrows",
                                            duration = seconds)
            }
            else {
                removeNotification(id = "maskrows")
            }
            msk
        }
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
        if (is.null(fitrv$value))
            NULL   ## no model
        else {
            if (input$packagebtn == "secr.fit")
                secr.make.newdata(fitrv$value, all.levels = TRUE)
            else {
                openCR.make.newdata(fitrv$value, all.levels = TRUE)
            }
        }
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
            core <- traprv$data
            if (is.null(core) || (density() == 0) || is.na(density())) {
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
        if (!inherits(fitrv$value, c("secr", "openCR")) || (input$likelihoodbtn != "Full") )
            return (NULL)
        else {
            if (input$packagebtn == 'secr.fit')
                V <- vcov(fitrv$value)['D','D']   ## FAILS IF HAVE JUST SWITCHED BUTTON
            else
                V <- vcov(fitrv$value)['superD','superD']
            sqrt(exp(V)-1) * 100
        }
    })
    ##############################################################################

    se.density <- reactive( {
        if (!inherits(fitrv$value, c("secr", "openCR"))) {
            NA
        }
        else {
            newd <- newdata()[1,,drop = FALSE]
            if (input$likelihoodbtn == "Full") {
                if (input$packagebtn == 'secr.fit') {
                    se.D <- predict(fitrv$value, newdata = newd)['D', 'SE.estimate']
                }
                else {
                    se.D <- predict(fitrv$value, newdata = newd)[['superD']][1,'SE.estimate']
                }
            }
            else {
                if (input$packagebtn == 'secr.fit') {
                    if (nsessions()==1)
                        se.D <- derivedresult()['D', 'SE.estimate']
                    else
                        se.D <- derivedresult()[[input$sess]]['D', 'SE.estimate']
                        
                }
                else {
                    se.D <- NA ## derivedresult()['superD','SE.estimate']
                }
                
            }
            se.D
        }
    })
    ##############################################################################

    se.lambda0 <- reactive({
        if (!inherits(fitrv$value, c("secr", "openCR")))
            NA
        else {
            if (input$packagebtn == 'secr.fit')
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])['lambda0','SE.estimate']
                # detectpar(fitrv$value)[['lambda0']]
            else
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])$lambda0[1,'SE.estimate']
        }
    })
    ##############################################################################
    
    se.sigma <- reactive ({
        if (!inherits(fitrv$value, c("secr", "openCR")))
            NA
        else {
            if (input$packagebtn == 'secr.fit')
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])['sigma','SE.estimate']
                # detectpar(fitrv$value)[['sigma']]
            else 
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])$sigma[1,'SE.estimate'] 
        }
    })
    ##############################################################################
    
    sigma <- reactive ({
        if (!inherits(fitrv$value, c("secr", "openCR")))
            NA
        else {
            if (input$packagebtn == 'secr.fit')
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])['sigma','estimate']
                # detectpar(fitrv$value)[['sigma']]
            else 
                predict(fitrv$value, newdata = newdata()[1,,drop = FALSE])$sigma[1,'estimate'] 
        }
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

    ## reactiveValues
    
    arrrv <- reactiveValues(v = 0)  # used to invalidate and re-plot detectorarray
    current <- reactiveValues(unit = "ha")
    fitrv <- reactiveValues(value = NULL)
    poprv <- reactiveValues(v = 0)  # used to invalidate and re-plot popn
    pxyrv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
    RSErv <- reactiveValues(current = FALSE, value = NULL, adjRSE = NULL)
    selecting <- reactiveValues(v=FALSE)
    sumrv <- reactiveValues(value = read.csv(text = paste(summaryfields, collapse = ", ")))
    
    ##############################################################################
    
    ## observeEvent

    # alpha
    # appendbtn
    # areaunit
    # CIclick
    # clearallbtn
    # clearlastbtn
    # detectfnbtn
    # distributionbtn
    # fitbtn
    # likelihoodbtn
    # model
    # okbtn
    # otherargs
    # packagebtn
    # pxyclick
    # randompopbtn
    # resetbtn
    # selectallbtn
    # selectfieldsbtn
    # selectnonebtn
    # suggestbuffer
    
    ## Invalidate results when model specification changes
    # likelihoodbtn
    # packagebtn
    # detectfnbtn
    # distributionbtn
    # model
    # otherarg

    ##############################################################################

    observeEvent(input$alpha, {
        updateCheckboxInput(session, "powertype", label = paste0(
            round(100 *(1-input$alpha), 1), "% CI"))
    })
    ##############################################################################

    observeEvent(input$appendbtn, {
        if (!is.null(traprv$data))
            addtosummary()
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

    observeEvent(input$clearallbtn, {
        sumrv$value <- sumrv$value[0,]
    }   )
    
    ##############################################################################
    
    observeEvent(input$clearlastbtn, {
        if (nrow(sumrv$value)>0)
            sumrv$value <- sumrv$value[-nrow(sumrv$value),]
    }   )
    
    ##############################################################################
    
    observeEvent(input$detectfnbtn, {
        fitrv$value <- NULL
    })
    ##############################################################################

    observeEvent(input$distributionbtn, {
        fitrv$value <- NULL
    })
    ##############################################################################

    observeEvent(input$fitbtn, ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
        if (is.null(capthist())) {
             showNotification("load data",
                             type = "warning", id = "nodata", duration = seconds)
        }
        else {
            progress <- Progress$new(session, min = 1, max = 15)
            on.exit(progress$close())
            progress$set(message = 'Fitting...',
                         detail = '')
            methodfactor <- 1 + ((input$method != "none") * 4)
            functionfactor <- switch(input$packagebtn, secr.fit = 4, openCR.fit = 1, 0.1)
            detectorfactor <- switch(input$detector, proximity = 1, single = 0.6, multi = 0.6, count = 4)
           
            if (ms(capthist())) {
                nm <- if (ms(mask())) nrow(mask()[[1]]) else nrow(mask())
                nt <- if (ms(traprv$data)) nrow(traprv$data[[1]]) else nrow(traprv$data)
                time <- nm * nt / 4.5e9 * ## blocked 2019-01-14 nrepeats() * 
                    noccasions()[1] * length(capthist()) *
                    methodfactor * functionfactor * detectorfactor
            }
            else {
                time <- nrow(mask()) * nrow(traprv$data) / 4.5e9 * ## blocked 2019-01-14 nrepeats() * 
                    noccasions() * 
                    methodfactor * functionfactor * detectorfactor
            }
            ## 2019-03-13 add safety margin for server
            ## does not yet use nrow(capthist)?
            ## need more comprehensive empirical approximation
            time <- time * 2
            if (time > 0.2)
                showModal(OKModal(time))
            else {
                fitmodel()
            }
        }
    })
    
    ##############################################################################
    
    observeEvent(input$likelihoodbtn, ignoreInit = TRUE, {
        fitrv$value <- NULL
        ## drop or add density formula depending on full/conditional likelihood
        if (input$likelihoodbtn == "Full") {
            if (input$packagebtn == "secr.fit")
                updateTextInput(session, "model", value = paste0("D~1, ", input$model))
            else
                updateTextInput(session, "model", value = paste0("superD~1, ", input$model))
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

    observeEvent(input$otherargs, {
        fitrv$value <- NULL
    })
    ##############################################################################

    observeEvent(input$packagebtn, {
        fitrv$value <- NULL

        ## toggle D/superD
        if (input$likelihoodbtn == "Full") {
            form <- strsplit(input$model, ",")[[1]]
            form <- stringr::str_trim(form)
            if (input$packagebtn == "secr.fit") {
                    form <- gsub("superD", "D", form)
            }
            else {
                form <- gsub("D", "superD", form)
            }
            newmodel <- paste(form, collapse = ", ")
            updateTextInput(session, "model", value = newmodel)
        }
    })
    ##############################################################################

    observeEvent(input$pxyclick, {
        invalidateOutputs()
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
            Pxy <- pdot (xy, trps,
                         detectfn = input$detectfnbtn,
                         detectpar = list(lambda0 = lambda0(), sigma = sigma()),
                         noccasions = noccasions()[input$sess])
            pxyrv$xy <-xy
            pxyrv$value <- Pxy}
    })
    ##############################################################################

    observeEvent(input$randompopbtn, ignoreInit = TRUE, {
        # invalidates pop when button pressed
        poprv$v <- poprv$v + 1
    })
    
    ##############################################################################

    observeEvent(input$resetbtn, {
        
        current$unit <- "ha"
        fitrv$value <- NULL
        
        ## Data input

        ## Trap layout
        updateTextInput(session, "trapargs", 
                        value = "", placeholder = "other args e.g., skip = 1")
        updateRadioButtons(session, "detector", selected = "multi")

        ## Captures
        updateTextInput(session, "captargs", 
                        value = "", placeholder = "other args e.g., skip = 1")
        updateRadioButtons(session, "fmt", selected = "trapID")

        ## Model
        
        updateRadioButtons(session, "detectfnbtn", selected = "HHN")
        updateRadioButtons(session, "distributionbtn", selected = "Poisson")
        updateRadioButtons(session, "likelihoodbtn", selected = "Full")
        updateRadioButtons(session, "packagebtn", selected = "secr.fit")

        updateTextInput(session, "model", 
                        value = "D~1, lambda0~1, sigma~1", placeholder = "")
        updateTextInput(session, "otherargs", 
                        value = "", placeholder = "other args e.g., details")
        
        ## Actions
        updateTextInput(session, "title", "", value = "",
                        placeholder = "label for Summary")
        updateTextInput(session, "otherfunction", 
                        value = "", placeholder = "e.g., vcov(fit)")

        
        ## Results
        updateRadioButtons(session, "resultsbtn", selected = "summary")

        ## Array plot
        updateCheckboxInput(session, "tracks", value = FALSE)
        updateCheckboxInput(session, "varycol", value = FALSE)
        updateNumericInput(session, "animal", value = 1)
        updateNumericInput(session, "sess", value = 1)

        ## pop plot
        updateCheckboxInput(session, "showHRbox", "Display 95% home range", value = FALSE)
        updateCheckboxInput(session, "showmaskbox", "Display mask", value = FALSE)
        updateCheckboxInput(session, "onlymaskbox", "Restrict to mask", value = TRUE)

        ## pxy plot
        updateCheckboxInput(session, "maskedge", value = FALSE)

        ## power plot
        updateCheckboxInput(session, "adjustRSEbox", value = TRUE)
        updateCheckboxInput(session, "powertype", "95% CI", value = TRUE)
        updateNumericInput(session, "xpos", value = 0)

        ## Habitat mask

        updateNumericInput(session, "buffer", value = 100)
        updateNumericInput(session, "habnx", value = 32)
        updateRadioButtons(session, "maskshapebtn", selected = "Trap buffer")
        updateCheckboxInput(session, "polygonbox", value = TRUE)
        updateCheckboxInput(session, "exclusionbox", value = TRUE)
        updateRadioButtons(session, "includeexcludebtn", selected = "Include")

        updateCheckboxInput(session, "dotsbox", value = FALSE)
        updateCheckboxInput(session, "xpdbox", value = FALSE)
        updateCheckboxInput(session, "maskedge2", value = FALSE)

        ## Summary

        # safer to leave this for manual reset using Summary page buttons        
        # updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
        # updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])
        # sumrv$value <- sumrv$value[0,]
        
        ## Options

        ## detector array
        updateRadioButtons(session, "areaunit", selected = "ha")

        updateNumericInput(session, "dec", value = 4)
        
        ## array plot
        updateCheckboxInput(session, "entireregionbox", value = TRUE)
        updateCheckboxInput(session, "snaptodetector", value = FALSE)
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

        captrv$data <- NULL
        captrv$clear <- TRUE
        reset('captfilename')
        
        polyrv$data <- NULL
        polyrv$clear <- TRUE
        reset('polyfilename')
        
    }, priority = 1000)
    
    ##############################################################################


    observeEvent(input$selectallbtn, {
        updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
        updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])
    }   )
    ##############################################################################

    observeEvent(input$selectfieldsbtn, {
        selecting$v <- ! selecting$v
        output$selectingfields <- renderText(selecting$v)
            
    }   )
    ##############################################################################
    
    observeEvent(input$selectnonebtn, {
        updateCheckboxGroupInput(session, "fields1", selected = "")
        updateCheckboxGroupInput(session, "fields2", selected = "")
    }   )
    ##############################################################################

    observeEvent(input$suggestbuffer, ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
               ch <- capthist()
        if (!is.null(ch)) {
            progress <- Progress$new(session, min = 1, max = 15)
            on.exit(progress$close())
            progress$set(message = 'Suggesting buffer width ...', detail = '')
           if (is.null(fitrv$value) || input$packagebtn == "openCR.fit") {
               ch <- if (ms(ch)) ch[[1]] else ch
               buff <- suggest.buffer(ch, detectfn = input$detectfnbtn,
                                      detectpar=list(lambda0 = 0.3, sigma = RPSV(ch, CC = TRUE)),
                                      noccasions = noccasions()[1], RBtarget = 0.001)
           }
            else {
               buff <- suggest.buffer(fitrv$value, RBtarget = 0.001)[1]
            }

            updateNumericInput(session, "buffer", value = round(buff))
        }
    })
    ##############################################################################

    ## renderText
    
    # maskPrint
    # resultsPrint 
    
    ##############################################################################
    
    output$maskPrint <- renderPrint({
        if (is.null(mask()))
            cat("No mask yet - load trap file on main screen\n")
        else 
            summary(mask())
    })
    ##############################################################################
    
    output$resultsPrint <- renderPrint({
        hideplotif <- function (condition, tab) {
            if (condition)
                hideTab(inputId = "plottabs", target = tab)
            else 
                showTab(inputId = "plottabs", target = tab)
        }
        hideplotif (is.null(traprv$data), "Array")
        hideplotif (is.null(fitrv$value), "Detectfn")
        hideplotif (is.null(fitrv$value), "Pxy")
        hideplotif (is.null(fitrv$value) || (input$likelihoodbtn != "Full"), "Popn")
        hideplotif (is.null(fitrv$value) || (input$likelihoodbtn != "Full"), "Power")
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
        disable("resultsbtn")  ## shinyjs
        if (is.null(traprv$data)) {
            cat("No data loaded\n")
        }
        else if (is.null(capthist())) {
            summary(traprv$data)
        }
        else if (is.null(fitrv$value)) {
            summary(capthist(), moves = TRUE)
        }
        else if (inherits(fitrv$value, c("secr","openCR"))) {
            enable("resultsbtn")    ## shinyjs
            if (input$resultsbtn == "summary")
                summary(fitrv$value)
            else if (input$resultsbtn == "derived") {
                if (input$packagebtn == "openCR.fit")
                    cat("derived method not enabled for openCR\n")
                else
                    derivedresult()
            }
            else {
                fncall <- input$otherfunction
                if (fncall=="") cat("No function specified\n")
                else {
                    fncall <- gsub("fit", "fitrv$value", fncall)
                    out <- try(eval(parse(text = fncall)))
                    if (inherits(out, "try-error")) {
                        err <- attr(out, 'condition')$message
                        cat("Did not compute\n")
                        err
                    }
                    else {
                        out
                    }
                }
            }
        }
        else {
            fitrv$value
        }
    })
    ##############################################################################
    
    ## renderPlot
    
    ## arrayPlot
    ## detnPlot
    ## popPlot
    ## pxyPlot
    ## powerPlot

    ##############################################################################
    
    output$arrayPlot <- renderPlot( { # height = 340, width = 340, {
        if (ms(traprv$data))
            tmpgrid <- traprv$data[[input$sess]]
        else 
            tmpgrid <- traprv$data
        if (is.null(tmpgrid)) return (NULL)
        par(mar = c(1,1,2,1), cex = 1.3, xpd = TRUE)
        plot (tmpgrid, border = border(1), bty='o', xaxs = 'i', yaxs = 'i', detpar = list(cex = input$cex), 
                   gridlines = (input$gridlines != "None"), gridspace = as.numeric(input$gridlines))
        
        if (inherits(capthist(), 'capthist')) {
            if (ms(capthist()))
                ch <- capthist()[[input$sess]]
            else
                ch <- capthist()
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
        }
    })
    ##############################################################################

    output$detnPlot <- renderPlot( height = 290, width = 400, {
        ## inp <- oS2()
        invalidateOutputs()
        pars <- c(lambda0(), sigma())
        if (any(is.na(pars))) {
            return (NULL)
        }
        else {
            par(mar=c(4,5,2,5))
            detectfnplot (detectfn = input$detectfnbtn,
                          pars = pars,
                          xval = 0:(3 * sigma()),
                          ylab = "",
                          hazard = TRUE,       ## 2017-08-28
                          ylim = c(0, lambda0()*1.2),
                          las=1, col = 'red', lwd = linewidth,
                          xaxs='i', yaxs='i')
            mtext(side = 2, line = 3.7, expression(paste("Detection hazard   ", lambda [0])))
            
            if (lambda0() <= 0.7) 
                p <- seq(0,1,0.05)
            else 
                p <- seq(0,1,0.1)
            
            axis(4, at = -log(1 - p), label = p, xpd = FALSE, las = 1)
            mtext(side = 4, line = 3.7, "Detection probability")
        }
    })
    ##############################################################################
    
    output$popPlot <- renderPlot( height = 300, width = 380, {
        core <- traprv$data
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
            rad <- secr::circular.r(p = 0.95, detectfn = input$detectfnbtn, sigma = tmpsig)
            symbols(tmppop$x, tmppop$y, circles = rep(rad, n),
                    inches = FALSE, fg = grey(0.7), add = TRUE, xpd = FALSE)
        }
    })
    ##############################################################################
    
    output$maskPlot <- renderPlot({
        core <- traprv$data
        if (is.null(core)) return (NULL)
        par(mar=c(2,2,2,2), xaxs='i', yaxs='i', xpd = input$xpdbox)
        
        plot (core, border = input$buffer, gridlines = FALSE)
        plot (mask(), add = TRUE, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox)
        plot (core, add = TRUE)
        
        if (!is.null(polyrv$data) && input$polygonbox) {
            sp::plot(polyrv$data, add = TRUE)
        }
        if (input$maskedge2) {
            plotMaskEdge(mask(), add = TRUE)
        }

        if (!input$xpdbox)
            box()
    })
    ##############################################################################
    
    border <- function (multiple) {
        spc <- spacing(traprv$data) 
        if (is.null(spc) || is.na(spc)) spc <- sigma()
        multiple * spc
    }
    
    output$pxyPlot <- renderPlot( height = 300, width = 380, {
        core <- traprv$data
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
                     detectfn = input$detectfnbtn,
                     detectpar = list(sigma = sigma(), lambda0 = lambda0()),
                     noccasions = noccasions()[input$sess], drawlabels = drawlabels,
                     binomN = NULL, levels = lev, 
                     poly = if (input$polygonbox) polyrv$data else NULL, 
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
            # strip.legend("right", legend = seq(0,1,0.1), title = "p.(x)", xpd = TRUE,
            strip.legend("right", legend = c(0,lev[1:10],1), title = "p.(x)", xpd = TRUE,
                         legendtype='breaks', inset = 0.01, col = cols[1:11])
        }
        if (input$maskedge) {
            plotMaskEdge(mask(), add = TRUE)
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
        if (ncol(tmp)>0) colnames(tmp) <- paste0('Analysis', 1:ncol(tmp))
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
    
    output$savebtn <- downloadHandler(
        filename = "fit.rds",
        content = function(file) {
            saveRDS(fitrv$value, file)
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
        shp <- sapply(input$polyfilename, shpfile)
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
