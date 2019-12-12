# 2019-11-05 secr only (openCR removed)
# 2019-11-05 new timing (trial evaluation LL)

library(secr)
library(shinyjs)
library(stringr)

secrversion <- packageVersion('secr')
if (compareVersion(as.character(secrversion), '4.0.1') < 0)
    stop("secrapp 1.1 requires secr version 4.0.1 or later",
         call. = FALSE)

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
        title = "secr app 1.1",
        includeCSS("secrstyle.css"),
        useShinyjs(),
        withMathJax(),
        tags$head(tags$style(".mypanel{margin-top:5px; margin-bottom:10px; padding-bottom: 5px;}")),
        tags$head(tags$style("#resultsPrint{color:blue; font-size:12px; overflow-y:scroll; min-height: 250px; max-height: 250px; background: ghostwhite;}")),
        #tags$head(tags$style("#resultsPrint{color:blue; font-size:12px; overflow-y:scroll; min-height: 250px; max-height: 250px; background: ghostwhite;}")),
        tags$head(tags$style("#codePrint{color:blue; font-size:12px; overflow-y:scroll; min-height: 250px; max-height: 300px; background: ghostwhite;}")),
        tags$head(tags$style("#maskPrint{color:blue; font-size:12px; background: ghostwhite;}")),
        tags$head(tags$style(type="text/css", "input.shiny-bound-input { font-size:14px; height:30px; margin-top:0px; margin-bottom:2px; padding-top:0px; padding-bottom:0px;}")),
        #tags$head(tags$style(type="text/css", "input.shiny-bound-input { font-size:14px; height:20px; margin-top:0px; margin-bottom:2px; padding-top:0px; padding-bottom:0px;}")),
        br(),
        navlistPanel(id = "navlist", widths = c(2,10), well=TRUE,
                     
                     "secr app 1.1",
                     
                     tabPanel("Main screen",
                              fluidRow(
                                  column (5, # offset = 0, style='padding:15px;',
                                          h2("Data input"),
                                          fluidRow(
                                              column(6,
                                                     wellPanel(class = "mypanel", 
                                                                  div(style="height: 80px;", # title = 'Text or Excel file, columns TrapID X Y',
                                                                      # trick from Felipe Gerard 2019-01 to allow reset
                                                                      # https://stackoverflow.com/questions/44203728/how-to-reset-a-value-of-fileinput-in-shiny
                                                                      # uiOutput('trapfile_ui')),
                                                                      fileInput("trapfilename", "Detector layout",   # Detector layout file
                                                                                 accept = "text/plain")),
                                                               # fluidRow(div(style="height: 25px;",
                                                               #     column(12, 
                                                               #         helpText(HTML(paste0("trapID, X, Y")))))
                                                               # ),
                                                               fluidRow(
                                                                   column(6, 
                                                                          selectInput("detector", "Detector type", 
                                                                                           choices = c("multi","proximity","count",
                                                                                                       "polygon", "polygonX", "transect", "transectX"),
                                                                                           selected = "multi")
                                                                   ),
                                                                   column(6, 
                                                                          br(),br(),
                                                                          actionLink("showtrapfilebtn", "show file")
                                                                   )
                                                               ),
                                                               fluidRow(
                                                                   column(12, 
                                                                          div(#style="height: 25px;",
                                                                              textInput("trapcovnames", "Covariate names",
                                                                                        value = "", placeholder = "e.g., traptype, habitat"))
                                                                          )
                                                               ),
                                                               fluidRow(
                                                                   column(12,
                                                                          div(# style="height: 25px;",
                                                                             textInput("trapargs", "Other arguments",value = "", placeholder = "e.g., skip = 1"))
                                                                          )
                                                               )
                                                               
                                                     )),
                                              column(6, 
                                                     wellPanel(class = "mypanel", 
                                                               div(style="height: 80px;", # title = 'Text or Excel file',
                                                                   fileInput("captfilename", "Captures",
                                                                             accept = c(".csv", ".txt", ".rdata",
                                                                                        ".rda", ".rds"))),
                                                               fluidRow(
                                                                   column(6, selectInput("fmt", label = "Format",
                                                                                         choices = c("trapID", "XY"))),
                                                                   column(6, 
                                                                          br(), br(),
                                                                          actionLink("showcaptfilebtn", "show file")
                                                                   )
                                                                   # column(6, selectInput("captsheet", label = "Sheet",
                                                                   #                       choices = c("Sheet1", "stoatcapt")))
                                                               ),
                                                               # uiOutput("captfilehelp"),
                                                               fluidRow(
                                                                   column(12, textInput("covnames", "Covariate names",
                                                                                   value = "", placeholder = "e.g., sex"))
                                                               ),
                                                               fluidRow(
                                                                   column(12,textInput("captargs", "Other arguments",
                                                                                       value = "", placeholder = "e.g., skip = 1"))
                                                               )
                                                               
                                              )
                                              )
                                          ),
                                          h2("Model"),
                                          wellPanel(class = "mypanel", 
                                                   fluidRow(
                                                       column(3, selectInput("detectfnbox", "Detectfn",
                                                                              choices = c("HN", "HR", "EX","HHN", "HHR", "HEX", "HVP"),
                                                                              selected = "HN"),
                                                              uiOutput("detectfnui") 
                                                       ),
                                                       column(3, radioButtons("likelihoodbtn", "Likelihood", choices = c("Full", "Conditional"))),
                                                       column(3, radioButtons("distributionbtn", label = "Distribution of n",
                                                                              choices = c("Poisson", "Binomial"))),
                                                       column(3, style="color:grey;",
                                                              selectInput("hcovbox", label = "Mixture hcov",
                                                                              choices = c("none"), selected = "none", width=160))
                                                   ),
                                                   fluidRow(
                                                       column(12, textInput("model", "", value = "D~1, g0~1, sigma~1"))
                                                   ),
                                                   fluidRow(
                                                       column(12, textInput("otherargs", "Other arguments", value = "", 
                                                                            placeholder = "e.g., details = list(fastproximity = FALSE), binomN = 1"))
                                                   )
                                         ),
                                         
                                         
                                         h2("Actions"),
                                         fluidRow(
                                             column(3, actionButton("fitbtn", "Fit model",  width = 130,
                                                                    title = "Fit spatially explicit capture-recapture model to estimate density and update Results")),
                                             column(3, actionButton("helpbtn", "secr help",  width = 130,
                                                                    title = "Open secr help index")),
                                             column(3, uiOutput("secrdesignurl"))  ## switch to secrdesign, with parameters
                                         ),
                                         
                                         br(),
                                         fluidRow(
                                             column(3, actionButton("resetbtn", "Reset all", width = 130, 
                                                                    title = "Reset all inputs to initial values")),
                                             column(3, bookmarkButton(width = 130)),
                                             column(3, helpText(HTML("F11 full screen")))
                                                    
                                         ),
                                         br(),
                                         fluidRow(
                                             column(11, textInput("title", "", value = "", 
                                                                  placeholder = "note for Summary")))
                                  ),
                                  
                                  column (6, # style='padding:0px;',
                                          h2("Results"),
                                                   fluidRow(
                                                       column(5, radioButtons("resultsbtn", label = "", 
                                                                               inline = TRUE, choices = c("summary", "predict", "derived", "other"))),
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
                                                                 tabPanel("Code",
                                                                          br(),
                                                                          fluidRow(
                                                                              column(12, verbatimTextOutput("codePrint"))
                                                                          )
                                                                 ),
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
                                                                 tabPanel("Pxy",
                                                                          br(),
                                                                          fluidRow(
                                                                              column(9, style='padding:0px;', plotOutput("pxyPlot", click = "pxyclick")),
                                                                              column(3, br())
                                                                          ),
                                                                          fluidRow(
                                                                              column(9, helpText(HTML("p.(x) is the probability an animal at point x will be detected at least once")))
                                                                          ),
                                                                          fluidRow(
                                                                              column(3, checkboxInput("maskedge", "show mask edge", value = FALSE))
                                                                          )
                                                                 ),
                                                                 
                                                                 tabPanel("Dxy", 
                                                                          br(),
                                                                          fluidRow(
                                                                              column(9, style='padding:0px;', plotOutput("DPlot", click = "Dclick")),
                                                                              column(3, br())
                                                                              ), 
                                                                          fluidRow(
                                                                              column(9,helpText(HTML("D(x) is the modelled density at point x")))
                                                                              ),
                                                                          fluidRow(
                                                                              column(2, checkboxInput("Dmaskedge", "mask edge", value = FALSE)),
                                                                              column(2, checkboxInput("Dshowdetectors", "detectors", value = FALSE)),
                                                                              column(2, checkboxInput("Dshowdetections", "detections", value = FALSE)),
                                                                              column(2, checkboxInput("Dshowpopn", "realisation", value = FALSE),
                                                                                     uiOutput('uipopN')),
                                                                              column(4, checkboxInput("showHRbox", "95% HR", value = FALSE))
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
                                                                                                         value = FALSE,
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
                                  column(3,
                                         tabsetPanel(
                                             type = "pills", id = "masktype", selected = "Build",
                                             tabPanel("Build",
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
                                             tabPanel("File", 
                                                      wellPanel(class = "mypanel", 
                                                                div(style = "height: 80px;",
                                                                    fileInput("maskfilename", "Mask file",
                                                                              accept = c('.txt'), 
                                                                              multiple = FALSE)),
                                                                numericInput("maskcov", "Covariate to display",
                                                                             min = 0,
                                                                             max = 0,
                                                                             value = 0,
                                                                             step = 1,
                                                                             width = 180)
                                                      )
                                             ) 
                                         )
                                  ),
                                  column(5, plotOutput("maskPlot"),
                                         conditionalPanel ("output.maskready", fluidRow(
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
                                                                                                    "usagepct", "maskbuffer", "masknrow", "maskspace",
                                                                                                    "likelihood", "distribution","model"
                                                                                        ),
                                                                                        selected = c("date", "time", "note", "traps", "captures", 
                                                                                                     "n", "r", "ndetectors", "noccasions",
                                                                                                     "usagepct", "maskbuffer", "masknrow", "maskspace",
                                                                                                     "likelihood", "distribution", "model"
                                                                                        )
                                                                     ))),
                                             column(6,
                                                    conditionalPanel("output.selectingfields == 'TRUE'",
                                                                     checkboxGroupInput("fields2", "",
                                                                                        choices = c("detectfn", 
                                                                                                    "hcov", "npar", "logLik", "AIC", "dAIC",
                                                                                                    "D", "se.D", "RSE.D", 
                                                                                                    "g0", "se.g0", "lambda0", "se.lambda0","sigma", "se.sigma", "z", "se.z",
                                                                                                    "k", "proctime"
                                                                                        ),
                                                                                        selected = c("detectfn",
                                                                                                     "hcov", "npar", "logLik", "AIC", "dAIC",
                                                                                                     "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", "se.sigma",
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
                                         
                                         # h2("Detector array"),
                                         # wellPanel(class = "mypanel", 
                                         #           fluidRow(
                                         #               column(6, radioButtons("areaunit", label = "Area units",
                                         #                                      choices = c("ha", "km^2"), 
                                         #                                      selected = "ha", inline = TRUE))
                                         #           )
                                         # ),
                                         h2("Data Import/Export"),
                                         wellPanel(class = "mypanel",
                                                   fluidRow(
                                                       column(8, fileInput("importfilename", "Import capthist from Rds file")),
                                                       # ,  
                                                       #                  accept = "text/plain")),
                                                       column(4, br(), actionLink("clearimportbtn", "Clear"))
                                                       
                                                   ),
                                                   fluidRow(
                                                       column(8, downloadLink("exportbtn", "Export capthist to Rds file", 
                                                                                title = "Save as RDS file"))  #, "Export capthist to Rds file")),
                                                       #column(4, br(), actionLink("exportbtn", "Save"))
                                                       
                                                   )
                                         ),
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
                              h2("secr app 1.1"), br(),
                              
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
                       "hcov", "detectfn", "npar", "logLik", "AIC", "dAIC",
                       "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", "se.sigma", "z", "se.z",
                       "k", "proctime"
                       )
    
    fieldgroup1 <- 1:16
    fieldgroup2 <- 17:35

    polygondetectors <- c("polygon", "polygonX", "transect", "transectX")
    hazarddetectfn <- c("HHN", "HHR", "HEX", "HVP")
    
    ## for cycling through animals at one detector 2019-03-08
    lasttrap <- 0
    clickno <- 0
    
    showNotification(paste("secr", desc$Version, desc$Date),
                     closeButton = FALSE, type = "message", duration = seconds)
     output$selectingfields <- renderText('false')
     output$multisession <- renderText('false')
     outputOptions(output, "selectingfields", suspendWhenHidden = FALSE)
     outputOptions(output, "multisession", suspendWhenHidden = FALSE)

     output$maskready <- reactive({
         return(!is.null(mask()))
     })
     
     output$modelFitted <- reactive({
         return(!is.null(fitrv$value))
     })
     
     outputOptions(output, "maskready", suspendWhenHidden=FALSE)
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
                 paste0("detector=", input$detector)
             )
             
             if (!is.null(input$trapfilename)) {
                 parm <- c(parm,
                           paste0("trapargs=", input$trapargs))
             }
             
             if (!is.null(input$captfilename)) {
                 parm <- c(parm,
                           paste0("noccasions=", as.character(noccasions())))
             }
             
             if (!is.null(fitrv$value)) {
                 parm <- c(parm,
                           paste0("D=", as.character(round(density(), input$dec))),
                           paste0(detectrv$value, "=", as.character(round(detect0(), input$dec))),
                           paste0("sigma=", as.character(round(sigma(), input$dec))))
             }
             
             parm <- paste(parm, collapse = "&")
             # open secrdesignapp in a new tab, with parameters from secrapp
             # designurl is set at top of this file
             
             if (input$detectfnbox %in% c('HHN','HEX')) {
                 tags$a(href =  paste0(designurl, "?", parm), "Switch to secrdesign", target="_blank")  
             }
         }
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
    
     output$uipopN <- renderUI({
         N <- nrow(pop())
         if (is.null(pop()) || !input$Dshowpopn) 
             helpText("") 
         else 
             helpText(HTML(paste0("N = ", N)))
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
            if (input$detector %in% polygondetectors) {
                if (!is.null(capthist())) {
                    nearest <- nearesttrap(xy, xy(capthist()))
                    updateNumericInput(session, "animal", value = animalID(capthist(), names=FALSE)[nearest])
                    id <- paste0(animalID(capthist())[nearest], ":")
                }
                else {
                    nearest <- nearesttrap(xy, tmpgrid)
                    id <- polyID(tmpgrid)[nearest]
                }
            }
            else {
                nearest <- nearesttrap(xy, tmpgrid)
                id <- ""
                if (!is.null(capthist())) {
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
                }
                else {
                    id <- paste0(rownames(tmpgrid)[nearest], ":")
                }
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
            traps = if (is.null(input$trapfilename)) "" else input$trapfilename$name[1],
            captures = if (is.null(captrv$data)) {
                if (is.null(input$importfilename)) "" else input$importfilename$name[1]
            }
            else input$captfilename$name[1],
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
            proctime = NA_real_
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
            df[,20:35] <- round(df[,20:35], input$dec)
        }
        sumrv$value <- rbind (sumrv$value, df)
        if (nrow(sumrv$value)>1) sumrv$value$dAIC <- sumrv$value$AIC - min(sumrv$value$AIC)
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
        if (!is.null(traprv$data) && is.null(importrv$value)) {
            args <- input$trapargs
            if (args != "") {
                args <- paste0(", ", args)
            }
            code <- paste0("array <- read.traps ('",
                           input$trapfilename[1,"name"],
                           "', detector = '", input$detector, "'", args, ")\n")
            # if (input$scalefactor != 1.0) {
            #     code <- paste0(code,
            #                    "# optional scaling about centroid\n",
            #                    "meanxy <- apply(array,2,mean)\n",
            #                    "array[,1] <- (array[,1]- meanxy[1]) * ", input$scalefactor, " + meanxy[1]\n",
            #                    "array[,2] <- (array[,2]- meanxy[2]) * ", input$scalefactor, " + meanxy[2]\n")
            #     #"array[,] <- array[,] * ", input$scalefactor, "\n")
            # }

            if (!is.null(covariates(traprv$data))) {
                covnames <- getcovnames(input$trapcovnames, ncol(covariates(traprv$data)), TRUE)
                covnamecode <- paste0("names(covariates(array)) <- ", covnames, "\n")
                code <- paste0(code, covnamecode)
            }
            if (comment) {
                tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
                tmp$sep <- "\n"
                code <- do.call(paste, tmp)
            }
        }
        code        
    }
    ##############################################################################
    
    maskcode <- function () {
        if (input$masktype == 'Build') {
            if (is.null(traprv$data))
                return("\n")
            else {
                type <- if (input$maskshapebtn == 'Rectangular') 'traprect' else 'trapbuffer'
                buffer <- as.character(round(input$buffer,2))
                polycode <- ""
                polyhabitat <- ""
                
                if (input$polygonbox && !is.null(input$polyfilename)) { 
                    polyhabitat <- input$includeexcludebtn == "Include"
                    polycode <- getSPcode(input$polyfilename, "poly", input$polygonbox)
                }
                paste0(polycode,
                       "mask <- make.mask (array",  
                       ", buffer = ", buffer, 
                       ", nx = ", input$habnx, 
                       ", type = '", type, "'",  
                       if (polycode == "") "" else ",\n    poly = poly",
                       if (polycode == "") "" else ", poly.habitat = ", polyhabitat,
                       ")\n")
            }
        }
        else {
            if (!is.null(input$maskfilename))
                paste0("mask <- read.mask ('", input$maskfilename[1,1], "', header = TRUE)\n")
        }
    }
    ##############################################################################
    
    captcode <- function (comment = FALSE) {
        # returns the R code needed to generate the specified array, 
        # as a character value
        code <- ""  
        if (!is.null(importrv$data)) {
            code <- paste0("ch <- readRDS('", input$importfilename[1,"name"], "')\n")
        }
        else {
        if (!is.null(captrv$data)) {
            args <- input$captargs
            if (args != "")
                args <- paste0(", ", args)
            if (input$fmt=='XY') {
                fmt <- ", fmt = 'XY'" 
                datacols <- 5
            }
            else {
                fmt <- ""
                datacols <- 4
            }
            ncov <- ncol(captrv$data) - datacols
            if (ncov==0) {
                cov <- NULL
            }
            else {
                covnames <- getcovnames(input$covnames, ncov, TRUE)
                cov <- if (covnames == "") "" else paste0(", covnames = ", covnames)
            }
            
            if (grepl('.xls', input$captfilename$name[1])) {
                code <- paste0("capt <- readxl::read_excel('", input$captfilename[1,"name"], "'", args, ")\n")
            }
            else {
                code <- paste0("capt <- read.table('", input$captfilename[1,"name"], "'", args, ")\n")
            }
            
            code <- paste0(code, "ch <- make.capthist (capt, traps = array", fmt, cov, ")\n")
            
            # if (input$scalefactor != 1.0) {
            #     code <- paste0(code,
            #                    "# optional scaling about centroid\n",
            #                    "meanxy <- apply(array,2,mean)\n",
            #                    "array[,1] <- (array[,1]- meanxy[1]) * ", input$scalefactor, " + meanxy[1]\n",
            #                    "array[,2] <- (array[,2]- meanxy[2]) * ", input$scalefactor, " + meanxy[2]\n")
            #     #"array[,] <- array[,] * ", input$scalefactor, "\n")
            # }
            
            if (comment) {
                tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
                tmp$sep <- "\n"
                code <- do.call(paste, tmp)
            }
        }
        }
        code        
    }
    ##############################################################################
    
    fitcode <- function() {
        detfn <- input$detectfnbox
        if (is.character(detfn)) detfn <- paste0("'", detfn, "'")
        CL <- if (input$likelihoodbtn == "Full") "" else ", CL = TRUE"
        hcov <- if(input$hcovbox == "none") "" else paste0(", hcov = '", input$hcovbox, "'")
        model <- paste0("model = list(", input$model, ")")
        distn <- if (input$distributionbtn == "Poisson") "" else 
            ",\n      details = list(distribution = 'binomial')"
        method <- if (input$method == "Newton-Raphson") "" else 
            paste0(",\n      method = '", input$method, "'")
        otherargs <- if (input$otherargs=="") "" else paste0(",\n      ", input$otherargs)
        code <- paste0(
            "fit <- secr.fit(ch, mask = mask, detectfn = ", detfn, CL, hcov, ", \n", 
            "      ", model, ", trace = FALSE", distn, method, otherargs, ")\n"
        )
        code
    }
    ##############################################################################
    
    timefn <- function(LL) attr(LL,'preptime') + attr(LL,'npar')^2 * attr(LL,'LLtime') * 10
    
    modellist <- function() {
        f <- paste0("list(", input$model, ")")
        f <- tryCatch(parse(text = f), error = function(e) NULL)
        eval(f)
    }
    
    fitmodel <- function(LLonly = FALSE) {
        
        if (!LLonly) {
            progress <- Progress$new(session, min = 1, max = 15)
            on.exit(progress$close())
            progress$set(message = 'Fitting...', detail = '')
        }
        CL <- input$likelihoodbtn != "Full"
        
        # f <- paste0("list(", input$model, ")")
        # f <- tryCatch(parse(text = f), error = function(e) NULL)
        # model <- eval(f)
        model <- modellist()

        otherargs <- try(eval(parse(text = paste0("list(", input$otherargs, ")"))))
        if (inherits(otherargs, "try-error") && !LLonly) {
            showNotification("model fit failed - check other arguments",
                             type = "error", id = "nofit", duration = seconds)
            fit <- NULL
        }
        else {
            args <- c(list(capthist = capthist(), 
                           trace = FALSE,
                           mask = mask(), 
                           model = model,
                           detectfn = input$detectfnbox),
                      otherargs)
            args$CL <- CL 
            args$details <- as.list(replace (args$details, "distribution", input$distributionbtn))
            if (LLonly)
                args$details <- as.list(replace (args$details, "LLonly", TRUE))
            if (input$hcovbox != "none") {
                args$hcov <- input$hcovbox
            }
            isolate(fit <- try(do.call("secr.fit", args), silent = TRUE))
            if (inherits(fit, "try-error") && !LLonly) {
                showNotification("model fit failed - check data, formulae and mask",
                                 type = "error", id = "nofit", duration = seconds)
                fit <- NULL
            }
        }
        if (LLonly) {
            return(fit)
        }
        else {
            fitrv$value <- fit
            if (length(fit)>0)
                fitrv$dsurf <- predictDsurface(fit)
            else 
                fitrv$dsurf <- NULL
            addtosummary()
        }
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
    OKModal <- function(expectedtime) {
        modalDialog(
            paste("Fitting is predicted to take ", round(expectedtime,1), " minutes"),
            size = "s",
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("okbtn", "Continue")
            )
        )
    }
    ##############################################################################
    
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
    
    importrv <- reactiveValues(
        data = NULL,
        clear = FALSE
    )
    
    polyrv <- reactiveValues(
        data = NULL,
        clear = FALSE
    )
    
    maskrv <- reactiveValues(
        data = NULL,
        clear = FALSE
    )
    
    ##############################################################################

    getcovnames <- function (cov, ncov, quote = FALSE, character = TRUE) {
        if (ncov <= 0) {
            NULL
        }
        else {
            covnames <- paste0('X', 1:ncov)   # default
            if (cov != "") {
                cov <- gsub("(')|(\")", "", cov)   # remove quotes
                nam <- strsplit(str_squish(cov), '[ ,]')
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
    
    ## read import file
    observe({
        req(input$importfilename)
        req(!importrv$clear)
        ch <- readRDS(input$importfilename[1,"datapath"])
        if (inherits(ch, 'capthist')) {
            importrv$data <- ch
            traprv$data <- traps(ch)
        }
        else {
            stop("not a valid capthist Rds")
        }
    })
    
    
    ## read trap file
    observe({
        req(input$trapfilename)
        req(!traprv$clear)
        filename <- input$trapfilename[1,"datapath"]
        
        if (is.null(filename))
            stop("provide valid filename")
        args <- input$trapargs
        if (args != "") {
            args <- paste0(", ", args)
        }
        
        readtrapcall <-  paste0("read.traps (filename, detector = input$detector", args, ")")
        temp <- try(eval(parse(text = readtrapcall)))
        if (!inherits(temp, "traps")) {
            showNotification("invalid trap file or arguments; try again",
                             type = "error", id = "badtrap", duration = seconds)
            traprv$data <- NULL
            #traprv$clear <- TRUE
        }
        else {
            ncov <- ncol(covariates(temp))
            if (length(ncov)>0 && ncov>0) {
                covnames <- getcovnames(input$trapcovnames, ncov, TRUE, FALSE)
                names(covariates(temp)) <- covnames
            }
            # if (covnames != "") {
            #     args <- paste0(", covnames = ", covnames)
            # }
            traprv$data <- temp
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
        if (grepl('.xls', input$captfilename$name[1])) {
            readcaptcall <- paste0("readxl::read_excel(filename", args, ")")
        }
        else {
            readcaptcall <- paste0("read.table (filename", args, ")")
        }
        captrv$data <- try(eval(parse(text = readcaptcall)))
        captrv$data <- as.data.frame(captrv$data)
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
    ## read mask file
    observe({
        req(input$maskfilename)
        req(!maskrv$clear)
        maskrv$data <- NULL
        if (input$masktype == 'File') {
            if (!is.null(input$maskfilename))
            maskrv$data <- read.mask(input$maskfilename[1,4], header = TRUE) 
            covar <- covariates(maskrv$data)
            if (!is.null(covar)) {
                updateNumericInput(session, "maskcov", max = ncol(covar))
            }
        }
    })
    ##############################################################################

    observeEvent(input$trapfilename, {
        traprv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$importfilename, {
        importrv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$captfilename, {
        captrv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$polyfilename, {
        polyrv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$maskfilename, {
        maskrv$clear <- FALSE
    }, priority = 1000)

    ##############################################################################
    
    capthist <- reactive( {
        if ((is.null(traprv$data) || is.null(captrv$data)) && is.null(importrv$data)) {
            updateNumericInput(session, "animal", max = 0)
            NULL
        }
        else {
            if (!is.null(importrv$data)) {
                ch <- importrv$data
            }
            else {
                ch <- try(suppressWarnings(make.capthist(captrv$data, traprv$data, 
                                                         fmt = input$fmt)))
                if (inherits(ch, 'try-error')) {
                    showNotification("invalid capture file or arguments; try again",
                                     type = "error", id = "badcapt", duration = seconds)
                    showNotification(ch, type = "error", id = "capterror", duration = seconds)
                    ch <- NULL
                }
                else {
                    if (ms(ch)) {
                        ncov <- ncol(covariates(ch[[1]]))
                        if (length(ncov)>0 && ncov>0) {
                            covnames <- getcovnames(input$covnames, ncov, TRUE, FALSE)
                        }
                        if (length(ncov)>0 && ncov>0) {
                            for (i in 1:length(ch)) names(covariates(ch[[i]])) <- covnames
                        }
                        showNotification("multisession data - may cause problems", 
                                         type = "warning", id = "mswarning", duration = seconds)
                        updateNumericInput(session, "animal", max = nrow(ch[[input$sess]]))
                        output$multisession <- renderText("true")
                        
                        
                    }
                    else {
                        ncov <- ncol(covariates(ch))
                        if (length(ncov)>0 && ncov>0) {
                            covnames <- getcovnames(input$covnames, ncov, TRUE, FALSE)
                        }
                        if (length(ncov)>0 && ncov>0) {
                            names(covariates(ch)) <- covnames
                        }
                        updateNumericInput(session, "animal", max = nrow(ch))
                        output$multisession <- renderText("false")
                    }
                    updateNumericInput(session, "sess", max = length(ch))
                    updateSelectInput(session, "hcovbox", choices = c("none", names(covariates(ch))))
                }
            }
            ch
        }
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
    
    detectrv <- reactive({
        if (input$detectfnbox %in% c('HHN', 'HHR', 'HEX', 'HAN', 'HCG', 'HVP')) {
            detectrv$value <- 'lambda0'
            ## following code not working so suppress
            # if (!("lambda0" %in% input$fields2)) 
            #     updateCheckboxGroupInput(session, "fields2", selected = c(input$fields2, "lambda0"))
            # if (!("se.lambda0" %in% input$fields2))
            #     updateCheckboxGroupInput(session, "fields2", selected = c(input$fields2, "se.lambda0"))
        }
        else {
            detectrv$value <- 'g0'
            ## following code not working so suppress
            # if (!("g0" %in% input$fields2)) 
            #     updateCheckboxGroupInput(session, "fields2", selected = c(input$fields2, "g0"))
            # if (!("se.g0" %in% input$fields2))
            #     updateCheckboxGroupInput(session, "fields2", selected = c(input$fields2, "se.g0"))
        }
    })
    ##############################################################################
    
    predictresult <- reactive({
        pred <- predict(fitrv$value, all.levels = TRUE)
        roundpr <- function (pr) {pr[,-1] <- round(pr[,-1], input$dec); pr}
        if (is.data.frame(pred))
            pred <- roundpr(pred)
        else 
            pred <- lapply(pred, roundpr)
        pred
    })
    ##############################################################################
    
    derivedresult <- reactive({
        # progress <- Progress$new(session, min = 1, max = 15)
        # on.exit(progress$close())
        # progress$set(message = 'Computing derived estimates ...', detail = '')
        showNotification("Computing derived estimates ...",
                         id = "derived", duration = seconds)
        der <- derived(fitrv$value, distribution = tolower(input$distributionbtn))
        round(der, input$dec)
    })
    ##############################################################################
    
    invalidateOutputs <- reactive({
        pxyrv$value <- NULL
        Drv$value <- NULL
        # updateNumericInput(session, "D", step = 10^trunc(log10(density()/50)))
    })
    
    ##############################################################################

    predictparm <- function(parm = 'D', stat = 'estimate') {
        if (!inherits(fitrv$value, "secr"))
            NA
        else {
            pred <- predict(fitrv$value, newdata = newdata())
            if (is.data.frame(pred)) {
                pred[parm, stat]
            }
            else {
                pred[[1]][parm,stat]
            }
        }
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
    
    mask <- reactive( {
        pxyrv$value <- NULL
        Drv$value <- NULL
        if (input$masktype=="Build") {
            
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
        else {
            maskrv$data
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
            secr.make.newdata(fitrv$value, all.levels = TRUE)
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
            input$Dshowpopn
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
            
            # dsurf <- predictDsurface(fitrv$value)
            msk <- fitrv$value$mask
            sim.popn (D = 'D.0', core=fitrv$dsurf, model2D="IHP", Ndist = Ndist)   # density()
            
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
        if (!inherits(fitrv$value, "secr") || (input$likelihoodbtn != "Full") )
            return (NULL)
        else {
            V <- vcov(fitrv$value)['D','D']   ## FAILS IF HAVE JUST SWITCHED BUTTON
            sqrt(exp(V)-1) * 100
        }
    })
    ##############################################################################

    se.density <- reactive( {
        if (!inherits(fitrv$value, "secr")) {
            NA
        }
        else {
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
    })
    ##############################################################################

    se.detect0 <- reactive({
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

    ## reactiveValues
    
    arrrv <- reactiveValues(v = 0)  # used to invalidate and re-plot detectorarray
    current <- reactiveValues(unit = "ha")
    fitrv <- reactiveValues(value = NULL)
    poprv <- reactiveValues(v = 0)  # used to invalidate and re-plot popn
    pxyrv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
    Drv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
    RSErv <- reactiveValues(current = FALSE, value = NULL, adjRSE = NULL)
    selecting <- reactiveValues(v=FALSE)
    sumrv <- reactiveValues(value = read.csv(text = paste(summaryfields, collapse = ", ")))
    detectrv <- reactiveValues(value='g0')
    traptextrv <- reactiveValues(value=FALSE)
    capttextrv <- reactiveValues(value=FALSE)
    
    ##############################################################################
    
    ## observeEvent

    # alpha
    # areaunit
    # CIclick
    # clearallbtn
    # clearlastbtn
    # detectfnbox
    # distributionbtn
    # fitbtn
    # likelihoodbtn
    # model
    # okbtn
    # otherargs
    # pxyclick
    # Dclick
    # randompopbtn
    # resetbtn
    # selectallbtn
    # selectfieldsbtn
    # selectnonebtn
    # suggestbuffer
    
    ## Invalidate results when model specification changes
    # likelihoodbtn
    # detectfnbox
    # distributionbtn
    # model
    # otherarg

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

    observeEvent(input$clearallbtn, {
        sumrv$value <- sumrv$value[0,]
    }   )
    
    ##############################################################################

    observeEvent(input$clearimportbtn, {
        reset('importfilename')
        
        importrv$data <- NULL
        importrv$clear <- TRUE
        
        traprv$data <- NULL
        traprv$clear <- TRUE
        
        captrv$data <- NULL
        captrv$clear <- TRUE
    })
    
    ##############################################################################
    
    observeEvent(input$clearlastbtn, {
        if (nrow(sumrv$value)>0)
            sumrv$value <- sumrv$value[-nrow(sumrv$value),]
    }   )
    
    ##############################################################################
    
    observeEvent(input$detector, {
        if (input$detector %in% polygondetectors) {
            updateSelectInput(session, "detectfnbox", choices = hazarddetectfn, selected = "HHN")
            updateSelectInput(session, "fmt", label = "Format", choices = c("XY"), selected = 'XY')
        }
        else {
            updateSelectInput(session, "detectfnbox", choices = c('HN','HR','EX',hazarddetectfn))
            updateSelectInput(session, "fmt", label = "Format", choices = c("trapID", "XY"))
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

    observeEvent(input$showtrapfilebtn, ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
        capttextrv$value <- FALSE
        traptextrv$value <- !is.null(input$trapfilename) && (input$showtrapfilebtn %% 2 == 1)
    })
    
    observeEvent(input$showcaptfilebtn, ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
        traptextrv$value <- FALSE
        capttextrv$value <- !is.null(input$captfilename) && (input$showcaptfilebtn %% 2 == 1)
    })
    
    observeEvent(input$fitbtn, ignoreInit = TRUE, {
        ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
        if (is.null(capthist())) {
             showNotification("load data",
                             type = "warning", id = "nodata", duration = seconds)
        }
        else {
            # one likelihood
            LL <- try(fitmodel(LLonly = TRUE) )
            if (inherits(LL, 'try-error')) {
                showNotification("failed to compute expected time")
                expectedtime <- Inf
            }
            else {
                expectedtime <- timefn(LL)/60
                if (expectedtime > timewarning)
                    if (expectedtime > timelimit) {
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
    
    observeEvent(input$helpbtn, ignoreInit = TRUE, {
        browseURL(file.path(path.package("secr"), "html",  "00Index.html")) 
    })
    
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
    })
    ##############################################################################
    
    observeEvent(input$trapcovnames, ignoreInit = TRUE, {
        fitrv$value <- NULL
        traprv$value <- NULL
        captrv$value <- NULL
    })
    ##############################################################################
    
    observeEvent(input$trapargs, ignoreInit = TRUE, {
        fitrv$value <- NULL
        traprv$value <- NULL
        captrv$value <- NULL
    })
    
    ##############################################################################
    
    observeEvent(input$covnames, ignoreInit = TRUE, {
        fitrv$value <- NULL
        captrv$value <- NULL
    })
    
    observeEvent(input$captargs, ignoreInit = TRUE, {
        fitrv$value <- NULL
        captrv$value <- NULL
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
        #dsurf <- predictDsurface(fitrv$value)
        Drv$xy <-c(input$Dclick$x, input$Dclick$y)
        if (pointsInPolygon(Drv$xy, fitrv$dsurf)) {
            maskrow <- nearesttrap(Drv$xy, mask())
            cov <- 'D.0'
            Drv$value <- covariates(fitrv$dsurf)[maskrow, cov, drop = FALSE]
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

    observeEvent(input$resetbtn, {
        
        current$unit <- "ha"
        fitrv$value <- NULL
        traptextrv$value <- FALSE
        capttextrv$value <- FALSE
        
        ## Data input

        ## Trap layout
        updateTextInput(session, "trapargs", 
                        value = "", placeholder = "e.g., skip = 1")
        updateSelectInput(session, "detector", selected = "multi")
        updateTextInput(session, "trapcovnames", value = "", placeholder = "e.g., traptype, habitat")
        
        ## Captures
        updateTextInput(session, "captargs", 
                        value = "", placeholder = "e.g., skip = 1")
        updateSelectInput(session, "fmt", selected = "trapID")
        updateTextInput(session, "covnames", value = "", placeholder = "e.g., sex")
        
        ## Model
        
        updateSelectInput(session, "detectfnbox", selected = "HN")
        updateRadioButtons(session, "distributionbtn", selected = "Poisson")
        updateRadioButtons(session, "likelihoodbtn", selected = "Full")
        updateSelectInput(session, "hcovbox", choices = "none", selected = "none")

        updateTextInput(session, "model", 
                        value = "D~1, g0~1, sigma~1", placeholder = "")
        updateTextInput(session, "otherargs", 
                        value = "", placeholder = "e.g., details = list(fastproximity = FALSE)")
        
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

        ## pxy plot
        updateCheckboxInput(session, "maskedge", value = FALSE)
        
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
        
        maskrv$data <- NULL
        maskrv$clear <- TRUE
        reset('maskfilename') 
        
        importrv$data <- NULL
        importrv$clear <- TRUE
        reset('importfilename')
        
        detectrv$value <- 'g0'

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
           if (is.null(fitrv$value)) {
               ch <- if (ms(ch)) ch[[1]] else ch
               detectparlist <- list(0.3, RPSV(ch, CC = TRUE), 1)
               names(detectparlist) <- c(detectrv$value, 'sigma', 'z')
               buff <- suggest.buffer(ch, detectfn = input$detectfnbox,
                                      detectpar = detectparlist,
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
    # codePrint 
    
    ##############################################################################
    
    output$maskPrint <- renderPrint({
        if (is.null(mask())) {
            if (input$masktype == 'Build') 
                cat("No mask yet - load trap file on main screen\n")
            else 
                cat("No mask yet - load mask file\n")
        }
        else {
            summary(mask())
        }
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
        hideplotif (is.null(fitrv$value) || (input$likelihoodbtn != 'Full'), "Dxy")
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
        if (traptextrv$value) {
            cat("Detector text file\n")
            cat("----------------------------------------------------------------\n")
            rawText <- readLines(input$trapfilename[1,"datapath"]) # get raw text
            writeLines(rawText)
        }
        else if (capttextrv$value) {
            cat("Capture text file\n")
            cat("----------------------------------------------------------------\n")
            rawText <- readLines(input$captfilename[1,"datapath"]) # get raw text
            writeLines(rawText)
        }
        else if (is.null(traprv$data)) {
            cat("No data loaded\n")
        }
        else if (is.null(capthist())) {
            summary(traprv$data)
        }
        else if (is.null(fitrv$value)) {
            summary(capthist(), moves = TRUE)
        }
        else if (inherits(fitrv$value, "secr")) {
            enable("resultsbtn")    ## shinyjs
            if (input$resultsbtn == "summary")
                summary(fitrv$value)
            else if (input$resultsbtn == "predict") {
                predictresult()
            }
            else if (input$resultsbtn == "derived") {
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
    
    output$codePrint <- renderPrint({
        cat("library(secr)\n")
        if (!is.null(traprv$data)) {
            cat("\n# input data\n")
            cat(arraycode())
        }
        cat(captcode())
        if (!is.null(capthist())) {
            cat("summary(ch)\n")
            cat("\n# fit model\n")
            cat(maskcode())
            cat(fitcode())
            cat("summary(fit)\n")
        }
    })
    ##############################################################################
    
    ## renderPlot
    
    ## arrayPlot
    ## detnPlot
    ## pxyPlot
    ## DPlot
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
                          xval = 0:(3 * sigma()),
                          ylab = "",
                          hazard = TRUE,       
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
    
    output$maskPlot <- renderPlot({
        
        core <- traprv$data
        msk <- mask()
        par(mar=c(2,2,2,2), xaxs='i', yaxs='i', xpd = input$xpdbox)
        
        if (input$masktype == "Build") {
            if (is.null(core)) return (NULL)
            plot (core, border = input$buffer, gridlines = FALSE)
            plot (msk, add = TRUE, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox)
            plot (core, add = TRUE)
            if (!is.null(polyrv$data) && input$polygonbox) {
                sp::plot(polyrv$data, add = TRUE)
            }
        }
        
        else {
            if (!is.null(msk)) {
                if (input$maskcov == 0) {
                    cov <- NULL
                    plot (msk, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox,
                          covariate = cov)
                }
                else {
                    covname <- names(covariates(msk))[input$maskcov]
                    plot (msk, dots = input$dotsbox, covariate = covname)
                }
                if (inherits(core, 'traps')) plot (core, add = TRUE)
            }
        }    
        if (!is.null(msk)) {
            if (input$maskedge2) {
                plotMaskEdge(msk, add = TRUE)
            }
            if (!input$xpdbox)
                box()
        }

        
    })
    ##############################################################################
    
    border <- function (multiple) {
        if (detector(traprv$data)[1] %in% c('polygon','polygonX','transect','transectX')) {
            spc <- max(diff(range(traprv$data$x)), diff(range(traprv$data$y)))/10
        }
        else {
            spc <- spacing(traprv$data) 
        }
        if (is.null(spc) || is.na(spc)) spc <- sigma()
        multiple * spc
    }
    
    output$pxyPlot <- renderPlot({
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
        pdot.contour(core, border = border, nx = input$pxynx,
                     detectfn = input$detectfnbox,
                     detectpar = detectparlist,
                     noccasions = noccasions()[input$sess], drawlabels = drawlabels,
                     binomN = 0, 
                     levels = lev, 
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
    
    output$DPlot <- renderPlot( {
        invalidateOutputs()
        par(mar=c(1,1,1,5))
        if (input$likelihoodbtn!='CL' && !is.null(fitrv$value)) {
            #dsurf <- predictDsurface(fitrv$value)
            plot (fitrv$dsurf, border = 0, scale = 1, title="D(x)")
            if (input$Dshowdetectors) {
                plot(traprv$data, add = TRUE)
            }
            if (input$Dshowdetections) {
                plot(capthist(), varycol = FALSE, add = TRUE)
            }
            if (!is.null(Drv$value)) {
                xy <- Drv$xy
                points(xy[1], xy[2], pch=16, cex=0.7)
                offset <- (par()$usr[2] - par()$usr[1])/15
                text(xy[1]+offset, xy[2], round(Drv$value,3), cex = 0.9, xpd = TRUE)
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
                        symbols(tmppop$x, tmppop$y, circles = rep(rad, n),
                                inches = FALSE, fg = grey(0.95), add = TRUE, xpd = FALSE)
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
    
    output$exportbtn <- downloadHandler(
        #if (!is.null(capthist())) {
            filename = "ch.rds",
            content = function(file) {
                saveRDS(capthist(), file)
            }
        #}
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

    setBookmarkExclude(c("fitbtn", 
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
    ## global variables
    
    timewarning <- 0.2  ## minutes
    timelimit <- 5.0    ## minutes

    ##############################################################################
}

##################################################################################
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "server")
##################################################################################
