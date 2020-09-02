## imports stringr, readxl

library(secr)
library(shinyjs)

secrversion <- packageVersion('secr')
if (compareVersion(as.character(secrversion), '4.3.1') < 0)
  stop("secrapp 1.3 requires secr version 4.3.1 or later",
    call. = FALSE)

# for transfer to secrdesign
# designurl <- "http://127.0.0.1:4429/"    ## temporarily use 4429 local
designurl <- "https://www.stats.otago.ac.nz/secrdesignapp/"   # secrdesignapp 1.2 and above reads parameters

# requires package rgdal to read shapefiles
# requires package sp for bbox and plot method for SpatialPolygons
# requires package parallel for max cores in simulate options (distributed with base R)
# requires package tools for file path when reading shapefiles (distributed with base R)
# requires package stringr for some string operations
# requires package readxl for reading Excel files

# interrupt is hard -
# see http://htmlpreview.github.io/?https://github.com/fellstat/ipc/blob/master/inst/doc/shinymp.html

##############################################################################
## global variables

linewidth <- 2  # for various plots 
seconds <- 6   ## default duration for showNotification()

timewarning <- 0.5  ## minutes
timelimit <- 10.0    ## minutes

availablecores <- min(24, parallel::detectCores())
defaultcores <- max(1, availablecores/2)

defaultresultsbtn <- c("summary", "hide", "other")
fittedresultsbtn <- c("summary", "hide", "predict", "derived", "other")

##############################################################################

# Define UI 
ui <- function(request) {
  
  fluidPage(
    title = "secr app 1.3",
    includeCSS("secrstyle.css"),
    useShinyjs(),
    withMathJax(),
    br(),
    navlistPanel(id = "navlist", widths = c(2,10), well=TRUE,
      
      "secr app 1.3",
      tabPanel("Introduction",
        # withMathJax(includeMarkdown("intro.rmd"))  # alternative Rmarkdown 
        fluidRow(
          column(8,
            h2("What is secrapp?"),
            
            HTML("Spatially explicit capture&ndash;recapture (SECR or SCR) (Efford 2004, Borchers and Efford 2008, 
                                  Efford et al. 2009) is used to estimate animal population density from data on marked animals 
                                  at arrays of passive detectors (traps, cameras, hair snags etc.)."),
            br(),br(),
            
            "SECR is computer-intensive, with both Bayesian and frequentist (maximum likelihood) flavours. 
                                  Here we focus on data from closed populations (no births, deaths, immigration or emigration 
                                  during sampling) and model fitting by maximum likelihood.",
            br(), br(),
            
            strong("secrapp"), "is an interactive interface to parts of the R package", strong("secr"), 
            "(Efford 2020). ", strong("secr"), " and ", strong("secrapp"), "together supercede the Windows", 
            " software DENSITY (Efford et al. 2004)." ,
            br(), br(),

            "NOTE: Many features of ", strong("secr"), "are available only from the R command line.",
                 "This application is sufficient for simple analyses, but in the long run you may need to master R.",
            hr(),
            
            h2("What does secrapp do?"),
            
            "The Shiny application runs simple SECR analyses on a ", 
            a(href="https://www.stats.otago.ac.nz/secrapp/", target="_blank", "University of Otago machine"), 
            "through your web browser. No setup is required. Examples of graphical outputs are shown on the right.",
            "Code is provided for core operations; this may be copied to the command line in R.",
            br(), br(),
            
            "You can also run ", strong("secrapp"), " on your own machine directly from ",
            a(href="https://github.com/MurrayEfford/secrapp", target="_blank", "GitHub."),
            
            hr(),
            
            h2("Where can I get help?"),
            
            "The internal", 
            actionLink("helplink", "Help screen"),
            "details the options available in", strong("secrapp"), ".",
            "There is also a ", 
            a(href="https://www.otago.ac.nz/density/pdfs/secrapp-tutorial.html", target="_blank", "step-by-step guide."),
            
            "The",  a(href="https://www.otago.ac.nz/density", target="_blank", "DENSITY"),
            "webpage has other resources and links. These include", br(),
            
            a(href="https://www.otago.ac.nz/density/pdfs/secr-datainput.pdf", target="_blank", "secr-datainput.pdf"), br(),
            a(href="https://www.otago.ac.nz/density/pdfs/secr-overview.pdf", target="_blank", "secr-overview.pdf"), br(),  
            a(href="https://www.otago.ac.nz/density/pdfs/secr-tutorial.pdf", target="_blank", "secr-tutorial.pdf"), br(),
            a(href="https://www.otago.ac.nz/density/pdfs/secr-manual.pdf", target="_blank", "secr-manual.pdf"), "(details of each", strong("secr"), "function)", br(),  
            br(),
            "Online help is available on the", a(href="http://www.phidot.org/forum/index.php", target="_blank", "phidot"),
            "forum and the", a(href="https://groups.google.com/forum/#%21forum/secrgroup", target="_blank", "secr"), 
            "Google group.",
            hr(),
            
            h2("References"),
            
            "Borchers, D. L. and Efford, M. G. (2008) ",
            a(HTML("Spatially explicit maximum likelihood methods for capture&ndash;recapture studies."),
              href="https://www.otago.ac.nz/density/pdfs/Borchers & Efford Biometrics 2008.pdf"),
              em("Biometrics"), "64: 377-385.", br(),
            
            "Efford, M. G. (2004)",
            a("Density estimation in live-trapping studies.",
            href="https://www.otago.ac.nz/density/pdfs/Efford 2004 Oikos.pdf"), em("Oikos"), "106: 598-610.", br(),
            
            "Efford, M. G., Borchers D. L. and Byrom, A. E. (2009)", 
            a(HTML("Density estimation by spatially explicit capture&ndash;recapture: likelihood-based methods."),
            href = "https://www.otago.ac.nz/density/pdfs/Efford Borchers & Byrom 2009.pdf"), 
            "In: D. L. Thomson, E. G. Cooch, M. J. Conroy (eds)",
            em("Modeling Demographic Processes in Marked Populations."), "Springer. Pp 255-269.", br(),
            
            "Efford, M. G. (2020)",
            a(HTML("secr: Spatially explicit capture&ndash;recapture models."), href = "https://CRAN.R-project.org/package=secr"), 
            " R package version 4.3.0.", br(),
            
            "Efford, M. G., Dawson, D. K. and Robbins, C. R. (2004)",
            a(HTML("DENSITY: software for analysing capture&ndash;recapture data from passive detector arrays."), 
              href="https://www.otago.ac.nz/density/pdfs/Efford Dawson & Robbins 2004 ABC.pdf"),
              em("Animal Biodiversity and Conservation"), "27.1: 217-228.",
            
            br(), br(),
            
            actionLink("mainlink3", "Go to Main screen")
          ),   # end column 8
          column(4, 
            ## img(src = "tinycode.png", width = 200, alt = "code", align = "left")),
            img(src = "tinyarray.png", width = 200, alt = "array", align = "left",  title="Plot of detectors, detections and tracks"),
            img(src = "tinymoves.png", width = 200, alt = "moves", align = "left",  title="Histogram of movement distances"),
            img(src = "tinydetectfn.png", width = 200, alt = "detectfn", align = "left", title = "Fitted detection function"),
            img(src = "tinybuffer.png", width = 200, alt = "buffer", align = "left", title = "Effect of buffer width on density estimate"),
            img(src = "tinypxy.png", width = 200, alt = "pxy", align = "left", vspace = 10, title = "Map of detection probability under fitted model"),
            img(src = "tinyDxy.png", width = 200, alt = "Dxy", align = "left", vspace = 10, title = "Map of fitted density surface"),
            img(src = "tinypower.png", width = 200, alt = "power", align = "left", title = "Power for 2-sample comparison given estimated precision")
          )
        )
      ),
      tabPanel("Main screen",
        fluidRow(
          column (5, # offset = 0, style='padding:15px;',
            h2("Data input"),
            fluidRow(
              column(12, radioButtons("datasource", "", inline = TRUE, 
                choices = c("Text files", "Excel files", "Stored capthist object"),
                selected = "Text files"))
            ),
            
            conditionalPanel( condition = "input.datasource != 'Stored capthist object'",
              
              fluidRow(
                column(6,
                  wellPanel(class = "mypanel", 
                    
                    conditionalPanel( condition = "input.datasource == 'Text files'",
                      div(style="height: 80px;",  # non-breaking space ASCII 160
                        fileInput("trapfilename", # Detector layout file
                          paste0("1.", strrep(intToUtf8(160),2), "Detector layout"),   
                          multiple = TRUE,
                          accept = c("text/plain")))),
                    
                    conditionalPanel( condition = "input.datasource == 'Excel files'",
                      div(style="height: 80px;",  # non-breaking space ASCII 160
                        fileInput("trapxlsname", paste0("1.", strrep(intToUtf8(160),2), "Detector layout"), 
                          multiple = TRUE,
                          accept = c(".xls", ".xlsx"))),
                      fluidRow(
                        column(12, selectInput("trapsheet", "Sheet", 
                          choices = c("Sheet1"),
                          multiple = FALSE))   ## WARNING: MULTIPLE SHEETS NOT IMPLEMENTED; REQUIRES CHANGE TO read.traps
                      )
                    ),
                    fluidRow(
                      column(6, 
                        selectInput("detector", "Detector type", 
                          choices = c("single", "multi","proximity","count",
                            "polygon", "polygonX", "transect", "transectX"),
                          selected = "multi")
                      ),
                      conditionalPanel( condition = "input.datasource == 'Text files'",
                        column(6, 
                          br(),
                          actionLink("showtrapfilelink", HTML("<small>show file</small>"))
                        )
                      )
                    ),
                    fluidRow(
                      column(12, style="color:grey;",
                        div(#style="height: 25px;",
                          textInput("trapcovnames", "Covariate names",
                            value = "", placeholder = "e.g., traptype, habitat"))
                      )
                    ),
                    fluidRow(
                      column(12,  style="color:grey;",
                        div(# style="height: 25px;",
                          textInput("trapotherargs", "Other arguments",value = "", placeholder = "e.g., skip = 1"))
                      )
                    )
                  )  # end wellPanel  
                ),   # end column(6, )
                
                column(6, 
                  wellPanel(class = "mypanel", 
                    
                    conditionalPanel( condition = "input.datasource == 'Text files'",
                      div(style="height: 80px;", 
                        fileInput("captfilename", paste0("2.", strrep(intToUtf8(160),2), "Captures"),
                          accept = c(".txt")))
                    ),
                    
                    conditionalPanel( condition = "input.datasource == 'Excel files'",
                      div(style="height: 80px;", 
                        fileInput("captxlsname", paste0("2.", strrep(intToUtf8(160),2), "Captures"),
                          accept = c(".xls", ".xlsx"))),
                      fluidRow(
                        column(12, selectInput("captsheet", "Sheet", choices = c("Sheet1")))
                      )
                    ),
                    
                    fluidRow(
                      column(6, selectInput("fmt", label = "Format",
                        choices = c("trapID", "XY"))),
                      
                      column(6, 
                        br(), 
                        conditionalPanel( condition = "input.datasource == 'Text files'",
                          actionLink("showcaptfilelink", HTML("<small>show file</small>")),
                          br()
                        ),
                        actionLink("filtercaptlink", HTML("<small>filter</small>"))
                      )
                      
                    ),
                    
                    fluidRow(
                      column(12, style="color:grey;",
                        textInput("covnames", "Covariate names",
                          value = "", placeholder = "e.g., sex")
                      )
                    ),
                    fluidRow(
                      column(12, style="color:grey;",
                        textInput("captotherargs", "Other arguments",
                          value = "", placeholder = "e.g., skip = 1")
                      )
                    ),
                    fluidRow(
                      conditionalPanel(condition = "output.filterCapt",
                        column(12, style="color:grey;",
                          textInput("filtercapttext", "Filter",
                            value = "", placeholder = "e.g., session = 1")
                        )
                      )
                    )
                  )   # end wellPanel
                )   # end column(6,)
              )   # end fluidRow
            ),  # end conditionalPanel Text, xls
            
            conditionalPanel( condition = "input.datasource == 'Stored capthist object'",
              wellPanel(class = "mypanel",
                fluidRow(
                  # known issue: 'accept' does not restrict types in RStudio,
                  # only in browsers
                  # https://github.com/rstudio/shiny/issues/951
                  column(9, fileInput("importfilename", 
                    "Import capthist from Rds file",
                    accept = c(".RDS", ".Rds",".rds"))),
                  column(3, br(), actionLink("clearimportlink", "Clear"))
                )
              )
            ),  # end conditionalPanel stored capthist
            
            h2("Model"),
            wellPanel(class = "mypanel", 
              fluidRow(
                column(3, selectInput("detectfnbox", "Detection function",
                  choices = c("HN", "HR", "EX","HHN", "HHR", "HEX", "HVP"),
                  selected = "HN"),
                  uiOutput("detectfnui") 
                ),
                column(3, radioButtons("likelihoodbtn", "Likelihood", choices = c("Full", "Conditional"))),
                column(3, radioButtons("distributionbtn", label = "Distribution of n",
                  choices = c("Poisson", "Binomial"))),
                column(3, style="color:grey;",
                  selectInput("hcovbox", label = "Mixture hcov",
                    choices = c("none"), selected = "none", width = 160)
                )
              ),
              fluidRow(
                column(7, textInput("model", "Model", value = "D~1, g0~1, sigma~1")),
                column(5, HTML("<small><strong>Habitat mask</strong></small>","&nbsp;"),
                  actionLink("masklink", HTML("<small> edit</small>")), br(),
                  verbatimTextOutput("maskdetailPrint")
                )
              ),
              fluidRow(
                column(12,  style="color:grey;",
                  textInput("otherargs", "Other arguments", value = "", 
                    placeholder = "e.g., details = list(fastproximity = FALSE), binomN = 1"))
              )
            ),   # end wellPanel Model
            
            
            h2("Actions"),
            fluidRow(
              column(3, actionButton("fitbtn", "Fit model",  width = 130,
                title = "Fit spatially explicit capture-recapture model to estimate density and update Results")),
              column(3, actionButton("secrhelpbtn", "secr help",  width = 130,
                title = "Open secr help index")),
              column(3, uiOutput("secrdesignurl")),  ## switch to secrdesign, with parameters
              column(3, actionLink("optionslink", HTML("<small>Go to Options</small>")))
            ),
            
            br(),
            fluidRow(
              column(3, actionButton("resetbtn", "Reset all", width = 130, 
                title = "Reset all inputs to initial values")),
              # column(3, bookmarkButton(width = 130)), # NOT WORKING 1.3
              column(3, actionButton("dummybookmarkbutton", "Bookmark", width = 130)),
              column(3),
              column(3, helpText(HTML("F11 full screen")))
            )
          ), # end left side column (5, )
          
          column (6,
            h2("Results"),
            
            fluidRow(
              column(6, 
                radioButtons("resultsbtn", label = "", 
                  inline = TRUE, choices = defaultresultsbtn)
                ),
              
              column(4,  
                textInput("otherfunction", label="", placeholder = "e.g., RPSV(ch, CC=TRUE)")
              ),
              conditionalPanel("output.modelFitted", 
                column(2, br(), downloadLink("savebtn", HTML("<small>Save fitted model</small>"))))
            ),
            
            fluidRow(
              column(12,verbatimTextOutput("resultsPrint"))
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
                    fluidRow(
                      column(10, style = 'padding:0px;', 
                        plotOutput("arrayPlot", 
                          click = clickOpts(id = "arrayClick", clip = FALSE),
                          hover = hoverOpts(id ="arrayHover", delayType = "throttle"))),
                      column(2, br(),
                        conditionalPanel("output.multisession=='true'",
                          numericInput("sess", "Session", min = 1, max = 2000, 
                            step = 1, value = 1)),
                        br(),
                        conditionalPanel("input.gridlines != 'None'",
                        uiOutput("uigridlines") ),
                        br(), 
                        uiOutput('xycoord'))
                    ),
                    conditionalPanel("output.capthistLoaded",
                      fluidRow(
                        column(2, offset = 1, checkboxInput("tracks", "All tracks", FALSE)),
                        column(2, numericInput("animal", "Select animal", min = 0, max = 2000, 
                          step = 1, value = 1)),
                        column(4, conditionalPanel("input.animal>0", 
                          verbatimTextOutput("animalIDPrint")
                        )),
                        column(3, conditionalPanel("output.modelFitted", checkboxInput("fxi", "fxi contour", FALSE)))
                      )),
                    conditionalPanel("output.usage", 
                      fluidRow(
                        column(2, offset = 1, checkboxInput("usageplot", "Usage", FALSE))
                        )
                    )
                    
                  ),
                  tabPanel("Moves", 
                    fluidRow(
                      column(8, plotOutput("movesHist", height = 360)),
                      column(4, br(), br(),
                        numericInput("nbar", "Nominal breaks",
                          min = 0,
                          max = 100,
                          value = 10,
                          step = 1,
                          width = 120),
                        uiOutput("sessionnumberui"),
                        verbatimTextOutput("movesPrint"),
                        conditionalPanel("output.nontrap == 'true'",
                          uiOutput("moveswarningui") )
                      )
                    )),
                  tabPanel("Detectfn", plotOutput("detnPlot", height = 320)),
                  tabPanel("Buffer", plotOutput("esaPlot", height = 320)),
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
                      column(2, checkboxInput("Dshowpopn", "random popn", value = FALSE),
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
                    column(8, 
                      numericInput("buffer", "Buffer width (m)",
                        min = 0,
                        max = 100000,
                        value = 100,
                        step = 5,
                        width = 180),
                      tags$div(title = "Resolution of mesh - number of columns in x-direction",
                        numericInput("habnx", "Mesh dimension nx",
                        min = 10,
                        max = 1000,
                        value = 32,
                        step = 1,
                        width = 180)
                      )
                    ),
                    column(4,
                      br(), br(),
                      #actionLink("suggestbufferlink", "suggest width",
                        actionLink("suggestbufferlink", HTML("<small>suggest width</small>"),
                          width = 130,
                        title = "Based on fitted model, if available, otherwise RPSV. Seeks buffer truncation bias <= 1%"))
                  ),
                  fluidRow(
                    column(9, 
                      tags$div(title = "Mask type 'trapbuffer' is trimmed to exclude points further than the buffer distance from any detector",
                        radioButtons("maskshapebtn", label = "Shape",
                        choices = c("Rectangular", "Trap buffer"), 
                        selected = "Trap buffer", inline = TRUE)
                      )
                    ),
                    column(3,
                      br(),
                      actionLink("clearbufferspec", HTML("<small>reset</small>"))
                    )
                    
                  )
                ),
                wellPanel(class = "mypanel", 
                  div(style="height: 80px;",
                    fileInput("maskpolyfilename", 
                      paste0("Mask polygon file(s)", strrep(intToUtf8(160), 3), " (optional)"),
                      accept = c('.shp','.dbf','.sbn','.sbx',
                        '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                      multiple = TRUE)),
                  uiOutput("habitatfile"),
                  conditionalPanel ("output.maskpolygonsready", 
                    
                    fluidRow(column(9,
                        tags$div(title = "Do polygons represent habitat ('include') or non-habitat ('exclude')?",
                          radioButtons("includeexcludebtn", label = "",
                            choices = c("Include", "Exclude"), 
                            selected = "Include", inline = TRUE)
                        ),
                      
                      ),
                      column(3,
                        br(),
                        actionLink("clearpolygondata", HTML("<small>clear</small>"))
                      )
                    )
                  )
                ),
                wellPanel(class = "mypanel",
                  div(style="height: 80px;",
                    fileInput("maskcovariatefilename",
                      paste0("Spatial data source for covariates (optional)"),
                      accept = c('.shp', '.dbf', '.sbn', '.sbx',
                        '.shx', '.prj', '.txt', '.rds'),
                      multiple = TRUE)),
                  uiOutput("covariatefile"),
                  conditionalPanel ("output.maskcovariatefileready", 
                    fluidRow(
                      column(9, 
                        checkboxInput("dropmissing", 
                          "Drop point if any covariate missing", value = FALSE)
                      ),
                      column(3,
                        actionLink("filtermask", HTML("<small>filter</small>")), br(),
                        actionLink("clearspatialdata", HTML("<small>clear</small>"))
                      )
                    ),
                    conditionalPanel("output.filterMask",
                    fluidRow(
                      column(12, textInput("filtermasktext", "Filter",
                        placeholder = "e.g., forest == 'beech'"))
                    ))
                  )
                )
              ),
              tabPanel("File", 
                wellPanel(class = "mypanel", 
                  div(style = "height: 80px;",
                    fileInput("maskfilename", "Mask file",
                      accept = c('.txt'), 
                      multiple = FALSE))
                )
              ) 
            ),
            fluidRow(
              column(6, actionLink("mainlink", "Return to Main screen")),
              column(6, conditionalPanel ("output.maskready", 
                downloadLink("savemask", "Save to text file")))
            )
          ),
          column(5, plotOutput("maskPlot"),
            conditionalPanel ("output.maskready", 
              fluidRow(
                column(2, offset = 1, checkboxInput("dotsbox", "dots", value = FALSE)),
                column(2, 
                  checkboxInput("frame", "frame", value = TRUE),
                  conditionalPanel ("output.maskpolygonsready", 
                    checkboxInput("showpoly", "polygons", value = FALSE)
                  )
                ),
                column(3, 
                  checkboxInput("maskedge2", "show edge", value = FALSE),
                  conditionalPanel ("output.maskcovariatesready", 
                    checkboxInput("legend", "legend", value = FALSE)
                    )
                  ),
                column(3, 
                  conditionalPanel ("output.maskcovariatesready", 
                    selectInput("maskcov", "Covariate", choices = "none")
                  ),
                  conditionalPanel("output.multisession=='true'",
                    numericInput("masksess", "Session", min = 1, max = 2000, 
                      step = 1, value = 1))
                  )
              )
            )   
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
          column(2, 
            wellPanel(
              h2("Fields"),
              fluidRow(
                column(6, actionButton("selectfieldsbtn", "Select", title = "Choose fields to display")), 
                column(6, 
                  actionLink("selectnofieldslink", "None", title = "Unselect all fields"), br(), 
                  actionLink("selectallfieldslink", "All", title = "Select all fields"))
              ),
              br(),
              h2("Analyses"),
              fluidRow(
                column(6, actionButton("selectanalysesbtn", "Select", title = "Choose analyses to display")), 
                column(4, 
                  actionLink("selectnoanalyseslink", "None", title = "Select no analyses"), br(),
                  actionLink("selectallanalyseslink",  "All",  title = "Select all analyses")
                )
              ), 
              br(), 
              h2("Download"),
              fluidRow(
                column(6, downloadButton("downloadSummaryrds", ".rds", 
                  title = "Save as RDS file; restore in R with e.g., readRDS(`summary.rds')")), 
                column(6, downloadButton("downloadSummary", ".csv", 
                  title = "Save as comma-delimited text (csv) file"))
              ),
              fluidRow(
                column(12, checkboxInput("keepselectedbox", "Keep only selected analyses", FALSE ))
              )
            ),
            
            fluidRow(
              column(5, offset=1,
                conditionalPanel("output.selectingfields == 'TRUE'",
                  checkboxGroupInput("fields1", "",
                    choices = c("date", "time", "note", "traps", "captures", "filter",
                      "n", "r", "ndetectors", "noccasions",
                      "usagepct", "maskbuffer", "masknrow", "maskspace",
                      "likelihood", "distribution","model", "hcov"
                    ),
                    selected = c("date", "time", "note", "traps", "captures", "filter",
                      "n", "r", "ndetectors", "noccasions",
                      "usagepct", "maskbuffer", "masknrow", "maskspace",
                      "likelihood", "distribution", "model"
                    )
                  )
                )
              ),
              column(6,
                conditionalPanel("output.selectingfields == 'TRUE'",
                  checkboxGroupInput("fields2", "",
                    choices = c("detectfn", 
                      "npar", "logLik", "AIC", "dAIC",
                      "D", "se.D", "RSE.D", 
                      "g0", "se.g0", "lambda0", "se.lambda0","sigma", "se.sigma", "z", "se.z",
                      "k", "proctime"
                    ),
                    selected = c("detectfn",
                      "npar", "logLik", "AIC", "dAIC",
                      "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", "se.sigma",
                      "k", "proctime"
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(6, offset = 1,
                conditionalPanel("output.selectinganalyses == 'TRUE'",
                checkboxGroupInput("analyses", "", choices = character(0))
              )
              )
            )
            
          ),
          column(10, 
            # h2("Results"),
            div(tableOutput("summarytable"), style = "width:100%; overflow-x: scroll")
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
            conditionalPanel("output.capthistLoaded", 
              wellPanel(class = "mypanel",
                fluidRow(
                  column(8, downloadLink("exportbtn", "Export current capthist to Rds file", 
                    title = "Save as RDS file"))),
                br()
              )
            ),
            h2("Model fitting"),
            wellPanel(class = "mypanel",
              fluidRow(
                column(11, selectInput("method", "Maximization method",
                  ## 2020-02-10 choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                  choices = c("Newton-Raphson", "Nelder-Mead"),
                  selected = "Newton-Raphson", width=160)),
                column(1, br(), br(), actionLink("incrementtime", label = "."))
              ),
              fluidRow(
                column(6, numericInput("ncores", "Number of cores", 
                  min = 1, 
                  max = availablecores, 
                  step = 1, 
                  value = defaultcores,
                  width = 100)),
                column(6, br(), uiOutput("ncoresui") )
              ),
              fluidRow(
                column(8, uiOutput("timelimitui") )
              )
            ),
            h2("Summary"),
            wellPanel(class = "mypanel",
              fluidRow(
                column(12, numericInput("dec", "Decimal places", 
                  min = 0, 
                  max = 8, 
                  value = 4, 
                  width=160))
              ),
              fluidRow(
                column(12, textInput("title", "", value = "", 
                  placeholder = "note for Summary"))
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
                column(6, checkboxInput("entireregionbox", "Show entire region", value = FALSE, width = 160)),
                column(6, checkboxInput("varycol", "Vary colours", FALSE)),
              ),
              fluidRow(
                column(6, numericInput("arrayborder", "Border  m", value = 20, width = 160, min = 0, max = 1000000, step = 5)),
                column(6, checkboxInput("arrayframe", "Frame", value = FALSE))
              ),
              br(),
              fluidRow(
                column(6, numericInput("rad", "Radial displ. (m)", 
                  value = 5,
                  min = 0, 
                  max = 5000, 
                  step = 1, 
                  width = 160)),
                column(6, numericInput("cex", "Point size (cex)", 
                  value = 1, 
                  min = 0.1, 
                  max = 5, 
                  step = 0.1, 
                  width = 160))
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
            ),
            h2("Dxy plot"),
            wellPanel(class = "mypanel", 
              fluidRow(
                column(12, textInput("Dxycol", "R code for colours",
                  value = "heat.colors(15, alpha = 1, rev = TRUE)"), br(),
                  textInput("showHRcol", "Colour of 95% HR", value = "grey50")
                )
              )
            )
          ),
          
          column(3,
            h2("Moves histogram"),
            wellPanel(class = "mypanel", 
              fluidRow(
                column(6,  checkboxInput("movesallbox", "Combine sessions",
                  value = FALSE,
                  width = 180),
                  checkboxInput("withinsessiononly", "Within-session only",
                    value = FALSE,
                    width = 180)
                ))),
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
            ),
            actionLink("mainlink2", "Return to Main screen")
            
          )
        )
      ),
      tabPanel("Help",
        withMathJax(includeMarkdown("help.rmd"))
      ),
      tabPanel("About",
        h2("secr app 1.3"), br(),
        
        h5(paste("This Shiny application provides an interface to the R package 'secr', version", 
          packageDescription("secr")$Version), "."),
        br(),
        h5("Copyright 2019, 2020 Murray Efford"),
        "The application is released under the ",
        a("GNU General Public License Version 3.0", href="https://www.gnu.org/licenses/gpl-3.0.txt", target="_blank"), br(),
        br(),
        h5("For further information see "), 
        a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
        a("CRAN.R-project.org/package=secr", href="https://CRAN.R-project.org/package=secr", target="_blank"), br(),
        a("https://github.com/MurrayEfford/secrapp", href="https://github.com/MurrayEfford/secrapp", target="_blank"), br(),
        br(),
        h5("Citation"),
        h5("[The preferred citation for this package has not been finalised]")
      )
      
    )   # end navlistpanel
  )       # end fluidPage
}

############################################################################################
# Define server logic

server <- function(input, output, session) {
  
  desc <- packageDescription("secr")
  summaryfields <- c("date", "time", "note", "traps", "captures", "filter", 
    "n", "r", "ndetectors", "noccasions", "usagepct", "maskbuffer", "masknrow", 
    "maskspace", "likelihood", "distribution", "model", "hcov", 
    "detectfn", "npar", "logLik", "AIC", "dAIC",
    "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", 
    "se.sigma", "z", "se.z", "k", "proctime"
  )
  
  fieldgroup1 <- 1:18
  fieldgroup2 <- 19:36
  
  polygondetectors <- c("polygon", "polygonX", "transect", "transectX")
  hazarddetectfn <- c("HHN", "HHR", "HEX", "HVP")
  
  ## for cycling through animals at one detector 2019-03-08
  lasttrap <- 0
  clickno <- 0
  
  disable("fitbtn")
  disable("captfilename")
  disable("captxlsname")
  disable("dummybookmarkbutton")
  
  showNotification(paste("secr", desc$Version, desc$Date), id = "lastaction",
    closeButton = FALSE, type = "message", duration = seconds)
  output$selectingfields <- renderText('false')
  output$selectinganalyses <- renderText('false')
  output$multisession <- renderText('false')
  output$nontrap <- renderText('false')
  outputOptions(output, "selectingfields", suspendWhenHidden = FALSE)
  outputOptions(output, "selectinganalyses", suspendWhenHidden = FALSE)
  outputOptions(output, "multisession", suspendWhenHidden = FALSE)
  outputOptions(output, "nontrap", suspendWhenHidden = FALSE)
  
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
  
  outputOptions(output, "maskready", suspendWhenHidden=FALSE)
  outputOptions(output, "maskcovariatesready", suspendWhenHidden=FALSE)
  outputOptions(output, "maskcovariatefileready", suspendWhenHidden=FALSE)
  outputOptions(output, "maskpolygonsready", suspendWhenHidden=FALSE)
  outputOptions(output, "modelFitted", suspendWhenHidden=FALSE)
  outputOptions(output, "capthistLoaded", suspendWhenHidden=FALSE)
  outputOptions(output, "filterCapt", suspendWhenHidden=FALSE)
  outputOptions(output, "filterMask", suspendWhenHidden=FALSE)
  outputOptions(output, "usage", suspendWhenHidden=FALSE)
  
  
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
  #-----------------------------------------------------------------------------

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
          paste0("D=", as.character(round(density(), input$dec))),
          paste0(detectrv$value, "=", as.character(round(detect0(), input$dec))),
          paste0("sigma=", as.character(round(sigma(), input$dec))))
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
            type = "error", id = "nofile", duration = NULL)
        }
        else  if (!requireNamespace("rgdal"))
          showNotification("need package rgdal to read shapefile", 
            type = "error", id = "norgdal", duration = NULL)
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
    OK <- FALSE
    df <- data.frame(
      date = format(Sys.time(), "%Y-%m-%d"),
      time = format(Sys.time(), "%H:%M:%S"),
      note = input$title,
      
      traps = if (input$datasource=="Text files") 
        paste(input$trapfilename$name[1], if (nrow(input$trapfilename)>1) "etc." else "")
      else if (input$datasource=="Excel files") 
        paste(input$trapxlsname$name[1], if (nrow(input$trapxlsname)>1) "etc." else "")
      else "",
      captures = if (input$datasource=="Text files") input$captfilename$name[1]
      else if (input$datasource=="Excel files") input$captxlsname$name[1] 
      else if (is.null(input$importfilename)) "" else input$importfilename$name[1],
      filter = if (filtercaptrv$value && input$filtercapttext!="") input$filtercapttext else "",
      ndetectors = ndetectors()[input$sess],
      noccasions = noccasions()[input$sess],
      usagepct = usagepct()[input$sess],
      maskbuffer = if (input$masktype=='Build') input$buffer else input$maskfilename$name[1],
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
      OK <- TRUE
    }
    sumrv$value <- rbind (sumrv$value, df)
    
    if (nrow(sumrv$value)>0) {
      rownames(sumrv$value) <- paste0("Analysis", 1:nrow(sumrv$value))
      
      updateCheckboxGroupInput(session, "analyses", 
        choices = rownames(sumrv$value),
        selected = c(input$analyses, paste0("Analysis", nrow(sumrv$value)))
      )
    }
    return(OK)
  }
  ##############################################################################
  
  getSPcode <- function (inputfilename, varname, comment = TRUE) {
    filename <- inputfilename[1,1]
    if (is.null(filename)) {
      return("")
    }
    else {
      ext <- tolower(tools::file_ext(filename))
      if (ext == "txt") {
        code <- paste0( 
          if (comment) "# coordinates from text file\n" else "",
          "coord <- read.table('", filename, "')   # read boundary coordinates\n",
          varname, " <- secr:::boundarytoSP(coord)  # convert to SpatialPolygons\n")
      }
      else if (ext %in% c("rdata", "rda")) {
        objlist <- load(inputfilename[1,4])
        code <- paste0( 
          if (comment) "# SpatialPolygons from RData file\n" else "",
          "objlist <- load('", filename, "')\n",
          varname, " <- get(objlist[1]) \n")
      }
      else if (ext == "rds") {
        code <- paste0( 
          if (comment) "# SpatialPolygons from RDS file\n" else "",
          varname, " <- readRDS('", filename, "') \n")
      }
      else {
        code <- paste0(
          if (comment) "# ESRI polygon shapefile\n" else "",
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
    sheet <- ""
    args <- input$trapotherargs
    if (args != "") {
      args <- paste0(", ", args)
    }
    if (!is.null(traprv$data) && is.null(importrv$data)) {
      if (input$datasource == "Text files") {
        filename <- input$trapfilename[,"name"]
      }
      else {
        filename <- input$trapxlsname[,"name"]
        if (length(filename)==1) {
          dataname <- input$trapxlsname[1,"datapath"]
          if (!input$trapsheet %in% readxl::excel_sheets(dataname)) return()
          if(!grepl("sheet", args)) {
            sheet <- paste0(", sheet = '", input$trapsheet, "'")
          }
        }
      }
      
      ## 2020-08-16
      if (length(filename)>1) {
        cr <- ",\n   "
        filename <- paste0("c('", paste(filename, collapse = "','"), "')")
      }
      else {
        cr <- ", "
        filename <- paste0("'", filename, "'")
      }
      code <- paste0("array <- read.traps (",
        filename,
        cr, "detector = '", input$detector, "'", 
        args, sheet, ")\n")
      cov <- covariates(traprv$data)
      if (!is.null(cov) && !ms(cov)) {     # defer hard case
        ncov <- if (ms(cov)) ncol(cov[[1]]) else ncol(cov)
        covnames <- getcovnames(input$trapcovnames, ncov, 'T', TRUE)
        covnamecode <- if (ms(cov)) ""   # defer hard case
        else paste0("names(covariates(array)) <- ", covnames, "\n")
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
    removeNotification("badpoly")
    
    if (input$masktype == 'Build') {
      if (is.null(traprv$data))
        return("\n")
      else {
        type <- if (input$maskshapebtn == 'Rectangular') 'traprect' else 'trapbuffer'
        buffer <- as.character(round(input$buffer,2))
        polycode <- ""
        polyhabitat <- ""
        sourcecode <- ""
        addcovcode <- ""
        dropcode <- ""
        
        if (!is.null(input$maskpolyfilename)) { 
          polyhabitat <- input$includeexcludebtn == "Include"
          polycode <- getSPcode(input$maskpolyfilename, "poly")
        }
        if (!is.null(input$maskcovariatefilename)) { 
          ext <- tolower(tools::file_ext(input$maskcovariatefilename[1,1]))
          if (ext == "txt") {
            sourcecode <- paste0(
              "covariatesource <- read.mask('", 
              input$maskcovariatefilename[1,1], 
              "')\n"
            )
          }
          else {
            sourcecode <- getSPcode(input$maskcovariatefilename, 
              "covariatesource", comment = FALSE)
          }
          addcovcode <- "mask <- addCovariates(mask, covariatesource)\n"
          if (input$dropmissing) {
            dropcode <- paste0(
              "covariatesOK <- apply(!is.na(covariates(mask)), 1, all)\n",
              "mask <- subset(mask, covariatesOK)\n"
            )
          }
        }
        trps <- if (is.null(importrv$data)) "array" else "traps(ch)"
        paste0(
          polycode,
          "mask <- make.mask (", trps,  
          ", buffer = ", buffer, 
          ", nx = ", input$habnx, 
          ", type = '", type, "'",  
          if (polycode == "") "" else ",\n    poly = poly",
          if (polycode == "") "" else ", poly.habitat = ", polyhabitat,
          ")\n",
          sourcecode,
          addcovcode,
          dropcode)
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
    sheet <- ""
    if (!is.null(importrv$data)) {
      code <- paste0("ch <- readRDS('", input$importfilename[1,"name"], "')\n")
    }
    else {
      if (!is.null(captrv$data)) {
        args <- input$captotherargs

        if (args != "") {
          args <- paste0(", ", args)
        }
        
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
          covnames <- getcovnames(input$covnames, ncov, 'C', TRUE)
          cov <- if (covnames == "") "" else paste0(", covnames = ", covnames)
        }
        
        if (input$datasource == 'Text files') {
          filename <- input$captfilename[1,"name"]
          dataname <- input$captfilename[1,"datapath"]
          nfield <- count.fields(dataname)[1]
          mincol <- (input$fmt == "XY") + 4
          # ensure correct type for trapID
          if (nfield>=mincol && input$fmt == "trapID" && ! grepl("colClasses", args)) {
            colClass <- c("'character'", "'character'","'integer'","'character'", rep(NA, nfield-4))
            args <- paste0(args, ", colClasses = c(", paste0(colClass, collapse = ","), ")")
          }
          code <- paste0("capt <- read.table('", filename, "'", args, ")\n")
        }
        else {
          filename <- input$captxlsname[1,"name"]
          dataname <- input$captxlsname[1,"datapath"]
          if (!input$captsheet %in% readxl::excel_sheets(dataname)) return()
          
          if(!grepl("sheet", args)) {
            sheet <- paste0(", sheet = '", input$captsheet, "'")
          }
          code <- paste0("capt <- readxl::read_excel('", filename, "'", args, sheet, ")\n")
        }
        
        code <- paste0(code, "ch <- make.capthist (capt, traps = array", fmt, cov, ")\n")
        
        if (filtercaptrv$value && !input$filtercapttext=="") {
          code <- paste0(code, "ch <- subset(capthist = ch, ", input$filtercapttext, ")\n")
        }
        
        if (comment) {
          tmp <- lapply(strsplit(code, "\n")[[1]], function(x) paste0("# ", x))
          tmp$sep <- "\n"
          code <- do.call(paste, tmp)
        }
      }
    }
    code
  }
  
  # ##############################################################################
  
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
    nc <- input$ncores
    ncores <- paste0(", ncores = ", nc)
    otherargs <- if (input$otherargs=="") "" else paste0(",\n      ", input$otherargs)
    code <- paste0(
      "fitted <- secr.fit(ch, mask = mask, detectfn = ", detfn, CL, hcov, ", \n",
      "      ", model, ", trace = FALSE", distn, method, ncores, otherargs, ")\n"
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
    
    removeNotification("nofit")
    if (!LLonly) {
      progress <- Progress$new(session, min = 1, max = 15)
      on.exit(progress$close())
      progress$set(message = 'Fitting...', detail = '')
    }
    CL <- input$likelihoodbtn != "Full"
    
    model <- modellist()
    
    otherargs <- try(eval(parse(text = paste0("list(", input$otherargs, ")"))), silent = TRUE)
    if (inherits(otherargs, "try-error") && !LLonly) {
      showNotification("model fit failed - check other arguments",
        type = "error", id = "nofit", duration = NULL)
      fit <- NULL
    }
    else {
      otherargs <- otherargs[!(names(otherargs) %in% c('capthist','trace','mask','model','detectfn'))]
      nc <- input$ncores
      args <- c(list(capthist = capthist(), 
        trace = FALSE,
        mask = mask(), 
        model = model,
        detectfn = input$detectfnbox,
        method = input$method,
        ncores = nc),
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
          type = "error", id = "nofit", duration = NULL)
        fit <- NULL
      }
    }
    if (LLonly) {
      return(fit)
    }
    else {
      updateRadioButtons(session, "resultsbtn", label = "", 
        inline = TRUE, choices = fittedresultsbtn)
      updateTextInput(session, "otherfunction", placeholder = "e.g., vcov(fitted)")

      fitrv$value <- fit
      if (length(fit)>0) {
        fitrv$dsurf <- predictDsurface(fit)
      }
      else {
        fitrv$dsurf <- NULL
      }
      
      if (fit$fit$minimum == 1e+10) {
        showNotification("Model failed to fit", id = "lastaction",
          type = "error", duration = NULL)
      }
      else {
        OK <- try(addtosummary())
        if (inherits(OK, 'try-error')) {
          showNotification("Problem adding results to summary", id = "lastaction",
            type = "error", duration = NULL)
        }
        else {
          showNotification("Model fitted", id = "lastaction",
            closeButton = FALSE, type = "message", duration = seconds)
          
          if (input$masktype == "Build") {
            x <- suppressWarnings(secr:::bufferbiascheck(fit, 
              buffer = round(input$buffer,2), biasLimit=0.01))
            if (!is.null(x)) {
              showNotification(x, id = "lastaction",
                type = "warning", duration = NULL)
              
            }
          }
          
          if (!is.null(fit$fit$hessian) && !is.null(fit$beta.vcv)) {
            svtol <- 1e-5
            eigH <- eigen(fit$fit$hessian)$values
            eigH <- abs(eigH)/max(abs(eigH))   
            
            eig <- round(eigH, -log10(svtol))
            rankH <- length(which(eigH > svtol))
            nbeta <- nrow(fit$beta.vcv)
            if (rankH < nbeta) {
              showNotification("at least one beta parameter is not identifiable (svtol=1e-5)", 
                id='lastaction', type = "warning", duration = NULL)
            }
          }
        }
      }
      
    }
    if (is.null(fitrv$value)) {
      updateRadioButtons (session, "resultsbtn", label = "", 
        inline = TRUE, choices = defaultresultsbtn)
    }
  }
  ##############################################################################
    
  maskOK <- function () {
    if (is.null(polyrv$data) || is.null(traprv$data)) {
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
  
  ## reactiveValues
  
  bookmarkrv <- reactiveValues(value = FALSE, captfilename="", 
    captxlsname="", maskfilename="")
  capttextrv <- reactiveValues(value = FALSE)
  current <- reactiveValues(unit = "ha")
  detectrv <- reactiveValues(value = 'g0')
  Drv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
  filtercaptrv <- reactiveValues(value = FALSE)
  filtermaskrv <- reactiveValues(value = FALSE)
  fitrv <-  reactiveValues(value = NULL)
  poprv <- reactiveValues(v = 0)  # used to invalidate and re-plot popn
  pxyrv <- reactiveValues(current = FALSE, xy = NULL, value = NULL)
  RSErv <- reactiveValues(current = FALSE, value = NULL, adjRSE = NULL)
  selectingfieldsrv <- reactiveValues(value = FALSE)
  selectinganalysesrv <- reactiveValues(value = FALSE)
  sumrv <- reactiveValues(value = read.csv(text = paste(summaryfields, collapse = ", ")))
  timerv <- reactiveValues(timewarning = timewarning, timelimit = timelimit)
  traptextrv <- reactiveValues(value = FALSE)

  
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
  
  covariaterv <- reactiveValues(
    data = NULL,
    covariates = NULL,
    names = character(0),
    clear = FALSE
  )
  
  currentIDrv <- reactiveValues(
    value = ""
  )
  
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
  
  #############################################################################
  ## read or re-read trap file
  
  observeEvent(c(input$trapfilename, input$trapxlsname, input$detector, 
    input$trapcovnames, input$trapotherargs, input$trapsheet), 
    ignoreInit = FALSE, {
    req(!traprv$clear)
    if (input$datasource == 'Text files') {
      req(input$trapfilename)
    }
    else {
      req(input$trapxlsname)
    }
    removeNotification("badtrapotherargs")
    removeNotification("badtrap")
    removeNotification("badcapt")
    
    if (!bookmarkrv$value) {
      reset('importfilename')
      importrv$data <- NULL
      importrv$clear <- TRUE
      
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
  ## read import file
  observe({
    req(input$importfilename)
    removeNotification("badcapt")
    req(!importrv$clear)
    ch <- readRDS(input$importfilename[1,"datapath"])
    if (inherits(ch, 'capthist')) {
      importrv$data <- ch
      traprv$data <- traps(ch)
      if (detector(traprv$data)[1] %in% polygondetectors) {
        updateSelectInput(session, "detectfnbox", choices = hazarddetectfn, 
          selected = "HHN")
        disable('suggestbufferlink')
      }
      else {
        updateSelectInput(session, "detectfnbox", choices = c('HN','HR','EX', hazarddetectfn), 
          selected = "HN")
        enable('suggestbufferlink')
      }
      showNotification("capthist imported", closeButton = FALSE, 
        type = "message", id = "lastaction", duration = seconds)
    }
    else {
      importrv$data <- NULL
      traprv$data <- NULL
      showNotification("not a valid capthist Rds", 
        type = "error", id = "badcapt", duration = NULL)
    }
  })
  
  ##############################################################################
  ## read capture file
  observe({
    req(!captrv$clear)
    removeNotification("badcapt")
    sheet <- ""
    if (input$datasource == 'Text files') {
      req(input$captfilename)
      # temporary fix for bad shiny bookmark
      if (bookmarkrv$value) {
        captdataname <- bookmarkrv$captfilename
      }
      else {
        captdataname <- input$captfilename[1,"datapath"]
      }
      captfilename <- input$captfilename[1,"name"]
    }
    else {
      req(input$captxlsname)
      # temporary fix for bad shiny bookmark
      if (bookmarkrv$value) {
        captdataname <- bookmarkrv$captlsname
      }
      else {
        captdataname <- input$captxlsname[1,"datapath"]
      }
      
      captfilename <- input$captxlsname[1,"name"]
      if (!input$captsheet %in% readxl::excel_sheets(captdataname)) return()
      
      samexls <- captfilename == input$trapxlsname[1,"name"]
      if (samexls && input$captsheet %in% input$trapsheet) {
        showNotification(id = "lastaction", type = "error", duration = NULL,
          "cannot use same xls sheet")
        captrv$data <- NULL
        return()
      }
      
      if (!grepl('sheet', input$captotherargs)) {
        sheet <- paste0(", sheet = '", input$captsheet, "'")
      }
    }
    args <- input$captotherargs
    mincol <- (input$fmt == "XY") + 4
    tempargs <- try(eval(parse(text = paste0("list(", args, ")"))), silent = TRUE)
    if (inherits(tempargs, "try-error")) {
      showNotification("arguments incomplete or invalid", type = "error", 
        id = "badcaptotherargs", duration = NULL)
    }
    else {
      removeNotification(id = "badcaptotherargs")
      if (args != "")
        args <- paste0(", ", args)
      if (grepl('.xls', captdataname)) {
        readcaptcall <- paste0("readxl::read_excel(captdataname", args, sheet, ")")
      }
      else {
        # ensure correct type for trapID
        nfield <- count.fields(captdataname)[1]
        if (nfield >= mincol && input$fmt == "trapID" && ! grepl("colClasses", args)) {
          colClass <- c("character", "character","integer","character", rep(NA, nfield-4))
          args <- paste0(args, ", colClasses = colClass")
        }
        readcaptcall <- paste0("read.table (captdataname", args, ")")
      }
      captrv$data <- try(eval(parse(text = readcaptcall)))
      captrv$data <- as.data.frame(captrv$data)
      captrv$clear <- TRUE
      if (!inherits(captrv$data, "data.frame") || ncol(captrv$data) < mincol) {
        showNotification("invalid capture file or arguments; try again",
          type = "error", id = "badcapt", duration = NULL)
        captrv$data <- NULL
      }
      else {
        showNotification("capthist loaded", closeButton = FALSE, 
          type = "message", id = "lastaction", duration = seconds)
      }
    }
    
  })
  ##############################################################################
  
  ## read mask polygon file
  observe({
    req(input$maskpolyfilename)
    req(!polyrv$clear)
    removeNotification("badpoly")
    polyrv$data <- readpolygon(input$maskpolyfilename)
    if (!inherits(polyrv$data, "SpatialPolygons")) {
      showNotification("invalid polygon file; try again",
        type = "error", id = "badpoly")
      polyrv$data <- NULL
    }
  })
  ##############################################################################
  
  ## read mask covariate source file
  observe({
    req(input$maskcovariatefilename)
    req(!covariaterv$clear)
    ext <- tolower(tools::file_ext(input$maskcovariatefilename[1,1]))
    if (ext == "txt") {
      covariaterv$data <- read.mask(input$maskcovariatefilename[1,4], 
        header = TRUE, stringsAsFactors = TRUE)
    }
    else if (ext == "rds") {
      covariaterv$data <- readRDS(input$maskcovariatefilename[1,4])
    }
    else {
      # shapefile
      covariaterv$data <- readpolygon(input$maskcovariatefilename)
    }
    
    if (!inherits(covariaterv$data, c("SpatialPolygonsDataFrame", "mask"))) {
      showNotification("invalid covariate file; try again",
        type = "error", id = "bad covariate file")
      covariaterv$data <- NULL
      covariaterv$names <- character(0)
    }
    else {
      if (inherits(covariaterv$data, "mask")) {
        covariaterv$names <- names(covariates(covariaterv$data))
      }
      else {
        covariaterv$names <- names(covariaterv$data)
      }
    }
    updateSelectInput(session, "maskcov",
      choices = c("none", covariaterv$names),
      selected = "none")
  })
  ##############################################################################
  ## read mask file
  observe({
    req(input$maskfilename)
    req(!maskrv$clear)
    maskrv$data <- NULL
    if (input$masktype == 'File') {
      covariaterv$names <- character(0)
      if (!is.null(input$maskfilename)) {
        # temporary fix for shiny problem with multiple files to upload
        if (bookmarkrv$value) {
          maskrv$data <- read.mask(bookmarkrv$maskfilename, header = TRUE, 
            stringsAsFactors = TRUE) 
        }
        else {
          maskrv$data <- read.mask(input$maskfilename[1,4], header = TRUE, 
            stringsAsFactors = TRUE) 
        }
        covariaterv$names <- names(covariates(maskrv$data))
        updateSelectInput(session, "maskcov", 
          choices = c("none", covariaterv$names))
      }
    }
  })
  
  ##############################################################################
  
  capthist <- reactive( {
    # showNotification("debug: capthist eval", closeButton = FALSE, type = "message", duration = 0.1)
    ch <- NULL
    covnames <- character(0)
    if ((is.null(traprv$data) || is.null(captrv$data)) && is.null(importrv$data)) {
      updateNumericInput(session, "animal", max = 0)
      ch <- NULL
    }
    else {
      if (!is.null(importrv$data)) {
        ch <- importrv$data
      }
      else {
        ch <- try(suppressWarnings(make.capthist(captrv$data, traprv$data, 
          fmt = input$fmt)))
        if (filtercaptrv$value && !input$filtercapttext=="") {
          subsetcaptcall <- paste0("subset (ch,",input$filtercapttext, ")")
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
      roundpr <- function (pr) {pr[,-1] <- round(pr[,-1], input$dec); pr}
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
        lapply(der, round, input$dec)
      else 
        round(der, input$dec)
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
  
  mask <- reactive( {
    pxyrv$value <- NULL
    Drv$value <- NULL
    if (input$masktype=="Build") {
      
      if (is.null(traprv$data))
        return(NULL)
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
      sp <- unlist(spacing(msk)) # [input$sess]
      if (sp > rpsv) {
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
      secr.make.newdata(fitrv$value, all.levels = TRUE)
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
  # otherargs
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

  # detectfnbox, likelihoodbtn, distributionbtn, hcovbox, model, otherargs,
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
    removeNotification("lastaction")
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
  
  observeEvent(input$maskpolyfilename, {
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
    input$hcovbox, input$model, input$otherargs,
    input$masktype, input$buffer, input$habnx, input$maskshapebtn, 
    input$maskpolyfilename, input$maskfilename,
    input$filtercaptlink, input$filtercapttext), 
    ignoreInit = TRUE, {
      fitrv$value <- NULL
      updateRadioButtons(session, "resultsbtn", label = "", 
        inline = TRUE, choices = defaultresultsbtn)
      showNotification("model modified, yet to be fitted", id="lastaction", 
        closeButton = FALSE,type="message", duration = NULL)
    })
  ##############################################################################
  
  # Invalidate mask
  observeEvent(input$masktype, {
    reset("maskfilename")
    reset("maskpolygonsfilename")
    reset("maskcovariatefilename")
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
    if (input$animal>0) {
      if (ms(capthist())) {
        currentIDrv$value <- rownames(capthist()[[input$sess]])[input$animal]
      }
      else {
        currentIDrv$value <- rownames(capthist())[input$animal]
      }
    }
    else {
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
  
  observeEvent(c(input$clearimportlink, input$datasource), ignoreInit = TRUE, {
    
    reset('trapfilename')
    reset('captfilename')
    reset('captxlsname')
    disable("captfilename")  # waiting for trap file
    disable("captxlsname")  # waiting for trap xls
    reset('importfilename')
    
    updateSelectInput(session, "detectfnbox", choices = c('HN','HR','EX', hazarddetectfn), 
      selected = "HN")
    
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
  
  observeEvent(input$resultsbtn, ignoreInit = TRUE, {
    if (input$resultsbtn == "hide") 
      hide("resultsPrint")
    else
      show("resultsPrint")
  })
  
  observeEvent(input$masklink, ignoreInit = TRUE, {
    updateNavlistPanel(session, "navlist", "Habitat mask")
  })
  
  observeEvent(input$clearspatialdata, ignoreInit = TRUE, {
    updateSelectInput(session, "maskcov", choices = "none", select = "none")
    updateCheckboxInput(session, "legend", value = FALSE)
    reset("maskcovariatefilename")
    covariaterv$data <- NULL
    covariaterv$names <- character(0)
    covariaterv$clear <- TRUE
  }, priority = 1000)

  observeEvent(input$clearpolygondata, ignoreInit = TRUE, {
    reset("maskpolyfilename")
    polyrv$data <- NULL
    polyrv$clear <- TRUE
    updateRadioButtons(session, "includeexcludebtn", selected = "Include")
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
    ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
    traptextrv$value <- FALSE
    capttextrv$value <- !is.null(input$captfilename) && (input$showcaptfilelink %% 2 == 1)
  })
  
  observeEvent(input$filtercaptlink, ignoreInit = TRUE, {
    ## ignoreInit blocks initial execution when fitbtn goes from NULL to 0
    filtercaptrv$value <- (input$filtercaptlink %% 2) == 1
  })
  
  observeEvent(input$filtermask, ignoreInit = TRUE, {
    filtermaskrv$value <- (input$filtermask %% 2) == 1
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
  
  observeEvent(input$secrhelpbtn, ignoreInit = TRUE, {
    secrhelp <- file.path(path.package("secr"), "html",  "00Index.html")
    if (file.exists(secrhelp)) {
      browseURL(secrhelp)
    }
    else {
      # revert to pdf manual if html help not found
      browseURL("https://www.otago.ac.nz/density/pdfs/secr-manual.pdf")
    }
  })
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
  })
  ##############################################################################
  
  observeEvent(input$trapcovnames, ignoreInit = TRUE, {
    fitrv$value <- NULL
    traprv$value <- NULL
    captrv$value <- NULL
  })
  ##############################################################################
  
  observeEvent(input$trapotherargs, ignoreInit = TRUE, {
    fitrv$value <- NULL
    traprv$value <- NULL
    captrv$value <- NULL
  })
  
  ##############################################################################
  
  observeEvent(input$covnames, ignoreInit = TRUE, {
    fitrv$value <- NULL
    captrv$value <- NULL
  })
  
  observeEvent(input$captotherargs, ignoreInit = TRUE, {
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
    showNotification("Bookmarking is disabled in secrapp 1.3", id = "lastaction", 
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
    updateTextInput(session, "otherargs", 
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
    
    updateCheckboxInput(session, "dropmissing", value = FALSE)
    updateRadioButtons(session, "includeexcludebtn", selected = "Include")
    
    updateCheckboxInput(session, "dotsbox", value = FALSE)
    updateCheckboxInput(session, "frame", value = TRUE)
    updateCheckboxInput(session, "maskedge2", value = FALSE)
    updateCheckboxInput(session, "legend", value = FALSE)
    updateCheckboxInput(session, "showpoly", value = FALSE)
    updateSelectInput(session, "maskcov", choices = "none", selected = "none")
    updateTextInput(session, "filtermasktext", value = "", placeholder = "")
    
    ## Summary
    
    # safer to leave this for manual reset using Summary page buttons        
    sumrv$value <- sumrv$value[0,]
    
    updateCheckboxGroupInput(session, "analyses", 
      choices = character(0), selected = character(0)
    )
    updateCheckboxInput(session, "keepselectedbox", value = FALSE)
    
    ## Options
    
    updateNumericInput(session, "ncores", value = defaultcores)
    updateSelectInput(session, "method", selected = "Newton-Raphson")
    
    ## detector array
    updateRadioButtons(session, "areaunit", selected = "ha")
    
    updateNumericInput(session, "dec", value = 4)
    
    ## array plot
    updateCheckboxInput(session, "entireregionbox", value = TRUE)
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
    reset('maskpolyfilename')
    
    covariaterv$data <- NULL
    covariaterv$names <- character(0)
    covariaterv$clear <- TRUE
    reset('maskcovariatefilename')
    
    maskrv$data <- NULL
    maskrv$clear <- TRUE
    reset('maskfilename') 
    
    importrv$data <- NULL
    importrv$clear <- TRUE
    reset('importfilename')
    
    detectrv$value <- 'g0'
    
    showNotification("all inputs reset", id = "lastaction",
      closeButton = FALSE, type = "message", duration = seconds)
    
  }, priority = 1000)
  
  ##############################################################################
  
  observeEvent(input$selectallfieldslink, {
    updateCheckboxGroupInput(session, "fields1", selected = summaryfields[fieldgroup1])
    updateCheckboxGroupInput(session, "fields2", selected = summaryfields[fieldgroup2])
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
  
  observeEvent(input$suggestbufferlink, ignoreInit = TRUE, {
    ## ignoreInit blocks initial execution when suggestbufferlink goes from NULL to 0
    ch <- capthist()
    if (!is.null(ch)) {
      progress <- Progress$new(session, min = 1, max = 15)
      on.exit(progress$close())
      progress$set(message = 'Suggesting buffer width ...', detail = '')
      if (is.null(fitrv$value)) {
        ch <- if (ms(ch)) ch[[input$masksess]] else ch
        # 0.3 is guess?
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
  
  ## renderText
  
  # maskPrint
  # resultsPrint 
  # codePrint 
  # movesPrint
  
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

  output$maskdetailPrint <- renderPrint({
    if (input$masktype == 'Build')
      x1 <- paste0(input$buffer, "-m  buffer, nx = ", input$habnx)
    else
      x1 <- paste0("Habitat mask from file")
    
    cat(x1, "\n")
    
    if (is.null(mask())) { 
      if (input$masktype == 'File')
        x2 <- "[yet to load]"
      else
        x2 <- ""
    }
    else {
      if (ms(mask()))
        x2 <- paste0("Session 1 ", nrow(mask()[[1]]), " points, spacing ", signif(spacing(mask()[[1]]),3), " m")
      else
        x2 <- paste0(nrow(mask()), " points, spacing ", signif(spacing(mask()),3), " m")
    }
    cat(x2, "\n")
    
  })
  #-----------------------------------------------------------------------------
  
  output$animalIDPrint <- renderPrint({
    if(is.na(input$animal) || input$animal<=0 || is.null(nsessions()))
      cat("")
    else {
      if (nsessions()>1) {
        ch <- capthist()[[input$sess]]
      }    
      else {
        ch <- capthist()
      }
      ch <- subset(ch, input$animal, dropnullocc = TRUE)
      
      covar <- covariates(ch) 
      if (length(covar)>0)
        covar <- paste(lapply(1:length(covar), function(i) 
          paste(names(covar)[i], as.character(covar[[i]]))), collapse = ', ')
      ID <- paste0("ID ", currentIDrv$value)
      ncapt <- sum(ch)
      nDet <- paste0("\n", ncapt, " detection", if (ncapt>1) "s," else ",") 
      ORL <- paste0("ORL ", signif(ORL(ch)$ORL, 3), " m")
      cat(paste(ID, covar, nDet, ORL, sep = " "))
    }
  })
  #-----------------------------------------------------------------------------
  
  otherfn <- function (fncall, ch, fitted) {
    if (fncall=="") cat("No function specified\n")
    else {
      if (fitted) fncall <- gsub("fitted", "fitrv$value", fncall)
      if (ch) fncall <- gsub("ch", "capthist()", fncall)
      tr <- traprv$data
      out <- try(eval(parse(text = fncall)), silent = TRUE)
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
  
  output$resultsPrint <- renderPrint({
    hideplotif <- function (condition, tab) {
      if (condition)
        hideTab(inputId = "plottabs", target = tab)
      else 
        showTab(inputId = "plottabs", target = tab)
    }
    hideplotif (is.null(traprv$data), "Array")
    hideplotif (is.null(capthist()), "Moves")
    hideplotif (is.null(fitrv$value), "Detectfn")
    hideplotif (is.null(fitrv$value), "Buffer")
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
    
    minchar <- 50
    if (traptextrv$value) {
      header <- paste("Detector layout file :", input$trapfilename[1,'name'])
    }
    else if (capttextrv$value) {
      header <- paste("Capture file :", input$captfilename[1,'name'])
    }
    else if (is.null(traprv$data)) {
      header <- "No data loaded"
    }
    else if (input$resultsbtn == "other") {
      header <- input$otherfunction
    }
    else if (is.null(capthist())) {
      if (input$datasource == 'Text files') {
        header <- paste("Summary of detector layout from", input$trapfilename[1,'name'])
        if (nrow(input$trapfilename)>1) {
          header <- paste0(header, " etc.")
        }
      }
      else if (input$datasource == 'Excel files') {
        header <- paste("Summary of detector layout from", input$trapxlsname[1,'name'])
        if (nrow(input$trapxlsname)>1) {
          header <- paste0(header, " etc.")
        }
      }
    }
    else if (is.null(fitrv$value)) {
      if (input$datasource == 'Text files') {
        header <- paste0("Summary of capthist object from ", input$trapfilename[1,'name'], 
          if (nrow(input$trapfilename)>1) " etc." else "",
          " and ", 
          input$captfilename[1,'name'])
      }
      else if (input$datasource == 'Excel files') {
        header <- paste("Summary of capthist object from", input$trapxlsname[1,'name'])
        if (input$captxlsname[1,'name'] != input$trapxlsname[1,'name']) {
          header <- paste(header, "and", input$captxlsname[1,'name'])
        }
      }
      else {
        header <- paste("Summary of capthist object from", input$importfilename[1,'name'])
      }
      if (filtercaptrv$value) {
        header <- paste0(header, ", ", input$filtercapttext)
      }
    }
    else if (inherits(fitrv$value, "secr")) {
      if (input$resultsbtn=='summary') {
        minchar <- nchar(summary(fitrv$value)$versiontime)+6
        header <- paste("Summary of fitted model", input$model)
      }
      else if (input$resultsbtn=='predict') {
        minchar <- 48
        header <- paste("Parameter estimates")
      }
      else if (input$resultsbtn=='derived')
        header <- paste("Derived estimates of effective sampling area 'esa' and density 'D'")
      else header <- ""
    }
    if (nchar(header)>0) {
      cat(header, "\n")
      if (substring(header,1,2)!="No") cat(strrep('-', max(minchar,nchar(header))), "\n")
    }    
    if (traptextrv$value) {
      rawText <- readLines(input$trapfilename[1,"datapath"]) # get raw text
      writeLines(rawText)
    }
    else if (capttextrv$value) {
      rawText <- readLines(input$captfilename[1,"datapath"]) # get raw text
      writeLines(rawText)
    }
    else if (is.null(traprv$data)) {
      cat("")
    }
    else if (is.null(capthist())) {
      if (input$resultsbtn == "summary")
        summary(traprv$data, covariates = TRUE)   ## arg covariates new in secr 4.3.1
      else
        otherfn(input$otherfunction, FALSE, FALSE)
    }
    else if (is.null(fitrv$value)) {
      if (input$resultsbtn == "summary") {
        if (ms(capthist())) {
        list(terse = summary(capthist(), terse = TRUE), bysession = summary(capthist(), moves = TRUE))            }
      else {
        summary(capthist(), moves = TRUE)
      }
      }
      else {
        otherfn(input$otherfunction, TRUE, FALSE)
      }
    }
    else if (inherits(fitrv$value, "secr")) {

      if (input$resultsbtn == "summary")
        summary(fitrv$value)
      else if (input$resultsbtn == "predict") {
        predictresult()
      }
      else if (input$resultsbtn == "derived") {
        derivedresult()
      }
      else {
        otherfn(input$otherfunction, TRUE, TRUE)
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
    if (!is.null(captrv$data) || !is.null(importrv$data)) {
      cat(captcode())
    }
    
    if (!is.null(capthist())) {
      cat("summary(ch)\n")
      cat("\n# fit model\n")
      cat(maskcode())
      cat(fitcode())
      cat("summary(fitted)\n")
    }
  })
  ##############################################################################
  
  output$movesPrint <- renderPrint({
    m <- movements()
    if (!is.null(m)) {
      maxm <- max(unlist(m))
      maxbyanimal <- suppressWarnings(sapply(m, max))
      nmax <- match(maxm, maxbyanimal)
      IDmax <- names(m)[nmax] 
      rpsv <- unlist(RPSV(capthist(), CC = TRUE))[input$sess]
      cat("Number of moves", length(unlist(m)), "\n")
      cat("Median move", round(median(unlist(m)),1), "m\n")
      cat("Naive HN sigma", round(rpsv,1), "m\n")
      cat("Longest move", round(maxm,1), "m\n")
      cat("by animal ID", IDmax, "\n")
    }
  })
  ##############################################################################
  
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
    
    removeNotification("arrayploterror")
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
          showNotification("fxi.contour requires point detector type")
        else {
          if (input$animal>0) {
            tmp <- try(fxi.contour(fitrv$value, i = input$animal, 
              sessnum = input$sess, border = input$buffer, add = TRUE), 
              silent = TRUE)
          }
          else {
            tmp <- try(fxi.contour(fitrv$value, i = NULL, 
              sessnum = input$sess,  border = input$buffer, add = TRUE), 
              silent = TRUE)
          }
          if (inherits(tmp, 'try-error')) {
            showNotification("error in fxi.contour; consider smaller mask spacing", 
              id="arrayploterror", type="error")
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
    if (detector(traprv$data)[1] %in% polygondetectors) {
      if (compareVersion(as.character(secrversion), '4.2.0') < 0) {
        showNotification("esa.plot for polygon detector types requires secr >= 4.2.0")
        return()
      }
    }
    if (input$masktype == "Build") {
      spscale <- secr:::spatialscale(fitrv$value, detectfn = input$detectfnbox, 
        session = input$sess)
      max.buffer <- max(input$buffer*1.5, 5*spscale)  # 2020-08-13 *1.5
      esa.plot(fitrv$value, session = input$sess, max.buffer = max.buffer, thin = 1)
      abline(v = input$buffer, col = "red")
    }
    else {
      esa.plot(fitrv$value, session = input$sess)
    }
    mtext("Density estimate", side = 4, line = 1.5)
  })
  ##############################################################################
  
  output$maskPlot <- renderPlot({
    
    plotmsk <- function (add) {
      if (is.null(input$maskcov) | (input$maskcov == "none")) {
        plot (msk, add = add, col = grey(0.94 - input$dotsbox/5), dots = input$dotsbox,
          covariate = NULL)
      }
      else {
        # debug cat(input$maskcov, "\n")
        plot (msk, add = add, dots = input$dotsbox, covariate = input$maskcov,
          legend = input$legend, inset = 0)
        nacovariate <- is.na(covariates(msk)[,input$maskcov])
        if (any(nacovariate)) {
          plot (subset(msk, nacovariate), add = TRUE, dots = input$dotsbox, 
            col = 'red')
        }
      }
    }
    core <- traprv$data
    msk <- mask()
    if (ms(core)) {
      core <- core[[input$masksess]]
      msk <- msk[[input$masksess]]
    }
    
    par(mar=c(2,1,2,5), xaxs='i', yaxs='i', xpd = !input$frame)
    if (input$masktype == "Build") {
      if (is.null(core)) return (NULL)
      plot (core, border = input$buffer, gridlines = FALSE)
      plotmsk(add = TRUE)
      plot (core, add = TRUE)
      if (!is.null(polyrv$data) && input$showpoly) {
        sp::plot(polyrv$data, add = TRUE)
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
    pdot.contour(core, border = border, nx = input$pxynx,
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
      col <- try(eval(parse(text = input$Dxycol)), silent = TRUE)
      plot (fitrv$dsurf, border = 0, scale = 1, title="D(x)", col = col)
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
  
  ## rendertable
  
  ##############################################################################
  
  output$summarytable <- renderTable({
    fields <- c(input$fields1, input$fields2)
    analyses <- input$analyses
    tmp <- sumrv$value[analyses,fields]
    if (length(analyses)>1) {
      tmp$dAIC <- tmp$AIC - min(tmp$AIC, na.rm = TRUE)
    }
    tmp <- t(tmp)
    # if (ncol(tmp)>0) colnames(tmp) <- paste0('Analysis', 1:ncol(tmp))
    tmp <- cbind(Field = fields, tmp)
    tmp } , spacing = "xs"
  )
  
  ##############################################################################
  
  ## downloadhandler
  
  ##############################################################################
  
  output$downloadSummary <- downloadHandler(
    filename = "summary.csv",
    content = function(file) {
      if (input$keepselectedbox) 
        df <- sumrv$value[input$analyses,]
      else 
        df <- sumrv$value
      write.csv(df, file, row.names = TRUE)
    }
  )
  
  output$downloadSummaryrds <- downloadHandler(
    filename = "summary.rds",
    content = function(file) {
      if (input$keepselectedbox) 
        df <- sumrv$value[input$analyses,]
      else 
        df <- sumrv$value
      saveRDS(df, file)
    }
  )
  
  output$savebtn <- downloadHandler(
    filename = "fit.rds",
    content = function(file) {
      saveRDS(fitrv$value, file)
    }
  )
  
  output$exportbtn <- downloadHandler(
    filename = "ch.rds",
    content = function(file) {
      saveRDS(capthist(), file)
    }
  )
  
  output$savemask <- downloadHandler(
    filename = "mask.txt",
    content = function(file) {
      if (ms(mask()))
        msk <- mask()[[input$masksess]]
      else 
        msk <- mask()
      write.mask(msk, file = file)
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
  
  setBookmarkExclude(c(
    "filtercaptlink",
    "fitbtn", 
    "selectallanalyseslink", 
    "selectnoanalyseslink", 
    "selectnofieldslink", 
    "selectallfieldslink",
    "resetbtn", 
    "selectfieldsbtn", 
    "selectanalysesbtn", 
    "selectingfields",          # rv
    "selectinganalyses"         # rv
    ))
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    maskpolyshp <- sapply(input$maskpolyfilename, shpfile)
    maskcovshp <- sapply(input$maskcovariatefilename, shpfile)
    shp <- any(maskpolyshp) | any(maskcovshp)
    if (shp) {
      showNotification("ESRI shapefile(s) will not be bookmarked", type = "warning",
        id = "noshapefile")
    }
    state$values$shp <- shp
    state$values$sumrv <- sumrv$value            # works
    state$values$fit <- fitrv$value
    state$values$filtercaptrv <- filtercaptrv$value
    state$values$port <- session$clientData$url_port
    
    if (!is.null(input$trapfilename))
      state$values$trapfilename <- tools::file_path_as_absolute(input$trapfilename$name[1])
    if (!is.null(input$trapxlsname))
      state$values$trapxlsname <- tools::file_path_as_absolute(input$trapxlsname$name[1])
    
    if (!is.null(input$captfilename))
      state$values$captfilename <- tools::file_path_as_absolute(input$captfilename$name[1])
    if (!is.null(input$captxlsname))
      state$values$captxlsname <- tools::file_path_as_absolute(input$captxlsname$name[1])
    
    if (!is.null(input$maskfilename))
      state$values$maskfilename <- tools::file_path_as_absolute(input$maskfilename$name[1])
    
    ## can manually recover with e.g.
    ## readRDS('d:/density secr 3.2/secrapp/shiny_bookmarks/9c88715bacc260cf/values.rds')$port
  })    
  # Read values from state$values when we restore
  onRestore(function(state) {
    bookmarkrv$value <- TRUE
    bookmarkrv$trapfilename <- state$values$trapfilename
    bookmarkrv$trapxlsname <- state$values$trapxlsname
    bookmarkrv$captfilename <- state$values$captfilename
    bookmarkrv$captxlsname <- state$values$captxlsname
    bookmarkrv$maskfilename <- state$values$maskfilename
    sumrv$value <- state$values$sumrv
    fitrv$value <- state$values$fit
    current$unit <- input$areaunit
    if (any(state$values$shp)) {
      showNotification("Cannot restore ESRI shapefile(s); re-select", type = "error", id = "noshapefile2")
    }
  })
  # Read values from state$values when we restore
  onRestored(function(state) {
    bookmarkrv$value <- FALSE
  })
}

##################################################################################
# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "server")
##################################################################################
