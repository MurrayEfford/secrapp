tabmain <- tabPanel(
  "Main screen",
  fluidRow(
    column (5, # offset = 0, style='padding:15px;',
            h2("Data input"),
            fluidRow(
              column(12, radioButtons("datasource", "", inline = TRUE, 
                                      # choices = c("Text files", "Excel files", "Stored capthist object"),
                                      choices = c("Text files", "Excel files", "Stored capthist object", "secr dataset"),
                                      selected = "Text files"))
            ),
            
            # conditionalPanel( condition = "input.datasource != 'Stored capthist object'",
            #conditionalPanel( condition = "input.datasource %in% c('Text files', 'Excel files')",
            conditionalPanel( condition = "input.datasource == 'Text files' | input.datasource == 'Excel files'",
                              
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
                                                                            actionLink("showcaptfilelink", HTML("<small>show file</small>"))
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
            
            conditionalPanel( condition = "input.datasource == 'secr dataset'",
                              wellPanel(class = "mypanel",
                                        fluidRow(
                                          column(4, 
                                                 div(style = "margin-top:-15px"),
                                                 selectInput("secrdatabox", "", 
                                                             choices = c(
                                                               "blackbearCH",
                                                               "captdata", 
                                                               "deermouse.ESG", 
                                                               "deermouse.WSG",
                                                               "hornedlizardCH",
                                                               "housemouse",
                                                               "infraCH",
                                                               "lineoCH",
                                                               "ovenCHp", 
                                                               "OVpossumCH",
                                                               "possumCH",
                                                               "stoatCH"),
                                                             selected = "blackbearCH",
                                                             selectize = FALSE,
                                                             size = 11)),
                                          column(8, 
                                                 uiOutput("secrdatadescriptionui"),
                                                 fluidRow(
                                                   column(8),
                                                   column(4, actionButton('secrhelptopicbtn', 'More'))))
                                        )
                              )
            ),  # end conditionalPanel secr dataset
            
            h2("Model"),
            wellPanel(class = "mypanel", 
                      fluidRow(
                        column(3, selectInput("detectfnbox", "Detection function",
                                              choices = c("HN", "HR", "EX","HHN", "HHR", "HEX", "HVP"),
                                              selected = "HN"),
                               div(style = "margin-top:-15px"),  # negative spacer
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
                        div(style = "margin-top:5px"),  # spacer
                        
                        column(6, textInput("model", "Model", value = "D~1, g0~1, sigma~1")),
                        column(6, HTML("<small><strong>Habitat mask</strong></small>","&nbsp;"),
                               HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                               actionLink("masklink", HTML("<small> edit</small>")), 
                               HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                               actionLink("suggestbufferlink2", HTML("<small>suggest</small>"),
                                          title = "Based on RPSV(ch, CC = TRUE); RB <= 0.1%"),
                               br(),
                               div(style = "margin-top:5px"),  # spacer
                               verbatimTextOutput("maskdetailPrint")
                        )
                      ),
                      fluidRow(
                        column(12,  style="color:grey;",
                               div(style = "margin-top:-15px"),  # negative spacer
                               
                               textInput("modelotherargs", "Other arguments", value = "", 
                                         placeholder = "e.g., details = list(fastproximity = FALSE), binomN = 1"))
                      )
            ),   # end wellPanel Model
            
            
            h2("Actions"),
            fluidRow(
              column(3, actionButton("fitbtn", "Fit model",  width = 130,
                                     title = "Fit spatially explicit capture-recapture model to estimate density and update Results")),
              column(3, actionButton("secrhelpbtn", "secr help",  width = 130,
                                     title = "Open secr manual in a browser window",
                                     onclick = "window.open('https://www.otago.ac.nz/density/pdfs/secr-manual.pdf',
                '_blank')")),
              column(3, uiOutput("secrdesignurl")),  ## switch to secrdesign, with parameters
              column(3, actionLink("optionslink", "Go to Options"))
            ),
            
            br(),
            fluidRow(
              column(3, actionButton("resetbtn", "Reset all", width = 130, 
                                     title = "Reset all inputs to initial values")),
              # column(3, bookmarkButton(width = 130)), # NOT WORKING 1.3
              column(3, actionButton("dummybookmarkbutton", "Bookmark", width = 130, title = "Bookmarking is disabled in secrapp 2.0")),
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
              column(12,
                     tags$head(tags$style("#resultsPrint{overflow-y:scroll; height: 350px;}")),
                     verbatimTextOutput("resultsPrint"))
            ),
            actionButton("hideresultsbtn", HTML("<small>hide</small>"), class = "btn-link"),
            
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
                                            column(9, style = 'padding:0px;', 
                                                   plotOutput("arrayPlot", 
                                                              click = clickOpts(id = "arrayClick", clip = FALSE),
                                                              hover = hoverOpts(id ="arrayHover", delayType = "throttle"))),
                                            column(3, br(),
                                                   conditionalPanel("output.multisession=='true'",
                                                                    numericInput("sess", "Session", min = 1, max = 2000, 
                                                                                 step = 1, value = 1)),
                                                   br(),
                                                   
                                                   conditionalPanel("output.capthistLoaded",
                                                                    checkboxInput("tracks", "All tracks", FALSE), br(),
                                                                    numericInput("animal", "Select animal", min = 0, max = 2000, 
                                                                                 step = 1, value = 1), 
                                                                    conditionalPanel("input.animal>0", verbatimTextOutput("animalIDPrint")),
                                                                    conditionalPanel("output.modelFitted", checkboxInput("fxi", "fxi contour", FALSE))
                                                   ),
                                                   conditionalPanel("output.usage", 
                                                                    fluidRow(
                                                                      column(2, offset = 1, checkboxInput("usageplot", "Usage", FALSE))
                                                                    )
                                                   ),
                                                   conditionalPanel("input.gridlines != 'None'",
                                                                    uiOutput("uigridlines") ),
                                                   br(), 
                                                   uiOutput('xycoord')
                                            )
                                          )
                                          # ,
                                          # conditionalPanel("output.capthistLoaded",
                                          #   fluidRow(
                                          #     column(2, offset = 1, checkboxInput("tracks", "All tracks", FALSE)),
                                          #     column(2, numericInput("animal", "Select animal", min = 0, max = 2000, 
                                          #       step = 1, value = 1)),
                                          #     column(4, conditionalPanel("input.animal>0", 
                                          #       verbatimTextOutput("animalIDPrint")
                                          #     )),
                                          #     column(3, conditionalPanel("output.modelFitted", checkboxInput("fxi", "fxi contour", FALSE)))
                                          #   )),
                                          # conditionalPanel("output.usage", 
                                          #   fluidRow(
                                          #     column(2, offset = 1, checkboxInput("usageplot", "Usage", FALSE))
                                          #     )
                                          # )
                                          
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
                     ),
                     actionButton("hidegraphicsbtn", HTML("<small>hide</small>"), class = "btn-link")
                     
              )
            )
    )
  )
)

