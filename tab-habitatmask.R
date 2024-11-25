tabhabitat <- tabPanel("Habitat mask",
                       fluidRow(
                         column(3,
                                tabsetPanel(
                                  type = "pills", id = "masktype", selected = "Build",
                                  # build mask by buffering around traps
                                  tabPanel("Build",
                                           wellPanel(class = "mypanel", 
                                                     fluidRow(
                                                       column(6, 
                                                              numericInput("buffer", "Buffer width (m)",
                                                                           min = 0,
                                                                           max = 100000,
                                                                           value = 100,
                                                                           step = 5,
                                                                           width = 160)
                                                       ),
                                                       column(6,
                                                              br(),
                                                              tags$div(style="text-align: right",
                                                                actionLink("suggestbufferlink", HTML("<small>suggest width</small>"),
                                                                         title = "Based on RPSV(ch, CC = TRUE); RB <= 0.1%")
                                                              )
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              radioButtons("meshdimensionbtn", label = "Specify mesh by -",
                                                                    choices = c("Number in x-direction  ", "Spacing"), 
                                                                    selected = "Number in x-direction  ", inline = TRUE))
                                                       ),
                                                       fluidRow(
                                                         column(6, 
                                                                tags$div(title = "Resolution of mesh - number of columns in x-direction",
                                                                  numericInput("habnx", "nx",
                                                                               min = 10,
                                                                               max = 1000,
                                                                               value = 32,
                                                                               step = 1,
                                                                               width = 160)
                                                                )
                                                         ),
                                                         column(6, 
                                                                numericInput("habspacing", "spacing m",
                                                                             min = 1,
                                                                             max = 10000,
                                                                             value = 10,
                                                                             step = 1,
                                                                             width = 160)
                                                         )
                                                       
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
                                                              tags$div(style = "text-align: right",
                                                                       actionLink("clearbufferspec", HTML("<small>reset</small>"))
                                                              )
                                                       )
                                                       
                                                     )
                                           ),
                                           wellPanel(class = "mypanel", 
                                                     tags$div(title = "Polygons can be used to clip the mask",
                                                              radioButtons("maskpolybtn", label = "Mask polygon source",
                                                                           choices = c("None", "File(s)", "R object"), 
                                                                           selected = "None", inline = TRUE)
                                                     ),
                                                     conditionalPanel( condition = "input.maskpolybtn == 'File(s)'",
                                                                       div(style="height: 80px;",
                                                                           fileInput("maskpolyfilename", 
                                                                                     paste0("Polygon file(s)", strrep(intToUtf8(160), 3)),
                                                                                     accept = c('.shp','.dbf','.sbn','.sbx',
                                                                                                '.shx',".prj", ".txt", ".rdata", ".rda", ".rds"), 
                                                                                     multiple = TRUE)
                                                                       ),
                                                                       uiOutput("habitatfile")
                                                     ),
                                                     
                                                     conditionalPanel( condition = "input.maskpolybtn == 'R object'",
                                                                       div(style="height: 80px;",
                                                                           # need also to load polyrv$data
                                                                           textInput("maskpolyobjectname", "Polygon object",
                                                                                     value = "", placeholder = "e.g., GSM")
                                                                       )),
                                                     
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
                                                                              tags$div(style="text-align: right",
                                                                                      actionLink("clearpolygondata", HTML("<small>clear</small>"))
                                                                              )
                                                                       )
                                                                       )
                                                     )
                                           ),
                                           wellPanel(class = "mypanel",
                                                     tags$div(title = "Optional mask covariates",
                                                              radioButtons("maskcovariatebtn", label = "Mask covariate source",
                                                                           choices = c("None", "File(s)"), 
                                                                           selected = "None", inline = TRUE)
                                                     ),
                                                     conditionalPanel( condition = "input.maskcovariatebtn == 'File(s)'",
                                                                       div(style="height: 80px;",
                                                                           fileInput("maskcovariatefilename",
                                                                                     paste0("Spatial data source for covariates"),
                                                                                     accept = c('.shp', '.dbf', '.sbn', '.sbx',
                                                                                                '.shx', '.prj', '.txt', '.rds'),
                                                                                     multiple = TRUE)),
                                                                       uiOutput("covariatefile"),
                                                                       conditionalPanel ("output.maskcovariatefileready", 
                                                                                         fluidRow(
                                                                                           column(10, 
                                                                                                  checkboxInput("dropmissing", 
                                                                                                                "Drop point if any covariate missing", value = FALSE)
                                                                                           ),
                                                                                           column(2,
                                                                                                  tags$div(style="text-align: right",
                                                                                                           actionLink("filtermask", HTML("<small>filter</small>")),
                                                                                                           br(),
                                                                                                           actionLink("clearspatialdata", HTML("<small>clear</small>"))
                                                                                                  )
                                                                                           )
                                                                                         ),
                                                                                         conditionalPanel("output.filterMask",
                                                                                                          fluidRow(
                                                                                                            column(12, textInput("filtermasktext", "Filter",
                                                                                                                                 placeholder = "e.g., forest == 'beech'"))
                                                                                                          )))
                                                     )
                                           )
                                  ),
                                  # mask input from file
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
                                                              downloadLink("savemask", "Save mask to text file")))
                                )
                         ),
                         column(5, plotOutput("maskPlot"),
                                conditionalPanel ("output.maskready", 
                                                  fluidRow(
                                                    column(3, offset = 1, 
                                                           checkboxInput("dotsbox", "dots", value = FALSE)),
                                                    
                                                    column(2, checkboxInput("frame", "frame", value = TRUE)),
                                                    column(3, 
                                                           checkboxInput("maskedge2", "show edge", value = FALSE),
                                                           conditionalPanel ("output.maskcovariatesready", 
                                                                             checkboxInput("legend", "legend", value = FALSE)
                                                           )
                                                    ),
                                                    column(2, 
                                                           conditionalPanel ("output.maskpolygonsready", 
                                                                             checkboxInput("showpoly", "polygon(s)", value = FALSE)
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
)