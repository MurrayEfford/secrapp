taboptions <- tabPanel("Options",
         
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
                                               column(8, downloadLink("exportbtn", "Export current capthist to RDS file", 
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
                                                     selected = "Newton-Raphson", width=160))
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
                              column(8, uiOutput("timelimitui") ),
                              column(4, actionLink("incrementtime", label = "."))
                            ),
                            fluidRow(
                              column(11, selectInput("refitmethod", "Re-fit method",
                                                     choices = c("Newton-Raphson", "Nelder-Mead", "none"),
                                                     selected = "Newton-Raphson", width=160))
                            )
                  ),
                  h2("Summary"),
                  wellPanel(class = "mypanel",
                            fluidRow(
                              column(12, numericInput("dec", "Significant figures", 
                                                      min = 0, 
                                                      max = 8, 
                                                      value = 4, 
                                                      width=160))
                            ),
                            fluidRow(
                              column(12, textInput("summaryprefix", "Prefix",value = "Analysis"))
                            ),
                            fluidRow(
                              column(12, textInput("title", "Note", value = "", 
                                                   placeholder = "note for summary"))
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
                  h2("Habitat mask plot"),
                  wellPanel(class = "mypanel", 
                            fluidRow(
                              column(6, numericInput("maskborder", "Border (buffer units)", 
                                                     min = 0, 
                                                     max = 1000, 
                                                     step = 0.5, 
                                                     value = 1, 
                                                     width = 180))
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
)
