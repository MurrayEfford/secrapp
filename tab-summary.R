tabsummary <- tabPanel(
  "Summary",
  br(),
  fluidRow(
    column(2, 
           div(style = "margin-top:-20px"),
           wellPanel(
             h2("Fields"),
             fluidRow(
               column(6, actionButton("selectfieldsbtn", "Select", title = "Choose fields to display")), 
               column(6, 
                      actionLink("selectnofieldslink", "None", title = "Unselect all fields"), br(), 
                      actionLink("selectdefaultfieldslink", "Default", title = "Select default fields"), br(), 
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
             conditionalPanel("output.summaries == 'true'",
                              br(),
                              fluidRow(
                                column(6, ""),
                                column(4, actionLink("clearallanalyseslink", "Clear", title = "Clear all analyses"))
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
                                                        choices = summaryfields[fieldgroup1],
                                                        selected = defaultfields1
                                     )
                    )
             ),
             column(6,
                    conditionalPanel("output.selectingfields == 'TRUE'",
                                     checkboxGroupInput("fields2", "",
                                                        choices = summaryfields[fieldgroup2],
                                                        selected = defaultfields2
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
)