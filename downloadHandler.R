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
