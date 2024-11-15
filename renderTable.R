## rendertable

##############################################################################

output$summarytable <- renderTable({
  fields <- c(input$fields1, input$fields2)
  analyses <- input$analyses
  tmp <- sumrv$value[analyses,fields, drop = FALSE]
  if (nrow(tmp)>0 && ncol(tmp)>0 && "dAIC" %in% fields) {
    if (length(analyses)>1) {
      tmp$dAIC <- tmp$AIC - min(tmp$AIC, na.rm = TRUE)
    }
    else {
      tmp$dAIC <- NA
    }
  }
  tmp <- t(tmp)
  # if (ncol(tmp)>0) colnames(tmp) <- paste0('Analysis', 1:ncol(tmp))
  tmp <- cbind(Field = fields, tmp)
  tmp } , spacing = "xs"
)

##############################################################################
