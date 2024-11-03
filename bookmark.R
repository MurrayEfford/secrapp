# 2024-11-03 not live 

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


