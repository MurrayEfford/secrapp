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

secrrv <- reactiveValues(
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
