##############################################################################
## global variables

linewidth <- 2      # for various plots 

# durations for showNotification
seconds        <- 6   # default
warningseconds <- 12  
errorseconds   <- 12   
invalidseconds <- NULL

timewarning <- 0.5  # minutes
timelimit <- 10.0   # minutes

availablecores <- min(24, parallel::detectCores())
defaultcores <- max(1, availablecores/2)

defaultresultsbtn <- c("summary", "other")
fittedresultsbtn <- c("summary", "predict", "derived", "other")

secrversion <- packageVersion('secr')
secryear <- substring(packageDate('secr'),1,4)

publishedversion <- "5.3.0"
publishedyear <- 2025

polygondetectors <- c("polygon", "polygonX", "transect", "transectX")
hazarddetectfn <- c("HHN", "HHR", "HEX", "HVP")

# for transfer to secrdesign
# secrdesignapp 1.2 and above reads parameters
designurl <- "https://www.stats.otago.ac.nz/secrdesignapp/"  

## summary field names
summaryfields <- c("date", "time", "note", "traps", "captures", "filter", 
                   "n", "r", "ndetectors", "noccasions", "usagepct", "maskbuffer", "masknrow", 
                   "maskspace", "likelihood", "distribution", "model", "hcov", 
                   "detectfn", "npar", "logLik", "AIC", "dAIC",
                   "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", 
                   "se.sigma", "z", "se.z", "k", "proctime", "method")
defaultfields1 <- c("date", "time", "note", "traps", "captures", "filter",
                    "n", "r", "ndetectors", "noccasions",
                    "usagepct", "maskbuffer", "masknrow", "maskspace",
                    "likelihood", "distribution", "model")
defaultfields2 <- c("detectfn",
                    "npar", "logLik", "AIC", "dAIC",
                    "D", "se.D", "RSE.D", "g0", "se.g0", "lambda0", "se.lambda0", "sigma", "se.sigma",
                    "k", "proctime")
fieldgroup1 <- 1:18
fieldgroup2 <- 19:37


##############################################################################

