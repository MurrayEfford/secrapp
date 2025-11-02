tabintro <- tabPanel(
  "Introduction",
  fluidRow(
    column(8,
           h2("What is secrapp?"),
           
           HTML("Spatially explicit capture&ndash;recapture (SECR or SCR) (Efford 2004, Borchers and Efford 2008, 
                  Efford et al. 2009, Royle et al. 2014) is used to estimate animal population density 
                  from data on marked animals at arrays of passive detectors 
                  (traps, cameras, hair snags etc.)."),
           br(),br(),
           
           "SECR is computer-intensive, with both Bayesian and frequentist (maximum likelihood) flavours. 
                                  Here we focus on data from closed populations (no births, deaths, immigration or emigration 
                                  during sampling) and model fitting by maximum likelihood.",
           br(), br(),
           
           strong("secrapp"), "is an interactive interface to parts of the R package", strong("secr"), 
           paste0("(Efford ", secryear,"). "), strong("secr"), " and ", strong("secrapp"), "together supercede the Windows", 
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
           a(href="https://murrayefford.github.io/SECRbook/", target="_blank", "The SECR Book"), " should be useful.",
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
           
           paste0("Efford, M. G. (", publishedyear, ")"),
           a(HTML("secr: Spatially explicit capture&ndash;recapture models."), href = "https://CRAN.R-project.org/package=secr"),
           paste0(" R package version ", publishedversion, "."), br(),
           
           "Efford, M. G., Borchers D. L. and Byrom, A. E. (2009)", 
           a(HTML("Density estimation by spatially explicit capture&ndash;recapture: likelihood-based methods."),
             href = "https://www.otago.ac.nz/density/pdfs/Efford Borchers & Byrom 2009.pdf"), 
           "In: D. L. Thomson, E. G. Cooch, M. J. Conroy (eds)",
           em("Modeling Demographic Processes in Marked Populations."), "Springer. Pp 255-269.", br(),
           
           "Efford, M. G., Dawson, D. K. and Robbins, C. R. (2004)",
           a(HTML("DENSITY: software for analysing capture&ndash;recapture data from passive detector arrays."), 
             href="https://www.otago.ac.nz/density/pdfs/Efford Dawson & Robbins 2004 ABC.pdf"),
           em("Animal Biodiversity and Conservation"), "27.1: 217-228.", br(),
           
           "Royle, J. A., Chandler, R. B., Sollmann, R. and Gardner, B. (2014)",
           em(HTML("Spatial capture&ndash;recapture.")), "Academic Press.",
           
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
)