tababout <- tabPanel(
  "About",
  h2("secr app 2.2"), br(),
  
  h5(paste("This Shiny application provides an interface to the R package 'secr', version", 
           publishedversion, ".")),
  br(),
  h5("Copyright 2019, 2022, 2024, 2025 Murray Efford"),
  "The application is released under the ",
  a("GNU General Public License Version 3.0", href="https://www.gnu.org/licenses/gpl-3.0.txt", target="_blank"), br(),
  br(),
  h5("For further information see "), 
  a("The SECR Book", href="https://murrayefford.github.io/SECRbook/", target="_blank"), br(),
  a("www.otago.ac.nz/density", href="https://www.otago.ac.nz/density", target="_blank"), br(),
  a("CRAN.R-project.org/package=secr", href="https://CRAN.R-project.org/package=secr", target="_blank"), br(),
  a("https://github.com/MurrayEfford/secrapp", href="https://github.com/MurrayEfford/secrapp", target="_blank"), br(),
  br(),
  h5("Citation"),
  h5("Analyses with this interface should refer to the parent package, secr. The citation for the current version is -"), 
  h5(paste0("Efford, M. G. (", publishedyear, "). secr: Spatially explicit capture-recapture models. R package version ", publishedversion, ".")), 
  h5("    https://CRAN.R-project.org/package=secr.")
)