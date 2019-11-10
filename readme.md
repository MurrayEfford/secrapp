# secrapp 1.1

secrapp performs simple spatially explicit capture-recapture analyses using the R package [secr](https://CRAN.R-project.org/package=secr). See the Help tab for details.

There is very limited support for multisession data and elaborate models: these are best avoided.

To run secrapp on your own machine, install packages **shiny**, **shinyjs**, **secr** and **stringr**. Then paste this code into an R session:

```r
library(shiny)
runGitHub("secrapp", "MurrayEfford")
```

Version 1.1 requires secr >= 4.0.1.

----

Some test data are provided:

| File name | Description | Usage |
|--------|-------------------------------|------------------|
trap.txt | secr test file ||
capt.txt | secr test file ||
ovtrap.txt | Locations of possum traps in the Orongorongo Valley | Design - File input of detector array |
OVforest.dbf | ESRI polygon shapefile extent of habitat near possum traps | Design - Region and Options - Habitat clip to polygons |
OVforest.shp |||
OVforest.shx |||
