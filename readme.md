# secrapp 2.0

This Shiny app performs simple spatially explicit capture-recapture analyses using the 
R package [secr](https://CRAN.R-project.org/package=secr). For details see the 
Help tab, the 
[step-by-step tutorial](https://www.otago.ac.nz/density/pdfs/secrapp-tutorial.html), 
and the [Density web page](https://www.otago.ac.nz/density/) .

To run secrapp directly from GitHub on your own machine, paste this code into an R session.

```r
install.packages("shinyjs")  # skip if already installed; installs 'shiny' if needed
install.packages("secr")     # skip if already installed
shiny::runGitHub("secrapp", "MurrayEfford")
```

Note:

- for some functions you will also need the packages 'sf', 'stringr', and 'readxl'  
- secrapp 2.0 requires secr 5.1.0 or a later version.

A version of the app may be run in a web browser without any setup by 
following this [link](https://www.stats.otago.ac.nz/secrapp). 
This arrangement has limited capacity for simultaneous users.

----

Some test data are provided here; see the **secr** help pages for details. 
Further example datasets may be found on the 
[Density website](https://www.otago.ac.nz/density/examples/). 

| File name | Description | Usage |
|--------|-------------------------------|------------------|
trap.txt | secr test file |Data input - Detector layout |
capt.txt | secr test file |Data input - Captures |
mask.txt | Demo mask file for capt.txt | Habitat mask - File input |
FTHLtrap.txt | Perimeter of horned lizard search area | Data input - Detector layout |
FTHLcapt.txt | Flat-tailed horned lizard test data |Data input - Captures |
FTHL.rds | Saved flat-tailed horned lizard dataset | Data input |
GSMboundary.dbf | ESRI polygon shapefile extent of habitat near black bear hair snags | Habitat mask - Mask polygon files |
GSMboundary.shp |||
GSMboundary.shx |||
hareCH6trap.txt | Snowshow hare data | Data input - Detector layout |
hareCH6capt.txt | Snowshow hare data | Data input - Captures |
hareCH6.xlsx | Snowshoe hare data as Excel spreadsheet | Data input |
hareCH6.rds | Saved snowshoe hare dataset | Data input |
ovtrap.txt | Locations of possum traps in the Orongorongo Valley | Data input - Detector layout |
ovcapt.txt | Brushtail possum captures in the Orongorongo Valley | Data input - Captures |
OVforestL.dbf | ESRI polygon shapefile extent of habitat near possum traps | Habitat mask - Mask polygon files |
OVforestL.shp |||
OVforestL.shx |||
ovenCHp.xlsx | Ovenbird proximity data as Excel spreadsheet | Data input |
ovenCHp.rds | Saved ovenbird proximity dataset | Data input |
