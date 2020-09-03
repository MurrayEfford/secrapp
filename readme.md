# secrapp 1.3

secrapp performs simple spatially explicit capture-recapture analyses using the 
R package [secr](https://CRAN.R-project.org/package=secr). See the Help tab for 
details.

There is limited support for multisession data and elaborate models: these are 
best avoided.

To run secrapp on your own machine, install packages **shinyjs** and **secr** 
with their dependencies. Then paste this code into an R session:

```r
library(shiny)
runGitHub("secrapp", "MurrayEfford")
```

Version 1.3 requires secr >= 4.3.1 (on CRAN early September 2020, otherwise see https://www.otago.ac.nz/density).

----

Some test data are provided:

| File name | Description | Usage |
|--------|-------------------------------|------------------|
trap.txt | secr test file |Data input - Detector layout |
capt.txt | secr test file |Data input - Captures |
masktext2.txt | Demo mask file for capt.txt | Habitat mask - File input |
FTHLtrap.txt | Perimeter of horned lizard search area | Data input - Detector layout |
FTHLcapt.txt | Flat-tailed horned lizard test data |Data input - Captures |
FTHL.Rds | Saved flat-tailed horned lizard dataset | Data input |
hareCH6trap.txt | Snowshow hare data | Data input - Detector layout |
hareCH6capt.txt | Snowshow hare data | Data input - Captures |
hareCH6.xlsx | Snowshoe hare data as Excel spreadsheet | Data input |
hareCH6.Rds | Saved snowshoe hare dataset | Data input |
ovtrap.txt | Locations of possum traps in the Orongorongo Valley | Data input - Detector layout |
OVforest.dbf | ESRI polygon shapefile extent of habitat near possum traps | Habitat mask - Mask polygon files |
OVforest.shp |||
OVforest.shx |||
ovenCHp.xlsx | Ovenbird proximity data as Excel spreadsheet | Data input |
ovenCHp.Rds | Saved ovenbird proximity dataset | Data input |
