# secrapp 1.4

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

Note 
- secrapp 1.3 required secr 4.3
- secrapp 1.4 requires secr 4.4.2 or a later version.


The app may also be run in a web browser without any setup by following this 
[link](https://www.stats.otago.ac.nz/secrapp). This arrangement is experimental 
and has limited capacity.

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
FTHL.Rds | Saved flat-tailed horned lizard dataset | Data input |
hareCH6trap.txt | Snowshow hare data | Data input - Detector layout |
hareCH6capt.txt | Snowshow hare data | Data input - Captures |
hareCH6.xlsx | Snowshoe hare data as Excel spreadsheet | Data input |
hareCH6.Rds | Saved snowshoe hare dataset | Data input |
ovtrap.txt | Locations of possum traps in the Orongorongo Valley | Data input - Detector layout |
ovcapt.txt | Brushtail possum captures in the Orongorongo Valley | Data input - Captures |
OVforest.dbf | ESRI polygon shapefile extent of habitat near possum traps | Habitat mask - Mask polygon files |
OVforest.shp |||
OVforest.shx |||
ovenCHp.xlsx | Ovenbird proximity data as Excel spreadsheet | Data input |
ovenCHp.Rds | Saved ovenbird proximity dataset | Data input |
