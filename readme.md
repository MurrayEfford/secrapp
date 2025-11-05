# secrapp 2.2

This Shiny app performs simple spatially explicit capture--recapture analyses using the 
R package [secr](https://CRAN.R-project.org/package=secr) (Efford 2025a). For details see the internal 
Help tab, the [step-by-step tutorial](https://www.otago.ac.nz/density/pdfs/secrapp-tutorial.html), 
the online [SECR Book] (Efford 2025b), and the [Density web page](https://www.otago.ac.nz/density/). 
See help.rmd for recent changes.

### Usage

Click this link to run [secrapp](https://www.stats.otago.ac.nz/secrapp) version 2.0 in your web 
browser from a University of Otago server without installing it locally (2.2 will follow soon).

It may be better to run **secrapp** on your own machine: models may fit faster, and you can 
install the latest the GitHub version. Also, there is a limit to the number of simultaneous users on the server.

To run secrapp in a local R session directly from GitHub, paste and run this code at the R command prompt:

```r
install.packages("shinyjs")  # skip if already installed; installs 'shiny' if needed
install.packages("secr")     # skip if already installed
shiny::runGitHub("secrapp", "MurrayEfford")
```

Note:

- for some functions you will also need the packages 'sf', 'stringr', and 'readxl'  
- secrapp 2.2 requires secr 5.3.0 or a later version.

### Test data

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

### References

Efford, M. G. (2025a) secr: Spatially explicit capture--recapture models. 
R package version 5.3.0. https://CRAN.R-project.org/package=secr

Efford, M. G. (2025b The SECR Book. A handbook of spatially explicit capture&ndash;recapture methods. Version 1.0.1.
Zenodo 15109938. Latest version online at https://murrayefford.github.io/SECRbook/.

[SECR book]: https://murrayefford.github.io/SECRbook/