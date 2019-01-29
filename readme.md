# secrdesignapp 1.1

secrdesignapp is a partial interactive interface to the R package [secrdesign](https://CRAN.R-project.org/package=secrdesign). See the Help tab for details.

The main new feature in version 1.1 is support for systematic and random designs within a region of interest.

Click this link to run secrdesignapp 1.1 in your web browser from a University of Otago server:

[secrdesignapp](https://www.stats.otago.ac.nz/secrdesignapp)

Simulations may run faster on your own machine, and the GitHub version is always the latest. To run secrdesignapp 1.1 in a local R session directly from GitHub:

```r
library(shiny)
runGitHub("secrdesignapp", "MurrayEfford")
```

Version 1.1 requires secr >= 3.2.0 and secrdesign >= 2.5.6. Some simulation features require secrdesign >= 2.5.7 and openCR 1.3.3 that are not yet on CRAN.

If you intend to use bookmarking on your local machine to store the state of `secrdesignapp` then specify the 'port' and 'destdir' arguments, e.g.,

```r
library(shiny)
runGitHub("secrdesignapp", "MurrayEfford", port = 8000, destdir = "d:/density secr 3.2/testing")
```
The URL generated by pressing the Bookmark button may then be entered into your browser once you have started a new session with the same 'port' and 'destdir'. This restores all input settings and the Summary table, but not results on the Simulation or Spacing pages. Bookmarking of ESRI shapefile inputs is not supported.

----

Some test data are provided:

| File name | Description | Usage |
|--------|-------------------------------|------------------|
ovtrap.txt | Locations of possum traps in the Orongorongo Valley | Design - File input of detector array |
OVforest.dbf | ESRI polygon shapefile extent of habitat near possum traps | Design - Region and Options - Habitat clip to polygons |
OVforest.shp |||
OVforest.shx |||
excltest.txt | OV polygon to demonstrate exclusion feature | Options - Detector array - Excluded region |
regionxy.txt | text file of a hypothetical study area boundary |Design - Region and Options - Habitat clip to polygons|
