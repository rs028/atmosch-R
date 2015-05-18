atmosch-R
=========

R code for atmospheric chemistry and mass spectrometry.


CONFIGURATION
-------------

Download the R files in a directory (e.g. "Z:\My Documents\atmosch-R")

Create a file called .Rprofile in your home directory (e.g. "Z:\My
Documents) with the following text:


```
.libPaths("Z:\\My Documents\\R\\win-library\\3.0")
f.home <- "Z:\\My Documents\\atmosch-R"

library(chron)

setwd(f.home)
source("atmosChem.R")
source("atmosPhys.R")
source("convertUnits.R")
source("massSpec.R")
source("physChem.R")
source("processData.R")
source("referenceData.R")
source("utilityFuncs.R")

```

N.B.: you may need to modify the directory paths in the above text to
match the configuration of your system.

If the .Rprofile file already exists, just add the text to the
existing file. You can customize the .Rprofile file to your personal
preferences (e.g. if only some of the R functions are required).

The atmosch-R functions should now be available in R and can be called
like all other R functions.


ADDITIONAL LIBRARIES
--------------------

Some of the atmosch-R functions require the "chron" library, which may
need to be installed separately from CRAN
(http://cran.r-project.org/).
