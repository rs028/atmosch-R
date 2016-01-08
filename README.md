atmosch-R
=========

Collection of R (http://www.r-project.org/) functions for mass
spectrometry, atmospheric chemistry and data analysis. Compatible with
the openair package (http://www.openair-project.org/).


CONFIGURATION
-------------

Download the atmosch-R files in a directory (e.g. "Z:\My Documents\atmosch-R")

Create a file called .Rprofile in the home directory (e.g. "Z:\My Documents)
with the following text:

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

If the .Rprofile file already exists, just add the text to the
existing file. Modify the directory paths in .Rprofile to match the
configuration of your system (e.g. version of win-library). On a Linux
system, .libPaths() may not be required and can be deleted or
commented out.

The .Rprofile file can be customized to your personal preferences
(e.g. if only some of the R functions are required).

The atmosch-R functions should now be available in R and can be listed
using the ls() command at the R prompt.


ADDITIONAL LIBRARIES
--------------------

Some atmosch-R functions require the "chron" library to handle dates
and times. To install it, type at the R prompt:

    install.packages("chron")

and follow the instructions.
