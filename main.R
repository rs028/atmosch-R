### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Load the atmosch-R functions into the R workspace.
###
### version 2.6, Apr 2021
### author: RS
### ---------------------------------------------------------------- ###
library(chron)

## go to the repository directory
setwd(f.repo)

## source the atmosch-R files
source("atmosChem.R")
source("atmosPhys.R")
source("convertUnits.R")
source("fileUtils.R")
source("instrumentFiles.R")
source("massSpec.R")
source("modelAtchem.R")
#source("obsoleteFuncs.R")   # obsolete functions (uncomment if needed)
source("physChem.R")
source("processData.R")
source("referenceData.R")
source("utilityFuncs.R")

### ---------------------------------------------------------------- ###
