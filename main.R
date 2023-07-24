### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Load the atmosch-R functions and the required packages into
### the R workspace.
###
### version 2.7, Jul 2023
### author: RS
### ---------------------------------------------------------------- ###
library(chron)

source(paste0(f.repo, "atmosChem.R"))
source(paste0(f.repo, "atmosPhys.R"))
source(paste0(f.repo, "convertUnits.R"))
source(paste0(f.repo, "fileUtils.R"))
source(paste0(f.repo, "instrumentFiles.R"))
source(paste0(f.repo, "massSpec.R"))
source(paste0(f.repo, "modelAtchem.R"))
source(paste0(f.repo, "physChem.R"))
source(paste0(f.repo, "processData.R"))
source(paste0(f.repo, "referenceData.R"))
source(paste0(f.repo, "utilityFuncs.R"))

## ------------------------------------------ ##
## obsolete functions (uncomment if needed)
#source(paste0(f.repo, "obsoleteFuncs.R"))
