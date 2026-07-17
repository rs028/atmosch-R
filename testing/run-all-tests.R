### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Run all the test scripts
### author: RS
### --------------------------------------------------------- ###
library(testit)

setwd(paste(f.repo, "testing/", sep=""))

cat("\n==========================")
cat("\n Test atmosch-R functions")
cat("\n==========================")

cat("\n[*] atmosChem.R")
source("scripts/run-atmosChem.R")
cat("\n    ..... OK")

cat("\n[*] atmosPhys.R")
source("scripts/run-atmosPhys.R")
cat("\n    ..... OK")

cat("\n[*] convertUnits.R")
source("scripts/run-convertUnits.R")
cat("\n    ..... OK")

cat("\n[*] fileUtils.R")
source("scripts/run-fileUtils.R")
cat("\n    ..... OK")

## cat("* testing: instrumentFiles.R")
## source("run-instrumentFiles.R")
## cat("\n    ..... OK")

## cat("* testing: massSpec.R")
## source("run-massSpec.R")
## cat("\n    ..... OK")

## cat("* testing: modelAtchem.R")
## source("run-modelAtchem.R")
## cat("\n    ..... OK")

cat("\n[*] physChem.R")
source("scripts/run-physChem.R")
cat("\n    ..... OK")

## cat("* testing: processData.R")
## source("run-processData.R")
## cat("\n    ..... OK")

cat("\n[*] referenceData.R")
source("scripts/run-referenceData.R")
cat("\n    ..... OK")

cat("\n[*] utilityFuncs.R")
source("scripts/run-utilityFuncs.R")
cat("\n    ..... OK")

cat("\n==========================\n\n")

setwd(f.repo)
