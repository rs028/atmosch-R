### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Run all the test scripts.
### author: RS
### --------------------------------------------------------- ###
library(testit)

setwd(paste(f.repo, "test_scripts/", sep=""))

cat("\nTest atmosch-R functions")
cat("\n========================\n\n")

cat("* testing: atmosChem.R\n")
source("run-atmosChem.R")
cat("  ....... OK\n")

cat("* testing: atmosPhys.R\n")
source("run-atmosPhys.R")
cat("  ....... OK\n")

cat("* testing: convertUnits.R\n")
source("run-convertUnits.R")
cat("  ....... OK\n")

cat("* testing: fileUtils.R\n")
source("run-fileUtils.R")
cat("  ....... OK\n")

cat("* testing: instrumentFiles.R\n")
source("run-instrumentFiles.R")
cat("  ....... OK\n")

## cat("* testing: massSpec.R\n")
## source("run-massSpec.R")
## cat("  ....... OK\n")

cat("* testing: modelAtchem.R\n")
source("run-modelAtchem.R")
cat("  ....... OK\n")

cat("* testing: physChem.R\n")
source("run-physChem.R")
cat("  ....... OK\n")

cat("* testing: processData.R\n")
source("run-processData.R")
cat("  ....... OK\n")

cat("* testing: referenceData.R\n")
source("run-referenceData.R")
cat("  ....... OK\n")

cat("* testing: utilityFuncs.R\n")
source("run-utilityFuncs.R")
cat("  ....... OK\n")

cat("\n========================\n\n")

setwd(f.repo)
