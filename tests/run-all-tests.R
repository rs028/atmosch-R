### --------------------------------------------------------- ###
### execute all test scripts
### --------------------------------------------------------- ###
library(testit)

setwd(paste(f.repo, "tests/", sep=""))

cat("\nTest of atmosch-R functions\n\n")

cat("* testing: atmosChem.R\n")
source("sc-atmosChem.R")
cat("  ..... OK\n")

cat("* testing: atmosPhys.R\n")
source("sc-atmosPhys.R")
cat("  ..... OK\n")

cat("* testing: convertUnits.R\n")
source("sc-convertUnits.R")
cat("  ..... OK\n")

##cat("* testing: fileUtils.R\n")
##source("sc-fileUtils.R")
##cat("  ..... OK\n")

##cat("* testing: instrumentFiles.R\n")
##source("sc-instrumentFiles.R")
##cat("  ..... OK\n")

##cat("* testing: massSpec.R\n")
##source("sc-massSpec.R")
##cat("  ..... OK\n")

##cat("* testing: modelAtchem.R\n")
##source("sc-modelAtchem.R")
##cat("  ..... OK\n")

##cat("* testing: physChem.R\n")
##source("sc-physChem.R")
##cat("  ..... OK\n")

##cat("* testing: processData.R\n")
##source("sc-processData.R")
##cat("  ..... OK\n")

cat("* testing: referenceData.R\n")
source("sc-referenceData.R")
cat("  ..... OK\n")

##cat("* testing: utilityFuncs.R\n")
##source("sc-utilityFuncs.R")
##cat("  ..... OK\n")

setwd("../")
