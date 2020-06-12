### --------------------------------------------------------- ###
### script to test the functions in modelAtchem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fAtchemIn()


## -----------------------------------------------
## fAtchemOut()

df1 <- fAtchemOut("../test_files/", list("speciesConcentrations.output", "environmentVariables.output"),
                  "01-07-15 00:00:00")
