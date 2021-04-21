### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in modelAtchem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fAtchemIn()

## -----------------------------------------------
## fAtchemOut()

out1 <- list("speciesConcentrations.output", "environmentVariables.output",
             "photolysisRates.output", "photolysisRatesParameters.output",
             "mainSolverParameters.output")

df1 <- fAtchemOut("../test_files/", out1, "01-07-15 00:00:00")

assert("=> fAtchemOut() output",
       is.data.frame(df1),
       nrow(df1) == 5,
       ncol(df1) == 113
       )

## -----------------------------------------------
## fAtchemRates()

df1 <- fAtchemRates("../test_files/", "productionRates.output", "OH", "01-07-15 00:00:00")
df1a <- df1[[1]]
df1b <- df1[[2]]

df2 <- fAtchemRates("../test_files/", "lossRates.output", "HO2", "01-07-15 00:00:00")
df2a <- df2[[1]]
df2b <- df2[[2]]

assert("=> fAtchemRates() output - production rates file",
       is.list(df1),
       is.data.frame(df1a),
       is.data.frame(df1b),
       length(df1) == 2,
       nrow(df1a) == 5,
       nrow(df1b) == 15,
       ncol(df1a) == 2,
       ncol(df1b) == 5
       )

assert("=> fAtchemRates() output - loss rates file",
       is.list(df2),
       is.data.frame(df2a),
       is.data.frame(df2b),
       length(df2) == 2,
       nrow(df2a) == 4,
       nrow(df2b) == 12,
       ncol(df2a) == 2,
       ncol(df2b) == 5
       )

## -----------------------------------------------
## fConstrGap()
