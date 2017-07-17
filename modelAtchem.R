### ---------------------------------------------------------------- ###
### functions for the AtChem/MCM model:
### - fAtchemIn()  : AtChem input files
### - fAtchemOut() : AtChem output files
###
### version 1.0, Mar 2017
### author: RS
### ---------------------------------------------------------------- ###

fAtchemIn <- function(input.dir, input.df, start.str) {
  ## make input files for the AtChem/MCM model:
  ## * concentrations in molecule cm-3
  ## * temperature in K
  ## * pressure in Pa
  ## * relative humidity in %
  ## * photolysis rates in s-1
  ## * sun declination in rad
  ##
  ## NB: the data.frame with the input data must have one datetime
  ## chron variable (TIME) and one or more data variables:
  ##
  ##        TIME           variable 1     variable 2   variable 3
  ##  21-01-15 12:00:00        10             25           30
  ##  21-01-15 12:30:00        25             30           45
  ##  21-01-15 13:00:00        40             55           60
  ##   ...
  ##
  ## input:
  ##     input.dir = directory for input files
  ##     input.df = input data.frame
  ##     start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##     init = list of values at model start datetime
  ##     --> input files in `input.dir`
  ## ------------------------------------------------------------
  if (!is.data.frame(input.df)) {
    df.name <- deparse(substitute(input.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
  ## time in seconds since model start
  model.start <- fChronStr(start.str, "d-m-y h:m:s")
  input.df$SEC <- round((input.df$TIME - model.start) * 86400)
  ## write each variable to file and make list of initial values
  init <- list()
  for (c in 2:(ncol(input.df)-1)) {
    inp.str <- fVarName(input.df[c])
    ## set negative values to zero and remove rows with NaN
    inp.var <- ifelse(input.df[[c]] < 0, 0, input.df[[c]])
    df.in <- data.frame(SEC=input.df$SEC, VAR=inp.var)
    df.in <- df.in[which(!is.na(df.in$VAR)),]
    ## find initial value of variable
    a1 <- fFindIdx(df.in$SEC, "LE", 0)
    init <- rbind(init, c(inp.str, df.in[a1,2]))
    ## write variable to file
    in.str <- fVarName(input.df[c])
    write.table(df.in, file=paste(input.dir, in.str, sep=""),
                sep="\t", row.names=F, col.names=F)
  }
  return(init)
}

fAtchemOut <- function(output.dir, output.lst, start.str) {
  ## load output files from the AtChem/MCM model:
  ## * concentration of chemical species
  ## * environmental variables
  ## * photolysis rates
  ## * model parameters
  ##
  ## input:
  ##       output.dir = model output directory
  ##       output.lst = list of output files
  ##       start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##        data.frame ( seconds, datetime chron, variable1, variable2, ... )
  ## ------------------------------------------------------------
  if (!is.list(output.lst)) {
    lst.name <- deparse(substitute(output.lst))
    stop(paste(lst.name, "must be a list", sep=" "))
  }
  ## load output files
  df.res <- read.delim(paste(output.dir, output.lst[1], sep=""), header=TRUE, sep="")
  if (length(output.lst) > 1) {
    for (i in 2:length(output.lst)) {
      res.i <- read.delim(paste(output.dir, output.lst[i], sep=""), header=TRUE, sep="")
      df.res <- merge(df.res, res.i, by="time")
    }
  }
  ## add timestamp (model time in seconds since start)
  df.out <- data.frame(SEC=df.res$time)
  df.out$datetime <- df.out$SEC/86400 + fChronStr(start.str, "d-m-y h:m:s")
  ## output data.frame
  df.out <- cbind(df.out, df.res[-1])
  colnames(df.out) <- toupper(colnames(df.out))
  return(df.out)
}
