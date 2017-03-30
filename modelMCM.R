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
  ##     --> input files in `input.dir'
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
    ## get initial value of variable
    a1 <- fFindIdx(df.in$SEC, "LE", 0)
    init <- rbind(init, c(inp.str, df.in[a1,2]))
    ## write variable to file
    in.str <- fVarName(input.df[c])
    write(nrow(df.in), file=paste(input.dir, in.str, sep=""))
    write.table(df.in, file=paste(input.dir, in.str, sep=""),
                append=T, sep="\t", row.names=F, col.names=F)
  }
  return(init)
}

fAtchemOut <- function(output.dir, start.str) {
  ## load output files from the AtChem/MCM model:
  ## * concentration of chemical species
  ## * environmental variables
  ## * photolysis rates
  ## * model parameters
  ##
  ## input:
  ##       mcm_dir = model output directory
  ##       start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##        data.frame ( seconds, datetime chron, variable1, variable2, ... )
  ## ------------------------------------------------------------
  ## set directory and filename of output files
  fn.conc <- paste(output.dir, "concentration.output", sep="")
  fn.env <- paste(output.dir, "envVar.output", sep="")
  fn.jval <- paste(output.dir, "photolysisRates.output", sep="")
  fn.vars <- paste(output.dir, "photoRateCalcParameters.output", sep="")
  ## load output files
  out.conc <- read.delim(fn.conc, header=TRUE, sep="")
  out.env <- read.delim(fn.env, header=TRUE, sep="")
  out.jval <- read.delim(fn.jval, header=TRUE, sep="")
  out.vars <- read.delim(fn.vars, header=TRUE, sep="")
  ## merge model results
  df.all <- merge(out.conc, out.env, by="time")
  df.all <- merge(df.all, out.jval, by="time")
  df.all <- merge(df.all, out.vars, by="time")
  ## add timestamp (model time in seconds since start)
  df.out <- data.frame(SEC=df.all$time)
  df.out$datetime <- df.out$SEC/86400 + fChronStr(start.str, "d-m-y h:m:s")
  df.out <- cbind(df.out, df.all[-1])
  ## output data.frame
  colnames(df.out) <- toupper(colnames(df.out))
  return(df.out)
}
