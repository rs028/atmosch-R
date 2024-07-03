### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Utilities and tools for atmosch-R functions:
### - fListWS()  : show variables in R workspace
### - fClearWS() : clear R workspace
### - fMergeDF() : merge list of data.frame
### - fFindIdx() : find point in vector greater/less than value
### - fVarName() : name of variable(s) in data.frame
###
### version 2.8, July 2024
### author: RS
### ---------------------------------------------------------------- ###

fListWS <- function(arg="") {
  ## Show functions/variables in the R workspace:
  ## * if no argument is given, atmosch-R functions/variables are
  ##   not shown
  ## * if argument is "atmosch", only the atmosch-R functions/variables
  ##   are shown
  ##
  ## NB: the names of all atmosch-R functions/variables begin with
  ##     lowercase `f` followed by a capital letter (functions) or by
  ##     a dot (variables).
  ##
  ## EXAMPLE:
  ##     fListWS("atmosch")
  ## ------------------------------------------------------------
  vv1 <- ls(pos=.GlobalEnv)
  vv2 <- ls(pos=.GlobalEnv, pattern="^f[.A-Z]")
  vv3 <- ls(pos=.GlobalEnv, pattern="^f[A-Z]")
  vv4 <- ls(pos=.GlobalEnv, pattern="^f[.]")
  if (arg == "atmosch") {
    cat("\natmosch-R variables:\n")
    print(vv4)
    cat("\natmosch-R functions:\n")
    print(vv3)
  } else {
    print(base::setdiff(vv1, vv2))
  }
}

fClearWS <- function() {
  ## Clear the R workspace:
  ## * delete all variables
  ## * keep atmosch-R functions/variables
  ## * close plot windows
  ##
  ## NB: the names of all atmosch-R functions/variables begin with
  ##     lowercase `f` followed by a capital letter (functions) or by
  ##     a dot (variables).
  ##
  ## EXAMPLE:
  ##     fClearWS()
  ## ------------------------------------------------------------
  vv1 <- ls(pos=.GlobalEnv)
  vv2 <- ls(pos=.GlobalEnv, pattern="^f[.A-Z]")
  rm(list=base::setdiff(vv1, vv2), pos=.GlobalEnv)
  graphics.off()
}

fMergeDF <- function(df.lst, var.str, type.str, suff.lst) {
  ## Merge two or more data.frames by a common variable, and rename
  ## the other variables using a list of suffixes.
  ##
  ## NB: the base function merge() only works on two data.frames at a
  ##     time -- see the documentation of merge().
  ##
  ## INPUT:
  ##     df.lst = list of data.frames to merge
  ##     var.str = name of common variable
  ##     type.str = type of merge operation ("ALL" to keep all rows OR
  ##                "NOTALL" to keep only the common rows)
  ##     suff.lst = list of suffixes to rename variables
  ## OUTPUT:
  ##     df.out = data.frame ( `merged data` )
  ## EXAMPLE:
  ##     xx <- fMergeDF(list(data_df1,data_df2,data_df3), "Time", "ALL", list("_a","_b","_c"))
  ## ------------------------------------------------------------
  lst.name <- deparse(substitute(df.lst))
  ## input validation
  if (!is.list(df.lst)) {
    stop(paste0("INPUT ERROR: ", lst.name, " must be a list"))
  }
  if (!all(sapply(df.lst, is.data.frame))) {
    stop(paste0("INPUT ERROR: all elements in ", lst.name, " must be data.frames"))
  }
  if (length(df.lst) != length(suff.lst)) {
    stop("INPUT ERROR: number of suffixes must match number of data.frames")
  }
  ## set type of merge operation
  type.all <- ifelse(type.str == "ALL", TRUE, FALSE)
  ## merge data.frames and rename variables
  if (length(df.lst) == 2) {   # two data.frames
    df.out <- merge(df.lst[1], df.lst[2], by=var.str, all=type.all,
                    suffixes=unlist(suff.lst))
  } else {   # multiple data.frames
    df.out <- df.lst[[1]]
    var.n <- which(colnames(df.out) == var.str)
    colnames(df.out)[-var.n] <- paste0(colnames(df.out)[-var.n], suff.lst[[1]])
    for (i in 2:length(df.lst)) {
      df.i <- df.lst[[i]]
      colnames(df.i)[-var.n] <- paste0(colnames(df.i)[-var.n], suff.lst[[i]])
      df.out <- merge(df.out, df.i, by=var.str, all=type.all)
    }
  }
  return(df.out)
}

fFindIdx <- function(vecd, ops, xval) {
  ## Find the first point that is greater/less than a reference value
  ## in an ordered data/chron vector.
  ##
  ## INPUT:
  ##     vecd = ordered data/chron vector
  ##     ops = greater/equal ("GE") OR greater ("G") OR less ("L") OR
  ##           less/equal ("LE")
  ##     xval = reference value
  ## OUTPUT:
  ##     idx = index of first point greater/less than reference value
  ## EXAMPLE:
  ##     xx <- fFindIdx(data_df$Datetime, "GE", chron("01/21/15","10:15:30"))
  ## ------------------------------------------------------------
  vecd <- unlist(vecd, use.names=FALSE)
  ## length, first, last values of data/chron vector
  vecd.n <- length(vecd)
  vecd.first <- vecd[1]
  vecd.last <- vecd[vecd.n]
  ## reference value is greater than any value in vector
  if (ops == "GE" && xval >= vecd.last) {
    return(vecd.n)
  }
  if (ops == "G" && xval > vecd.last) {
    return(vecd.n)
  }
  ## reference value is less than any value in vector
  if (ops == "LE" && xval <= vecd.first) {
    return(1)
  }
  if (ops == "L" && xval < vecd.first) {
    return(1)
  }
  ## index of first point in vector greater/less than reference value
  idx <- switch(ops,
                "GE" = head(which(vecd >= xval), 1),
                "G"  = head(which(vecd > xval), 1),
                "L"  = tail(which(vecd < xval), 1),
                "LE" = tail(which(vecd <= xval), 1),
                stop("INPUT ERROR: invalid operator")
                )
  return(idx)
}

fVarName <- function(var.df) {
  ## Extract the name of one or more variables in a data.frame.
  ##
  ## A variable can be addressed with:
  ## * the column number (df[1], returning a data.frame)
  ## * the variable name (df["A"], returning a data.frame)
  ## * the `$` operator (df$A, returning a numeric array)
  ##
  ## NB: addressing a variable with the column number as df[,1] or
  ##     df[[1]] returns a numeric array which does not contain the
  ##     variable name.
  ##
  ## INPUT:
  ##    var.df = variable(s) in data.frame
  ## OUTPUT:
  ##    var.name = name of variable(s)
  ## EXAMPLE:
  ##     xx <- fVarName(data_df$A)
  ## ------------------------------------------------------------
  if (is.data.frame(var.df)) {   # df[1] OR df["A"]
    var.name <- colnames(var.df)
  } else {
    var.dat <- deparse(substitute(var.df))
    if (grepl("\\$", var.dat)) {   # df$A
      var.str <- strsplit(var.dat, "\\$")
      var.name <- unlist(var.str)[2]
    } else {
      stop("INPUT ERROR: variable does not contain name")
    }
  }
  return(var.name)
}
