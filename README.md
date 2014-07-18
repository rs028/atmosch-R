atmosch-R
=========

R code for atmospheric chemistry and mass spectrometry.

Download the R files in a directory (e.g. "C:\My Documents\atmosch-R")

Create a file called .Rprofile in your home directory (e.g. "C:\My
Documents) with this content:
    source("C:\My Documents\atmosch-R\main.R")

If .Rprofile already exists, just add that line. The atmosch-R
functions should now be available in R and can be called like all
other R functions.

The file main.R can be edited if only some functions are
required. Some of the code requires the "chron" library, which may
need to be installed separately from CRAN.
