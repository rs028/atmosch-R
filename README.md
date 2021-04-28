atmosch-R
=========

atmosch-R is a collection of [R](http://www.r-project.org/) functions for
atmospheric chemistry, mass spectrometry, data processing and analysis. It is
compatible with the [openair](https://davidcarslaw.github.io/openair/) package.

Use of this software is free (see `LICENSE.md`), but please acknowledge or cite,
as appropriate, if you use it. Comments, suggestions, requests, reports of
errors/bugs and submissions of code are welcome.

**N.B.:** it is _strongly recommended_ to check the results and the output of
the functions, and make sure that they work as intended.


SETUP & CONFIGURATION
---------------------

Download the atmosch-R [archive file](https://github.com/rs028/atmosch-R/archive/refs/heads/master.zip)
and unzip it in a directory of choice (e.g., `D:\My Documents\R Code\`). This creates the
repository directory, called `atmosch-R-master`, which contains the atmosch-R files.
Rename the repository directory to `atmosch-R/`.

The `.Rprofile` file, which contains the customization settings for R, should be
located in the home directory (e.g., `D:\My Documents\`). If `.Rprofile` does not exist, create it (see
[this note](https://stackoverflow.com/questions/28664852/saving-a-file-as-rprofile-in-windows),
if working on Windows).

Add the following lines to `.Rprofile`:

```
f.repo <- "D:\\My Documents\\R Code\\atmosch-R"

source(paste(f.repo, "main.R", sep=""))
```

The variable `f.repo` is a string with the path to the repository directory
(`atmosch-R/`). Edit `f.repo` in `.Rprofile` to match the configuration
of the system (e.g., a different directory name or Windows partition letter;
or UNIX-style directory paths for Linux systems).

**Restart R**. The atmosch-R functions should now be available in the R
workspace and can be listed using the command `fListWS("atmosch")` at the R
prompt. The names of all atmosch-R functions begin with lowercase _f_, followed
by the capitalized function name.

All functions in atmosch-R include a header with a brief description of what the
function does, the input, the output, and an example of how it is used.


ADDITIONAL PACKAGES
-------------------

-  The [chron](https://cran.r-project.org/web/packages/chron/) package
is used to handle dates and times. To install chron, type at the R prompt:
```
install.packages("chron")
```
and follow the instructions. Add `library(chron)` to `.Rprofile`.

- The [testit](https://cran.r-project.org/web/packages/testit/) package is used
for testing. It is not required in order to use the atmosch-R functions.
To install testit, type at the R prompt:
```
install.packages("testit")
```
and follow the instructions. To run the tests, use the command:
```
source("test_scripts/all-tests.R")
```

**N.B.:** on some systems -- depending on the OS, the user permissions, the R
installation, etc... -- it may be necessary to add the path of the additional R
packages to `.Rprofile` (e.g., `.libPaths("D:\\My Documents\\R\\win-library\\3.0"`)


OPENAIR
-------

atmosch-R can be used in conjunction with openair, a widely used R package for
analysis of air quality and atmospheric composition data. To install openair,
type at the R prompt:

```
install.packages("openair")
```

and follow the instructions. Optionally, add `library(openair)` to `.Rprofile`.

The openair uses the POSIX format to handle dates and times, instead of the chron format
used in atmosch-R. In addition, several openair functions make assumptions about the
names of some key variables, such as: date/time, wind speed, wind direction, etc...
The `fOpenair()` function in `processData.R` can be used to convert a _data.frame_
to the format used by openair functions.
