atmosch-R
=========

**atmosch-R** is a collection of R (http://www.r-project.org/)
functions for atmospheric chemistry, mass spectrometry, data
processing and analysis. It is compatible with the [openair
package](https://davidcarslaw.github.io/openair/).

Use of this software is free (see `LICENSE.md`), but please acknowledge
or cite, as appropriate, if you use it. Comments, suggestions,
requests, reports of errors/bugs and submissions of code are welcome.

**N.B.:** it is _strongly recommended_ to check the results and the
  output of the functions, and make sure that they work as intended.


SETUP & CONFIGURATION
---------------------

Download the atmosch-R archive file (`atmosch-R-master.zip`) and unzip
it in a directory of choice (e.g., `Z:\My Documents\R Code\`). This
will create a directory called `atmosch-R-master/` with the atmosch-R
files. Rename the directory `atmosch-R/`.

The `.Rprofile` file, which contains the customization settings for R,
should be located in your home directory (e.g., `Z:\My Documents\`).
If it does not exist, create it (see [this
note](https://stackoverflow.com/questions/28664852/saving-a-file-as-rprofile-in-windows)
if working on Windows).

Add the following lines to `.Rprofile`:

```
f.repo <- "Z:\\My Documents\\R Code\\atmosch-R"

source(paste(f.repo, "main.R", sep=""))
```

The `f.repo` variable is a string containing the path to the directory
with the atmosch-R functions. Edit this variable in `.Rprofile` to
match the configuration of your system (e.g., a different directory
name or Windows partition letter; or UNIX-style directory paths for
Linux systems).

**Restart R**. The atmosch-R functions should now be available in the
R workspace and can be listed using the `ls()` command at the R
prompt. The names of all atmosch-R functions begin with lowercase "f"
followed by the capitalized function name.

Example code and test data can be found on the [wiki
pages](https://github.com/rs028/atmosch-R/wiki/).


ADDITIONAL PACKAGES
-------------------

Some atmosch-R functions require the **chron** library to handle dates
and times. To install chron, type at the R prompt:

```
install.packages("chron")
```

and follow the instructions. Add `library(chron)` to `.Rprofile`.

The atmosch-R functions can be used in conjunction with openair. In
particular, the `fOpenair()` function in `processData.R` can be used
to convert a data.frame to the format used by openair functions. To
install openair, type at the R prompt:

```
install.packages("openair")
```

and follow the instructions.  
_Optional:_ add `library(openair)` to `.Rprofile`.

On some machines -- depending on the operating system, the
installation method, the user permissions -- it may be necessary to
add the location of the additional libraries to `.Rprofile`. For
example (edit the path and R version to match your configuration):

```
.libPaths("Z:\\My Documents\\R\\win-library\\3.0")
```
