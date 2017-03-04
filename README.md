atmosch-R
=========

Collection of R (http://www.r-project.org/) functions for mass
spectrometry, atmospheric chemistry and data analysis. Compatible with
the **openair** package (http://www.openair-project.org/).

Use of this software is free (see LICENSE), but please acknowledge or
cite, as appropriate, if you use it. Comments, suggestions, requests,
reports of errors/bugs and submissions of code are welcome.

**N.B.:** It is _strongly recommended_ to check the results and the
  output of the functions, and make sure that they work as intended.


CONFIGURATION
-------------

Download the atmosch-R archive file (`atmosch-R-master.zip`) and unzip
it in a directory of choice (e.g. `Z:\My Documents\R Code\`). This
creates a directory called `atmosch-R-master/` containing the
atmosch-R files. Rename the directory as `atmosch-R/`.

Create a file called `.Rprofile` in your home directory (e.g. `Z:\My
Documents`) with the following content:

```
.libPaths("Z:\\My Documents\\R\\win-library\\3.0")
f.home <- "Z:\\My Documents\\R Code\\atmosch-R"

library(chron)

setwd(f.home)
source("atmosChem.R")
source("atmosPhys.R")
source("convertUnits.R")
source("fileUtils.R")
source("massSpec.R")
source("physChem.R")
source("processData.R")
source("referenceData.R")
source("utilityFuncs.R")
```

If `.Rprofile` already exists, add the text above to the existing
file. Modify the directory paths in `.Rprofile` to match the
configuration of your system (e.g. the version of win-library). On
Linux systems, `.libPaths()` may not be required; in this case line 1
can be deleted or commented out. The `.Rprofile` file can be
customized to your personal preferences (e.g. if only some of the
functions are required).

Restart R. The atmosch-R functions should now be available in R and
can be listed using the `ls()` command at the R prompt. The names of
all atmosch-R functions begin with lowercase "f" followed by the
capitalized function name.

Example code and data can be found in the
[wiki](https://github.com/rs028/atmosch-R/wiki/).


ADDITIONAL LIBRARIES
--------------------

Some atmosch-R functions require the **chron** library to handle dates
and times. To install it, type at the R prompt:

```
install.packages("chron")
```

and follow the instructions.

The atmosch-R functions are compatible with the **openair**
package. In particular, the `fOpenair()` function, in `processData.R`,
can be used to convert a data.frame to the openair format.

To install openair, type at the R prompt:

```
install.packages("openair")
```

and follow the instructions. Add `library(openair)` to
`.Rprofile`. For more information on **openair** see the [project
website](http://www.openair-project.org/).
