# dbConnectGUI

dbConnectGUI provides a simple GUI for connecting to and exploring MySQL
databases.  This is a rewrite of the dbConnect and isn't finished yet.  
Not all functionality of MySQL is available through the GUI's interface.

## Installation

You can download the [zip](https://github.com/Dasonk/dbConnectGUI/zipball/master) or [tar ball](https://github.com/Dasonk/dbConnectGUI/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
## Make sure your current packages are up to date
update.packages()
## devtools is required
library(devtools)
install_github("dbConnectGUI", "Dasonk")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

One thing to note is that although RMySQL is only listed under "Suggests" it is required to use anything in this package.  This is for compatibility with CRAN.
