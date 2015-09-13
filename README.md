[SDR] (https://github.com/aklxao2/SDR/) 
====

[![Travis-CI Build Status](https://travis-ci.org/aklxao2/SDR.svg?branch=master)](https://travis-ci.org/aklxao2/SDR)  
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SDR)](https://cran.r-project.org/web/packages/SDR/index.html)
[![Downloads] (http://cranlogs.r-pkg.org/badges/SDR)](https://cran.rstudio.com/web/packages/SDR/index.html)

Implementation of algorithms of the data mining task called "subgroup discovery" implemented directly in R without any other dependecies.
Those algorithms works with data sets provided in the format of _KEEL data mining tool_ . Also, the package provide a Shiny App to ease the work.
The interface is also available at http://sdrinterface.shinyapps.io/shiny to make the interface without R installed in our systems.

## Installation

The package is available on CRAN, so it could be installed as another package available on CRAN, simple typing:
```R
install.packages("SDR")
```

You can install the package using the function `install_github()` provided by the `devtools` package:  

```R
devtools::install_github("aklxao2/SDR")
```

## Usage and examples

The package provide a web interface to ease the work, this intarface could be accesed by:
* visiting http://sdrinterface.shinyapps.io/shiny
* calling the function to launch the interface in a local server.  
To create the local server you can simply call the function `SDR_GUI()` of our package:  
```R
library("SDR")
SDR_GUI()
```

This will launch the interface in our predetermined browser. 

Also the package can handle with _KEEL_ datasets to use within the subgroups discovery algorithms in order to use it in a R script for example. 
Those functions could change the target variable used by the algorithms, and also modify the fuzzy sets definitions created. For example, having the file `iris.dat` in our working directory:
```R
irisKEEL <- read.keel("iris.dat")
irisKEEL <- modifyFuzzyCrispIntervals(irisKEEL, 6) #Use 6 fuzzy labels 
#With classic iris dataset we cannot change the target variable, we need a categorical one. So, this function throws an error with this dataset
irisKEEL <- changeTargetVariable(irisKEEL, 3) #Use variable at position 3 as target variable.
```

The are three subgroup discovery implemented: __SDIGA__, __MESDIF__ and __NMEEF-SD__ You can use this algorithms by:
* specifying the path of a parameter file 
* specifying all the parameters of the algorithm in the function call, one by one.  

The former is normally used with datasets prepared to make an execution directly, because this way does not allow to change the structure of the datasets and set other target variable, this target variable is the last variable defined in the dataset.
The latter is for dataset that we loaded in R and make some modifications.

Having a parameters file for MESDIF, for example, in our working directory called `paramFile.txt`:
```R
MESDIF("paramFile.txt")
``` 
For more information and examples about the use of the algorithms and subgroup discovery, please, refer to the documentation.
