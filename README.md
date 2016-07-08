#[SDEFSR] (https://github.com/aklxao2/SDEFSR/) 

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SDEFSR)](https://cran.r-project.org/web/packages/SDEFSR/index.html)
[![Downloads] (http://cranlogs.r-pkg.org/badges/SDEFSR)](https://cran.rstudio.com/web/packages/SDEFSR/index.html)
[![totalDownloads] (http://cranlogs.r-pkg.org/badges/grand-total/SDEFSR)](https://cran.rstudio.com/web/packages/SDEFSR/index.html)

R package with Subgroup Discovery algorithms based on Evolutionary Fuzzy Systems.
These algorithms works with data sets with _KEEL_ , _ARFF_, or _CSV_ formats or data.frame objects . Also, the package provide a Shiny App to ease the work.

## Installation

The package is available on CRAN, so it could be installed as another package available on CRAN, simple typing:
```R
install.packages("SDEFSR")
```

You can install the package using the function `install_github()` provided by the `devtools` package:  

```R
devtools::install_github("aklxao2/SDEFSR")
```

## Usage and examples

The package provide a web interface to ease the work, this intarface could be accesed by calling the function: 
`SDR_GUI()`:

```R
library("SDR")
SDR_GUI()
```

This will launch the interface in our predetermined browser. 

Assuming the files `iris.dat`, `iris.arff` and `iris.csv` corresponding to the
classical iris dataset in KEEL, ARFF and CSV formats respectively in the working directory, the loading of these files will be as follows:
```R
> irisFromKEEL <- read.dataset("iris.dat") 
> irisFromARFF <- read.dataset("iris.arff")
> irisFromCSV <- read.dataset("iris.csv")
```
Note that the function detects the type of data by the extension. To read csv files, the function has optional parameters that defines the separator used between fields, the decimal separator, the quote indicator and the \code{NA} identifier as parameters. By default, this options and values are `sep = ",", quote = "\"", dec = "."` and `na.strings = "?"` respectively. 


The are four subgroup discovery implemented: __SDIGA__, __MESDIF__, __NMEEF-SD__ and __FuGePSD__. You can use this algorithms by:
* specifying the path of a parameter file 
* specifying all the parameters of the algorithm in the function call, one by one.  

The former is normally used with datasets prepared to make an execution directly, because this way does not allow to change the structure of the datasets and set other target variable, this target variable is the last variable defined in the dataset.
The latter is for dataset that we loaded in R and make some modifications.

Having a parameters file for MESDIF, for example, in our working directory called `paramFile.txt`:
```R
MESDIF("paramFile.txt")
``` 
For more information and examples about the use of the algorithms and subgroup discovery, please, refer to the documentation.
