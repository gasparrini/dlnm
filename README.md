-----------------------------------

## dlnm: Distributed Lag Non-Linear Models

[![CRAN Version](https://www.r-pkg.org/badges/version/dlnm)](https://cran.r-project.org/package=dlnm)
[![Monthly Downloads](https://cranlogs.r-pkg.org/badges/dlnm)](https://cranlogs.r-pkg.org/badges/dlnm)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/dlnm)](https://cranlogs.r-pkg.org/badges/grand-total/dlnm)

The package `dlnm` contains functions to specify and interpret distributed lag linear (DLMs) and non-linear (DLNMs) models. The DLM/DLNM methodology is illustrated in detail in a series of articles referenced at the end of this document.

### Info on the `dlnm` package

The package `dlnm` is available on the Comprehensive R Archive Network (CRAN), with info at the related web page (https://cran.r-project.org/package=dlnm). A development website is available on GitHub (https://github.com/gasparrini/dlnm).

For a short summary of the functionalities of this package, refer to the main help page by typing:

```r
help(dlnm)
```

in R after installation (see below). For a more comprehensive overview, refer to the main vignette of the package that can be opened with:

```r
vignette("dlnmOverview")
```

### Installation

The last version officially released on CRAN can be installed directly within R by typing:

```r
install.packages("dlnm")
```

### R code in published articles

Several peer-reviewed articles and documents provide R code illustrating methodological developments of `dlnm` or replicating substantive results using this package. An updated version of the code can be found at the GitHub (httpsgithub.com/gasparrini) or personal web page (http://www.ag-myresearch.com) of the package maintainer.

### References:

Gasparrini A. Distributed lag linear and non-linear models in R:
the package dlnm. *Journal of Statistical Software*. 2011;
**43**(8):1-20. [freely available [here](http://www.ag-myresearch.com/2011_gasparrini_jss.html)]

Gasparrini A, Scheipl F, Armstrong B, Kenward MG. A penalized framework for distributed lag non-linear models. *Biometrics*. 2017;**73**(3):938-948. [freely available [here](http://www.ag-myresearch.com/2017_gasparrini_biomet.html)]://

Gasparrini A. Modelling lagged associations in environmental time series data: a simulation study. *Epidemiology*. 2016; **27**(6):835-842. [freely available [here](http://www.ag-myresearch.com/2016_gasparrini_epidem.html)]

Gasparrini A. Modeling exposure-lag-response associations with distributed
lag non-linear models. *Statistics in Medicine*. 2014;
**33**(5):881-899. [freely available [here](http://www.ag-myresearch.com/2014_gasparrini_statmed.html)].

Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear
models. *Statistics in Medicine*. 2010; **29**(21):2224-2234.
[freely available [here](http://www.ag-myresearch.com/2010_gasparrini_statmed.html)].

Gasparrini A., Armstrong, B., Kenward M. G. Reducing and meta-analyzing
estimates from distributed lag non-linear models. *BMC Medical Research
Methodology*. 2013; **13**(1):1. [freely available [here](http://www.ag-myresearch.com/2013_gasparrini_bmcmrm.html)].

Armstrong, B. Models for the relationship between ambient temperature and
daily mortality. *Epidemiology*. 2006, **17**(6):624-31. [available
[here](https://pubmed.ncbi.nlm.nih.gov/17028505/)].