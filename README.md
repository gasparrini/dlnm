[![Travis-CI Build Status](https://travis-ci.org/fabian-s/dlnm.svg?branch=cran-ready)](https://travis-ci.org/fabian-s/dlnm)

-----------------------------------

## Distributed Lag Non-linear Models (DLNM)

The package `dlnm` contains functions to specify and interpret distributed lag linear (DLMs) and non-linear (DLNMs) models. The DLM/DLNM methodology is illustrated in detail in a series of articles referenced at the end of this document.

### Info on the `dlnm` package

The package is available on the Comprehensive R Archive Network (CRAN), with info at the related web page (https://cran.r-project.org/package=dlnm). A development website is available on GitHub (https://github.com/gasparrini/dlnm).

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

Several peer-reviewed articles and documents provide R code illustrating methodological developments or replicating substantive results. An updated version of the code can be found at the GitHub (https://github.com/gasparrini) or personal web page (http://www.ag-myresearch.com) of the package maintainer.

### References:

Gasparrini A. Distributed lag linear and non-linear models in R:
the package dlnm. *Journal of Statistical Software*. 2011;
**43**(8):1-20, freely available [here](http://www.ag-myresearch.com/jss2011).

Gasparrini A, Scheipl F, Armstrong B, Kenward MG. A penalized framework for distributed lag non-linear models. *Biometrics*. 2017; in press. [freely available here]

Gasparrini A. Modeling exposure-lag-response associations with distributed
lag non-linear models. *Statistics in Medicine*. 2014;
**33**(5):881-899, freely available [here](http://www.ag-myresearch.com/statmed2014).

Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear
models. *Statistics in Medicine*. 2010; **29**(21):2224-2234,
freely available [here](http://www.ag-myresearch.com/statmed2010).

Gasparrini A., Armstrong, B., Kenward M. G. Reducing and meta-analyzing
estimates from distributed lag non-linear models. *BMC Medical Research
Methodology*. 2013; **13**(1):1, freely available [here](http://www.ag-myresearch.com/bmcmrm2013).

Armstrong, B. Models for the relationship between ambient temperature and
daily mortality. *Epidemiology*. 2006, **17**(6):624-31, available
[here](http://www.ncbi.nlm.nih.gov/pubmed/17028505).