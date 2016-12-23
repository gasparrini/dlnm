[![Travis-CI Build Status](https://travis-ci.org/fabian-s/dlnm.svg?branch=cran-ready)](https://travis-ci.org/fabian-s/dlnm)

# Distributed Lag Non-linear Models (DLNM)

The package `dlnm` contains functions to specify and run distributed lag
linear and non-linear models. 

Distributed lag non-linear models (DLNM) 
represent a modelling framework to describe simultaneously non-linear and
delayed dependencies, termed as *exposure-lag-response associations*.
The methodology of DLNMs was originally developed for time series data, and
has been recently extended to other study designs and data structures,
compatible with cohort, case-control or longitudinal studies, amongst
others. A thorough methodological overview is given in the references and
the package vignettes, type `vignette(dlnmOverview)` for starters after loading 
the package.


### References:

Gasparrini A. Distributed lag linear and non-linear models in R:
the package dlnm. *Journal of Statistical Software*. 2011;
**43**(8):1-20, freely available [here](http://www.ag-myresearch.com/jss2011).

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