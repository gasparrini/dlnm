\name{dlnm-package}
\alias{dlnm-package}
\alias{dlnm}
\docType{package}

\title{
Distributed Lag Non-linear Models (DLNM)
}

\description{
The package \pkg{dlnm} contains functions to specify and run distributed lag linear and non-linear models. These functions are used to build basis and cross-basis matrices and then to predict and plot the results for a fitted model.
}

\section{Modelling framework}{
Distributed lag non-linear models (DLNM) represent a modelling framework to describe simultaneously non-linear and delayed dependencies, termed as \emph{exposure-lag-response associations}. The methodology of DLNMs was originally developed for time series data, and has been recently extended to other study designs and data structures, compatible with cohort, case-control or longitudinal studies, amongst others. A thorough methodological overview is given in the references and the package vignettes detailed below.

The modelling framework is based on the definition of a \emph{cross-basis}, a bi-dimensional space of functions specifying the dependency along the space of the predictor and along lags. The cross-basis functions are built combining the basis functions for the two dimensions, produced by applying existing or user-defined functions such as splines, polynomials, linear threshold or indicators. The DLNM family includes simple distributed lag models (DLM) as a special case.

The application of DLNMs requires the availability of predictor values at equally-spaced time points. In the original development in time series analysis, these are represented by the ordered series of observations. More generally, the data can be stored in a matrix of \emph{exposure histories}, where each row represents the lagged values of the predictor for each observation.

The cross-basis matrix of transformed variables is included in the model formula of a regression model to estimate the associated parameters. The estimation can be carried out with the default regression functions, such as \code{\link{lm}}, \code{\link{glm}}, \code{gam} (package \pkg{mgcv}), \code{clogit} and \code{coxph} (package \pkg{survival}), \code{lme} (package \pkg{nlme}), \code{lmer} and \code{glmer} (package \pkg{lme4}). Estimates are then extracted to obtain predictions and graphical representations which facilitate the interpretation of the results.
}

\section{Functions and data included in the package}{
Given a time series vector or a matrix of exposure histories, \code{\link{crossbasis}} creates two set of basis functions to define the relationship in the two dimensions of predictor and lags. This step is performed through a call to the function \code{\link{onebasis}}, which in turn internally calls existing or user-defined functions and produces a basis matrix of class \code{"crossbasis"} with specific attributes. Standard choices for the functions in the two dimension are \code{\link[splines]{ns}} or \code{\link[splines]{bs}} from package \pkg{splines}, or the internal functions \code{\link{poly}}, \code{\link{strata}}, \code{\link{thr}}, \code{\link{integer}} and \code{\link{lin}} in \pkg{dlnm}. Other existing of user-defined functions can be also chosen. The functions \code{\link{equalknots}} and \code{\link{logknots}} can be used for knot placement. The two basis matrices are then combined in a matrix object of class \code{"crossbasis"}, containing the transformed variables to be included in the model formula. 

After the model fitting, \code{\link{crosspred}} generates predictions for a set of suitable values of the original predictor and lag period, and stores them in a \code{"crosspred"} object. The function \code{\link{exphist}} can be used to generate exposure histories for predictions. The fit of a DLNM can be reduced and re-expressed as the chosen function of one of the two dimensions through the function \code{\link{crossreduce}}. It returns a \code{"crossreduce"} object storing the new parameters and predictions.

Method functions are available for objects \code{"onebasis"}, \code{"crossbasis"}, \code{"crosspred"} and \code{"crossreduce"}. Specific \code{\link[=summary.crossbasis]{summary}} methods summarize the content of each object. The plotting functions \code{\link[=plot.crosspred]{plot}}, \code{\link[=lines.crosspred]{lines}} and \code{\link[=points.crosspred]{points}}, offer a set of choices to plot the results, while \code{\link[=coef.crosspred]{coef}} and \code{\link[=vcov.crosspred]{vcov}} return the coefficients and associated (co)variance matrix for a (optionally reduced) DLNM.

The data set \code{\link{chicagoNMMAPS}} is provided to perform examples of use of \pkg{dlnm} in time series analysis. It includes time series data of daily mortality counts, weather and pollution variables for Chicago in the period 1987-2000. The data sets \code{\link{nested}} and \code{\link{drug}} include simulated data to illustrate the extension of \pkg{dlnm} to other study designs, specifically nested case-controls and randomized controlled trials. The former contains information on 300 risk sets each with one cancer case and one matched control, and an occupational exposure collected in 5-year periods. The latter contains information on 200 subjects who are randomly allocated a different dose of a drug for two out of four weeks, with their outcome measured after 28 days.
}

\section{Additional information}{
Additonal details on the package \pkg{dlnm} are available in the vignettes included in the installation. These documents offer a detailed description of the capabilities of the package, and some examples of application to real data, with an extensive illustration of the use of the functions.

The vignette \strong{dlnmOverview} offers a general illustration of the DLNM methodology and the functions included in the package. The vignette \strong{dlnmTS} includes specific examples on the use of the functions for time series analysis. The vignette \strong{dlnmExtended} provides some examples on the extension of the methodology and package in other study designs and on the use of user-written functions.

A vignette is available by typing:

\code{vignette("dlnmOverview")}

A list of changes included in the current and previous versions can be found by typing:

\code{file.show(system.file("ChangeLog",package="dlnm"))}

General information on the development and applications of the DLNM modelling framework, together with an updated version of the \R{} scripts for running the examples in published papers, can be found at \bold{\href{http://www.ag-myresearch.com}{www.ag-myresearch.com}}.

Please use \code{citation("dlnm")} to cite this package.
}

\author{
Antonio Gasparrini and Ben Armstrong, with contributions from Fabian Scheipl

Maintainer: Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>
}

\references{
Gasparrini A. Distributed lag linear and non-linear models in R: the package dlnm. \emph{Journal of Statistical Software}. 2011; \bold{43}(8):1-20. [freely available \href{http://www.ag-myresearch.com/jss2011.html}{here}].

Gasparrini A. Modeling exposure-lag-response associations with distributed lag non-linear models. \emph{Statistics in Medicine}. 2014; \bold{33}(5):881-899. [freely available \href{http://www.ag-myresearch.com/statmed2014}{here}]
  
Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234. [freely available \href{http://www.ag-myresearch.com/statmed2010}{here}]

Gasparrini A., Armstrong, B., Kenward M. G. Reducing and meta-analyzing estimates from distributed lag non-linear models.\emph{BMC Medical Research Methodology}. 2013; \bold{13}(1):1. [freely available \href{http://www.ag-myresearch.com/bmcmrm2013}{here}].

Armstrong, B. Models for the relationship between ambient temperature and daily mortality. \emph{Epidemiology}. 2006, \bold{17}(6):624-31. [available \href{http://www.ncbi.nlm.nih.gov/pubmed/17028505}{here}]
}

\keyword{package}

\seealso{
\code{\link{onebasis}} to generate simple basis matrices. \code{\link{crossbasis}} to generate cross-basis matrices. \code{\link{crosspred}} to obtain predictions after model fitting. \code{\link{crossreduce}} to reduce the fit to one dimension. The methods \code{\link{plot.crosspred}} and \code{\link{plot.crossreduce}} to plot several type of graphs.

Type \code{'vignette(dlnmOverview)'} for a detailed description.
}

