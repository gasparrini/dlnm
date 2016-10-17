#' Distributed Lag Non-linear Models (DLNM)
#' 
#' The package \pkg{dlnm} contains functions to specify and run distributed lag
#' linear and non-linear models. These functions are used to build basis and
#' cross-basis matrices and then to predict and plot the results for a fitted
#' model.
#' 
#' 
#' @name dlnm-package
#' @aliases dlnm-package dlnm
#' @docType package
#' @section Modelling framework: Distributed lag non-linear models (DLNM)
#' represent a modelling framework to describe simultaneously non-linear and
#' delayed dependencies, termed as \emph{exposure-lag-response associations}.
#' The methodology of DLNMs was originally developed for time series data, and
#' has been recently extended to other study designs and data structures,
#' compatible with cohort, case-control or longitudinal studies, amongst
#' others. A thorough methodological overview is given in the references and
#' the package vignettes detailed below.
#' 
#' The modelling framework is based on the definition of a \emph{cross-basis},
#' a bi-dimensional space of functions specifying the dependency along the
#' space of the predictor and along lags. The cross-basis functions are built
#' combining the basis functions for the two dimensions, produced by applying
#' existing or user-defined functions such as splines, polynomials, linear
#' threshold or indicators. The DLNM family includes simple distributed lag
#' models (DLM) as a special case.
#' 
#' The application of DLNMs requires the availability of predictor values at
#' equally-spaced time points. In the original development in time series
#' analysis, these are represented by the ordered series of observations. More
#' generally, the data can be stored in a matrix of \emph{exposure histories},
#' where each row represents the lagged values of the predictor for each
#' observation.
#' 
#' The cross-basis matrix of transformed variables is included in the model
#' formula of a regression model to estimate the associated parameters. The
#' estimation can be carried out with the default regression functions, such as
#' \code{\link{lm}}, \code{\link{glm}}, \code{gam} (package \pkg{mgcv}),
#' \code{clogit} and \code{coxph} (package \pkg{survival}), \code{lme} (package
#' \pkg{nlme}), \code{lmer} and \code{glmer} (package \pkg{lme4}). Estimates
#' are then extracted to obtain predictions and graphical representations which
#' facilitate the interpretation of the results.
#' @author Antonio Gasparrini and Ben Armstrong, with contributions from Fabian
#' Scheipl
#' 
#' Maintainer: Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{onebasis}} to generate simple basis matrices.
#' \code{\link{crossbasis}} to generate cross-basis matrices.
#' \code{\link{crosspred}} to obtain predictions after model fitting.
#' \code{\link{crossreduce}} to reduce the fit to one dimension. The methods
#' \code{\link{plot.crosspred}} and \code{\link{plot.crossreduce}} to plot
#' several type of graphs.
#' 
#' Type \code{'vignette(dlnmOverview)'} for a detailed description.
#' @references Gasparrini A. Distributed lag linear and non-linear models in R:
#' the package dlnm. \emph{Journal of Statistical Software}. 2011;
#' \bold{43}(8):1-20, freely available at
#' \url{http://www.ag-myresearch.com/jss2011}.
#' 
#' Gasparrini A. Modeling exposure-lag-response associations with distributed
#' lag non-linear models. \emph{Statistics in Medicine}. 2014;
#' \bold{33}(5):881-899, freely available at
#' \url{http://www.ag-myresearch.com/statmed2014}.
#' 
#' Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear
#' models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234,
#' freely available at \url{http://www.ag-myresearch.com/statmed2010}.
#' 
#' Gasparrini A., Armstrong, B., Kenward M. G. Reducing and meta-analyzing
#' estimates from distributed lag non-linear models.\emph{BMC Medical Research
#' Methodology}. 2013; \bold{13}(1):1, freely available at
#' \url{http://www.ag-myresearch.com/bmcmrm2013}.
#' 
#' Armstrong, B. Models for the relationship between ambient temperature and
#' daily mortality. \emph{Epidemiology}. 2006, \bold{17}(6):624-31, available
#' at \url{http://www.ncbi.nlm.nih.gov/pubmed/17028505}.
#' 
#' @keywords package
NULL
# TODOs
# - argument order of exphist, exphistint is different - why ?!?
# - vignette Rnw sources are missing
# - looking up "fun"-string in onebasis/checkonebasis instead of handing over a
#   proper function invites nasty scoping issues that seem to break examples in 
#   R CMD check. 


#-------------------------------------------------------------------------------

#' Internal Functions for Package dlnm
#' 
#' These functions are called internally by other functions and are not meant
#' to be directly run by the users.
#' @note These and other undocumented internal functions are not exported in
#' the namespace of the package \pkg{dlnm}. The user can visualize them through
#' the use of the triple colon operator '\code{:::}' or through the function
#' \code{\link{getAnywhere}}.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso See \code{\link{dlnm-package}} for an introduction to the package
#' and for links to package vignettes providing more detailed information.
#' @keywords internal
#' @aliases internals dlnm-internal
#' @name internals
NULL

#-------------------------------------------------------------------------------
# documentation for datasets below


#' A Trial on the Effect of Time-Varying Doses of a Drug
#' 
#' The data set contains simulated data from an hypothetical randomized
#' controlled trial on the effect of time-varying doses of a drug. The study
#' include records for 200 randomized subjects, each receiving doses of a drug
#' randomly allocated in two out of four weeks, with daily doses varying each
#' week. The daily doses are reported on 7-day intervals corresponding to each
#' week.
#' 
#' The exposure history for each subject (series of daily doses from day 28 to
#' 1) can be recovered by expanding the values given in
#' \code{day1.7}-\code{day22.28}.
#' 
#' @name drug
#' @docType data
#' @format A data frame with 200 observations on the following 7 variables.
#' \itemize{ \item\code{id}: subject ID.  \item\code{out}: the outcome level
#' measured at day 28.  \item\code{sex}: the sex of the subject.
#' \item\code{day1.7}: daily dose for the first week.  \item\code{day8.14}:
#' daily dose for the second week.  \item\code{day15.21}: daily dose for the
#' third week.  \item\code{day22.28}: daily dose for the fourth week.  }
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{nested}} for an example of nested case-control study
#' data. \code{\link{chicagoNMMAPS}} for an example of time series data.
#' 
#' The application of DLNMs to these data with detailed examples are given in
#' vignette \strong{dlnmExtended}.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @source This data set only contains simulated data.
#' @keywords datasets
NULL

#' Daily Mortality Weather and Pollution Data for Chicago
#' 
#' The data set contains daily mortality (all causes, CVD, respiratory),
#' weather (temperature, dew point temperature, relative humidity) and
#' pollution data (PM10 and ozone) for Chicago in the period 1987-2000 from the
#' National Morbidity, Mortality and Air Pollution Study (NMMAPS)
#' 
#' These data represents a subsample of the variables included in the NMMAPS
#' dataset for Chicago.
#' 
#' The variable \code{temp} is derived from the original \code{tmpd} after a
#' transformation from Fahrenheit to Celsius. The variables \code{pm10} and
#' \code{o3} are an approximated reconstruction of the original series, adding
#' the de-trended values and the median of the long term trend. This is the
#' reason they include negative values.
#' 
#' @name chicagoNMMAPS
#' @docType data
#' @format A data frame with 5114 observations on the following 14 variables.
#' \itemize{ \item\code{date}: Date in the period 1987-2000.  \item\code{time}:
#' The sequence of observations \item\code{year}: Year \item\code{month}: Month
#' (numeric) \item\code{doy}: Day of the year \item\code{dow}: Day of the week
#' (factor) \item\code{death}: Counts of all cause mortality excluding accident
#' \item\code{cvd}: Cardiovascular Deaths \item\code{resp}: Respiratory Deaths
#' \item\code{temp}: Mean temperature (in Celsius degrees) \item\code{dptp}:
#' Dew point temperature \item\code{rhum}: Mean relative humidity
#' \item\code{pm10}: PM10 \item\code{o3}: Ozone }
#' @seealso \code{\link{nested}} for an example of analysing
#' exposure-lag-response associations in a nested case-control study.
#' \code{\link{drug}} for an example of analysing exposure-lag-response
#' associations in a randomized controlled trial.
#' 
#' The application of DLNMs to this data with more detailed examples are given
#' in vignette \strong{dlnmExtended}.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @source The complete dataset used to be available at the Internet-based
#' Health and Air Pollution Surveillance System (iHAPSS) website:
#' 
#' \url{http://www.ihapss.jhsph.edu}
#' 
#' or through the packages \pkg{NMMAPSdata} or \pkg{NMMAPSlite}. Currently, the
#' data are not available any more and the two packages have been archived.
#' @keywords datasets
NULL

#' Nested Case-Control Study with a Time-Varying Exposure and a Cancer Outcome
#' 
#' The data set contains simulated data from an hypothetical nested
#' case-control study on the association between a time-varying occupational
#' exposure and a cancer outcome. The study includes 300 risk sets, each with a
#' case and a control matched by age year. The data on the exposure is
#' collected on 5-year age intervals between 15 and 65 years.
#' 
#' The exposure history for each subject (series of yearly exposures) can be
#' recovered by expanding the values given in \code{exp15}-\code{exp60}, and
#' then selecting the values backward from the age of the subject for a given
#' lag period.
#' 
#' @name nested
#' @docType data
#' @format A data frame with 600 observations on the following 14 variables.
#' \itemize{ \item\code{id}: subject ID.  \item\code{case}: indicator for case
#' (1) or control (0).  \item\code{age}: age of each subject.
#' \item\code{riskset}: risk set id.  \item\code{exp15}: yearly exposure in the
#' age period 15-19 year.  \item\code{exp20}: yearly exposure in the age period
#' 20-24 year.  \item\code{...} \item\code{exp60}: yearly exposure in the age
#' period 60-64 year.  }
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{drug}} for an example of randomized controlled trial
#' data. \code{\link{chicagoNMMAPS}} for an example of time series data.
#' 
#' The application of DLNMs to these data with detailed examples are given in
#' vignette \strong{dlnmExtended}.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @source These nested case-control data were extracted from a simulated
#' cohort with 300 cases of cancer and a time-varying exposure.
#' @keywords datasets
NULL



