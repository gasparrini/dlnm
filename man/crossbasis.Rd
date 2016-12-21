\name{crossbasis}
\alias{crossbasis}
\alias{summary.crossbasis}

\title{ Generate a Cross-Basis Matrix for a DLNM }

\description{
The function generates the basis matrices for the two dimensions of predictor and lags, given the functions selected to model the relationship in each space. Then, these one-dimensions basis matrices are combined in order to create the related cross-basis matrix, which can be included in a model formula to fit distributed lag linear (DLMs) and non-linear models (DLNMs).
}

\usage{
crossbasis(x, lag, argvar=list(), arglag=list(), group=NULL, ...)

\method{summary}{crossbasis}(object, ...)
}

\arguments{
  \item{x }{ either a numeric vector representing a complete series of ordered observations (for time series data), or a matrix of exposure histories over the same lag period for each observation. See Details below.}
  \item{lag }{ either an integer scalar or vector of length 2, defining the the maximum lag or the lag range, respectively.}
  \item{argvar, arglag }{ lists of arguments to be passed to the function \code{\link{onebasis}} for generating the two basis matrices for predictor and lags, respectively. See Details below.}
 \item{group }{ a factor or a list of factors defining groups of observations. Only for time series data.}
 \item{object }{ a object of class \code{"crossbasis"}.}
  \item{\dots }{ additional arguments. See Details below.}
}

\details{
The argument \code{x} defines the type of data. If a \eqn{n}-dimensional vector, the data are interpreted as a time series of equally-spaced and complete observations. If a \eqn{n \times (L-\ell_0+1)}{n x (L-L0+1)} matrix, the data are interpreted as a set of complete exposure histories at equally-spaced lags over the same lag period from \eqn{\ell_0}{L0} to \eqn{L} for each observation. The latter is general and can be used for applying DLMs and DLNMs beyond time series data. Lags are usually positive integers: if not provided, by default the minimum lag \eqn{L0} is set to 0, and the maximum lag \eqn{L} is set to 0 if \code{x} is a vector or to \code{ncol(x)-1} otherwise. Negative lags are rarely needed but allowed.

The lists in \code{argvar} and \code{arglag} are passed to \code{\link{onebasis}}, which calls existing or user-defined functions to build the related basis matrices. The two lists should contain the argument \code{fun} defining the chosen function, and a set of additional arguments of the function. The \code{argvar} list is applied to \code{x}, in order to generate the matrix for the space of the predictor. The \code{arglag} list is applied to a new vector given by the sequence obtained by \code{lag}, in order to generate the matrix for the space of lags. By default, the basis functions for lags are defined with an intercept (if not otherwise stated). Some arguments can be automatically re-set by \code{\link{onebasis}}. Then, the two set of basis matrices are combined in order to create the related cross-basis matrix.

Common choices for \code{fun} are represented by \code{\link[splines]{ns}} and \code{\link[splines]{bs}} from package \pkg{splines} or by the internal functions of the package \pkg{dlnm}, namely \code{\link{poly}}, \code{\link{strata}}, \code{\link{thr}}, \code{\link{integer}} and \code{\link{lin}}. In particular, DLMs can be considered a special case of DLNMs with a linear function in \code{argvar}. Functions \code{\link{ps}} and \code{\link{cr}} are used to specify penalized models with an external method (see \code{\link{cbPen}}). See \code{help(onebasis)} and the help pages of these functions for information on the additional arguments to be specified. Also, other existing or user-defined functions can be applied.

The argument \code{group}, only used for time series data, defines groups of observations representing independent series. Each series must be consecutive, complete and ordered.
}

\value{
A matrix object of class \code{"crossbasis"} which can be included in a model formula in order to fit a DLM or DLNM. It contains the attributes \code{df} (vector of length 2 with the df for each dimension), \code{range} (range of the original vector of observations), \code{lag} (lag range), \code{argvar} and \code{arglag} (lists of arguments defining the basis functions in each space, which can be modified if compared to lists used in the call). The method \code{summary.crossbasis} returns a summary of the cross-basis matrix and the related attributes, and can be used to check the options for the basis functions chosen for the two dimensions.
}

\references{
Gasparrini A. Distributed lag linear and non-linear models in R: the package dlnm. \emph{Journal of Statistical Software}. 2011; \bold{43}(8):1-20. [freely available \href{http://www.ag-myresearch.com/2011_gasparrini_jss.html}{here}].

Gasparrini A, Scheipl F, Armstrong B, Kenward MG. A penalized framework for distributed lag non-linear models. \emph{Biometrics}. 2017; in press. [freely available here]

Gasparrini A. Modeling exposure-lag-response associations with distributed lag non-linear models. \emph{Statistics in Medicine}. 2014; \bold{33}(5):881-899. [freely available \href{http://www.ag-myresearch.com/2014_gasparrini_statmed.html}{here}]
  
Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234. [freely available \href{http://www.ag-myresearch.com/2010_gasparrini_statmed.html}{here}]
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>}

\note{
Missing values in \code{x} are allowed, but this causes the observation (for non-time series data with \code{x} as a matrix) or the following observations corresponding to the lag period (for time series data with \code{x} as a vector series) to be set to \code{NA}. Although correct, this could generate computational problems in the presence of a high number of missing observations.

The name of the crossbasis object will be used by \code{\link{crosspred}} in order to extract the related estimated parameters. If more than one variable is transformed through cross-basis functions in the same model, different names must be specified.

Before version 2.2.0 of \pkg{dlnm}, the \code{argvar} list could include a \code{cen} argument to be passed internally to \code{\link{onebasis}} for centering the basis. This step is now moved to the prediction stage, with a \code{cen} argument in \code{\link{crosspred}} or \code{\link{crossreduce}} (see the related help pages). For backward compatibility, the use of \code{cen} in \code{crossbasis} is still allowed (with a warning), but may be discontinued in future versions.
}

\section{Warnings}{
In previous versions of the package the function adopted a different usage. In particular, the \code{argvar} list should not include a \code{cen} argument any more (see Note in this help page or \code{\link{onebasis}}). Users are strongly suggested to comply with the current usage, as backward compatibility may be discontinued in future versions of the package.

Meaningless combinations of arguments in \code{argvar} and \code{arglag} passed to \code{\link{onebasis}} could lead to collinear variables, with identifiability problems in the model and the exclusion of some of them.

It is strongly recommended to avoid the inclusion of an intercept in the basis for \code{x} (\code{intercept} in \code{argvar} should be \code{FALSE}, as default), otherwise a rank-deficient cross-basis matrix will be specified, causing some of the cross-variables to be excluded in the regression model. Conversely, an intercept is included by default in the basis for the space of lags.
}

\seealso{
\code{\link{onebasis}} to generate one-dimensional basis matrices. The \code{\link[=smooth.construct.cb.smooth.spec]{cb smooth constructor}} for cross-basis penalized spline smooths. \code{\link{crosspred}} to obtain predictions after model fitting. The method function \code{\link[=plot.crosspred]{plot}} to plot several type of graphs.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
### example of application in time series analysis - see vignette("dlnmTS")

# create the crossbasis objects and summarize their contents
cb1.pm <- crossbasis(chicagoNMMAPS$pm10, lag=15, argvar=list(fun="lin"),
  arglag=list(fun="poly",degree=4))
cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag=3, argvar=list(df=5),
  arglag=list(fun="strata",breaks=1))
summary(cb1.pm)
summary(cb1.temp)

# run the model and get the predictions for pm10
library(splines)
model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
  family=quasipoisson(), chicagoNMMAPS)
pred1.pm <- crosspred(cb1.pm, model1, at=0:20, bylag=0.2, cumul=TRUE)

# plot the lag-response curves for specific and incremental cumulative effects
plot(pred1.pm, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
  main="Lag-response curve for a 10-unit increase in PM10")
plot(pred1.pm, "slices", var=10, col=2, cumul=TRUE, ylab="Cumulative RR",
  main="Lag-response curve of incremental cumulative effects")

### example of application beyond time series - see vignette("dlnmExtended")

# generate the matrix of exposure histories from the 5-year periods
Qnest <- t(apply(nested, 1, function(sub) exphist(rep(c(0,0,0,sub[5:14]), 
  each=5), sub["age"], lag=c(3,40))))

# define the cross-basis
cbnest <- crossbasis(Qnest, lag=c(3,40), argvar=list("bs",degree=2,df=3),
  arglag=list(fun="ns",knots=c(10,30),intercept=FALSE))
summary(cbnest)

# run the model and predict
library(survival)
mnest <- clogit(case~cbnest+strata(riskset), nested)
pnest <- crosspred(cbnest,mnest, cen=0, at=0:20*5)

# bi-dimensional exposure-lag-response association
plot(pnest, zlab="OR", xlab="Exposure", ylab="Lag (years)")
# lag-response curve for dose 60
plot(pnest, var=50, ylab="OR for exposure 50", xlab="Lag (years)", xlim=c(0,40))
# exposure-response curve for lag 10
plot(pnest, lag=5, ylab="OR at lag 5", xlab="Exposure", ylim=c(0.95,1.15))
}

\keyword{smooth}
\keyword{ts}

