\name{crosspred}
\alias{crosspred}
\alias{summary.crosspred}

\title{ Generate Predictions for a DLNM }

\description{
The function generates predictions from a distributed lag non-linear model (DLNM). These are interpreted as estimated associations defined on a grid of values of the original predictor and lags, computed versus a reference predictor value.
}

\usage{
crosspred(basis, model=NULL, coef=NULL, vcov=NULL, model.link=NULL, at=NULL,
  from=NULL, to=NULL, by=NULL, lag, bylag=1, cen=NULL, ci.level=0.95,
  cumul=FALSE)

\method{summary}{crosspred}(object, ...)
}

\arguments{
  \item{basis }{ an object of class \code{"onebasis"} or \code{"crossbasis"}.}
  \item{model }{ a model object for which the prediction is desired. See Details below.}
  \item{coef, vcov, model.link }{ user-provided coefficients, (co)variance matrix and model link for the prediction. See Details below.}
  \item{at }{ either a numeric vector representing the values of a constant exposure throughout the lag period defined by \code{lag}, or a matrix of exposure histories over the same lag period used for estimation.}
  \item{from, to}{ range of predictor values used for prediction.}
  \item{lag }{ either an integer scalar or vector of length 2, defining the lag range used for prediction. Defalut to values used for estimation.}
  \item{by, bylag}{ increment of the sequences of predictor and lag values used for prediction.}
  \item{cen }{ logical or a numeric scalar. It specifies the centering value, then used as a reference for predictions. See Details below.}
  \item{ci.level }{ confidence level for the computation of confidence intervals.}
  \item{cumul }{ logical. If \code{TRUE}, incremental cumulative associations along lags are also predicted. See Details.}
  \item{object }{ an object of class \code{"crosspred"}.}
  \item{\dots }{ additional arguments to be passed to \code{summary}.}
}

\details{
\code{model} is the model object including \code{basis} in its formula. The object \code{basis} must be the same representing the basis or cross-basis matrix included in \code{model}, preserving its attributes and class.

The function computes predictions for specific combinations of predictor and lag values, and the net overall predictions accounting for the whole lag period. By default, predictor values are set internally as approximately 50 equally-spaced points within the range, or alternatively directly defined through \code{at} or \code{from}/\code{to}/\code{by}. Lag values are are set by default at all the integer values within the lag period, or determined by \code{lag} and \code{bylag}.

The values in \code{at} can be provided as a vector, and in this case they are replicated for each lag. As an alternative usage, \code{at} can be provided as a matrix of complete exposure histories over the same lag period used for estimation, in order to compute the association with a specific exposure pattern (see also \code{\link{exphist}}).

Predictions are computed versus a reference value, with default values dependent on the function used in \code{basis}, or manually set through \code{cen}. Briefly, sensible default values are automatically defined for \code{\link{strata}}, \code{\link{thr}} and \code{\link{integer}} (corresponding to the reference region), and for \code{\link{lin}} (corresponding to 0). For other choices, such as \code{\link{ns}}, \code{\link{bs}}, \code{\link{poly}} or other existing or user-defined functions, the centering value is set by default to the mid-range. The inclusion of the intercept in \code{basis} term nullifies the centering.

Exponentiated predictions are included if \code{model.link} is equal to \code{log} or \code{logit}. Confidence intervals computed using a normal approximation and a confidence level of \code{ci.level}. \code{model.link} is automatically selected from \code{model} for some classes, but needs to be provided for different classes. Matrices with incremental cumulative predicted associations along lags at each values used for prediction are included if \code{cumul=TRUE}.

The function automatically works with model objects from regression function \code{\link{lm}} and \code{\link{glm}}, \code{\link[mgcv]{gam}} (package \pkg{mgcv}), \code{\link[survival]{coxph}} and \code{\link[survival]{clogit}} (package \pkg{survival}), \code{\link[nlme]{lme}} and \code{\link[nlme]{nlme}} (package \pkg{nlme}), \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}} and \code{\link[lme4]{nlmer}} (package \pkg{lme4}), \code{\link[gee]{gee}} (package \pkg{gee}), \code{\link[geepack]{geeglm}} (package \pkg{geepack}). The function also works with any regression function for which \code{\link{coef}} and \code{\link{vcov}} methods are available. Otherwise, the user needs to input the coefficients and associated (co)variance matrix related to the parameters of the crossbasis as arguments \code{coef} and \code{vcov}, and information on the link function in \code{model.link}. In this case, dimensions and order must match the variables included in \code{basis}. 

The function can be used to compute predictions for models with simple basis functions not including lag, computed with \code{\link{onebasis}}. In this case, only unlagged predicted associations are returned.
}

\value{
A list object of class \code{"crosspred"} with the following (optional) components:
  \item{predvar }{ vector or matrix of values used for prediction, depending on the format of the argument \code{at} (see Details above).}
  \item{cen }{ (optional) numeric scalar defining the centering value.}
  \item{lag }{ integer vector defining the lag range used for prediction.}
  \item{bylag }{ increment of the sequence of lag values.}
  \item{coefficients, vcov }{ coefficients and their variance-covariance matrix.}
  \item{matfit, matse }{ matrices of predictions and standard errors at the chosen combinations of predictor and lag values.}
  \item{matlow, mathigh }{ matrices of confidence intervals for \code{matfit}.}
  \item{allfit, allse }{ vectors of the overall cumulative predicted association and standard errors.}
  \item{alllow, allhigh }{ vectors of confidence intervals for \code{allfit}.}
  \item{cumfit, cumse }{ matrices of incremental cumulative predicted associations along lags and related standard errors at the chosen combinations of predictor and lag values. Computed if \code{cumul=TRUE}.}
  \item{cumlow, cumhigh }{ matrices of confidence intervals for \code{cumfit}. Computed if \code{cumul=TRUE}.}
  \item{matRRfit }{ matrix of exponentiated specific associations from \code{matfit}.}
  \item{matRRlow, matRRhigh }{ matrices of confidence intervals for \code{matRRfit}.}
  \item{allRRfit }{ vector of exponentiated overall cumulative associations from \code{allfit}.}
  \item{allRRlow, allRRhigh }{ vectors of confidence intervals for \code{allRRfit}.}
  \item{cumRRfit }{ matrix of exponentiated incremental cumulative associations from \code{cumfit}. Computed if \code{cumul=TRUE}.}
  \item{cumRRlow, cumRRhigh }{ matrix of confidence intervals for . Computed if \code{cumul=TRUE}.}
\item{ci.level }{ confidence level used for the computation of confidence intervals for \code{cumRRfit}.}
  \item{model.class }{ class of the model command used for estimation.}
  \item{model.link }{ a specification for the model link function.}
The function \code{\link{summary.crosspred}} returns a summary of the list.
}

\references{
Gasparrini A. Distributed lag linear and non-linear models in R: the package dlnm. \emph{Journal of Statistical Software}. 2011; \bold{43}(8):1-20. [freely available \href{http://www.ag-myresearch.com/jss2011.html}{here}].

Gasparrini A. Modeling exposure-lag-response associations with distributed lag non-linear models. \emph{Statistics in Medicine}. 2014; \bold{33}(5):881-899. [freely available \href{http://www.ag-myresearch.com/statmed2014}{here}]
  
Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234. [freely available \href{http://www.ag-myresearch.com/statmed2010}{here}]
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>}

\note{
All the predictions are generated using a reference value, which if not directly specific by \code{cen} is given default values corresponding to the mid-range point for continuous functions. Before version 2.2.0 of \pkg{dlnm}, centering was produced in \code{\link{onebasis}} or \code{\link{crossbasis}} (see the related help pages), and for backward compatibility this information is kept (with a warning) and used in \code{crosspred} unless \code{cen} is directly defined as an argument.

Exponentiated predictions are included if \code{model.link} (specified automatically by \code{model} or selected by the user) is equal to \code{log} or \code{logit}.
}

\section{Warnings}{
In case of collinear variables in the \code{basis} object, some of them are discarded and the related parameters not included in \code{model}. Then, \code{crosspred} will return an error. Check that the specification of the variables is meaningful through \code{\link{summary.crossbasis}} or \code{\link{summary.onebasis}}.

The name of the object \code{basis}  will be used to extract the related estimated parameters from \code{model}. If more than one variable is transformed by cross-basis functions in the same model, different names must be specified.
}

\seealso{
\code{\link{onebasis}} to generate one-dimensional basis matrices. \code{\link{crossbasis}} to generate cross-basis matrices. \code{\link{crossreduce}} to reduce the fit to one dimension. 
The method function \code{\link[=plot.crosspred]{plot}} to plot several type of graphs.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
### default usage - see vignette("dlnmTS")

# seasonal analysis: select summer months only
chicagoNMMAPSseas <- subset(chicagoNMMAPS, month>5 & month<10)

# create the crossbasis objects, including info on groups
cb2.o3 <- crossbasis(chicagoNMMAPSseas$o3, lag=5,
  argvar=list(fun="thr",thr=40.3), arglag=list(fun="integer"),
  group=chicagoNMMAPSseas$year)
cb2.temp <- crossbasis(chicagoNMMAPSseas$temp, lag=10,
  argvar=list(fun="thr",thr=c(15,25)), arglag=list(fun="strata",breaks=c(2,6)),
  group=chicagoNMMAPSseas$year)
summary(cb2.o3)
summary(cb2.temp)

# run the model
library(splines)
model2 <- glm(death ~  cb2.o3 + cb2.temp + ns(doy, 4) + ns(time,3) + dow,
  family=quasipoisson(), chicagoNMMAPSseas)
  
# get the predictions for o3 at specific exposure values
pred2.o3 <- crosspred(cb2.o3, model2, at=c(0:65,40.3,50.3))

# get figures for the overall cumulative association, with ci
pred2.o3$allRRfit["50.3"]
cbind(pred2.o3$allRRlow, pred2.o3$allRRhigh)["50.3",]

# plot the estimated lag-response curve (with 80%CI)
plot(pred2.o3, "slices", var=50.3, ci="bars", type="p", col=2, pch=19,
  ci.level=0.80, main="Lag-response a 10-unit increase above threshold (80CI)")
# plot the estimated overall cumulative exposure-response curve
plot(pred2.o3,"overall",xlab="Ozone", ci="l", col=3, ylim=c(0.9,1.3), lwd=2,
  ci.arg=list(col=1,lty=3), main="Overall cumulative association for 5 lags")
  
# plot the estimated exposure-lag-response surface
plot(pred2.o3, xlab="Ozone", main="3D: default perspective")
plot(pred2.o3, xlab="Ozone", main="3D: different perspective", theta=250, phi=40)

### extended usage - see vignette("dlnmExtended")

# generate the matrix of exposure histories from the weekly data
Qdrug <- as.matrix(drug[,rep(7:4, each=7)])
colnames(Qdrug) <- paste("lag", 0:27, sep="")

# define the decay function
fdecay <- function(x, scale=5, ...) {
  basis <- exp(-x/scale)
  attributes(basis)$scale <- scale
  return(basis)
}

# define the cross-basis
cbdrug2 <- crossbasis(Qdrug, lag=27, argvar=list("lin"),
  arglag=list(fun="fdecay",scale=6))
summary(cbdrug2)

# run the model and predict
mdrug2 <- lm(out~cbdrug2+sex, drug)
pdrug2 <- crosspred(cbdrug2, mdrug2, at=0:20*5)

# dose 20 for 10 days
histdrug <- exphist(rep(20,10), time=10, lag=27)
pdrug4 <- crosspred(cbdrug2, mdrug2, at=histdrug)
with(pdrug4,c(allfit,alllow,allhigh))

# define exposure profile with weekly exposures to 10, 50, 0 and 20
expdrug <- rep(c(10,50,0,20),c(2,1,1,2)*7)

# define the exposure histories for all the time points
dynhist <- exphist(expdrug, lag=27)

# predict the effects
pdyndrug <- crosspred(cbdrug2, mdrug2, at=dynhist)

# plot of the evolution of the effects along time given the doses
plot(pdyndrug,"overall", ylab="Effect", xlab="Time (days)", ylim=c(-5,27), 
  xlim=c(1,50))
}

\keyword{smooth}
\keyword{ts}
