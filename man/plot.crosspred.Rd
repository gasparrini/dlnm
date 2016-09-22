\name{plot.crosspred}
\alias{plot.crosspred}
\alias{lines.crosspred}
\alias{points.crosspred}
\alias{crossplot}

\title{ Plot Predictions for a DLNM }

\description{
High and low-level method functions for graphs (3d, contour, slices and overall) of predictions from distributed lag non-linear models (DLNM).
}

\usage{
\method{plot}{crosspred}(x, ptype, var=NULL, lag=NULL, ci="area", ci.arg,
  ci.level=x$ci.level, cumul=FALSE, exp=NULL, ...)

\method{lines}{crosspred}(x, ptype, var=NULL, lag=NULL, ci="n", ci.arg,
  ci.level=x$ci.level, cumul=FALSE, exp=NULL, ...)

\method{points}{crosspred}(x, ptype, var=NULL, lag=NULL, ci="n", ci.arg,
  ci.level=x$ci.level, cumul=FALSE, exp=NULL, ...)
}

\arguments{
  \item{x }{ an object of class \code{"crosspred"}.}
  \item{ptype }{ type of plot. Default to \code{"3d"} for lagged relationship, otherwise \code{"overall"}. See Details below.}
   \item{var, lag }{ vectors (for \code{plot}) or numeric scalars (for \code{lines}-\code{points}) of predictor or lag values at which specific associations must be plotted. Used only if \code{ptype="slices"}.}
  \item{ci }{ type of confidence intervals representation: one of \code{"area"}, \code{"bars"}, \code{"lines"} or \code{"n"}. Default to \code{"area"} in high level functions, \code{"n"} for low-level functions.}
  \item{ci.arg }{ list of arguments to be passed to low-level plotting functions to draw the confidence intervals. See Details.}
  \item{ci.level }{ confidence level for the computation of confidence intervals.}
  \item{cumul }{ logical. If \code{TRUE}, incremental cumulative associations along lags are plotted. Used only if \code{type="slices"}. See Details.}
  \item{exp }{ logical. It forces the choice about the exponentiation. See Details.}
  \item{\dots }{ optional graphical arguments. See Details.}
}

\details{
Different plots can be obtained by choosing the following values for the argument \code{ptype}:

\bold{\code{"3d"}}: a 3-D plot of predicted associations on the grid of predictor-lag values. Additional graphical arguments can be included, such as \code{theta}-\code{phi} (perspective), \code{border}-\code{shade} (surface), \code{xlab}-\code{ylab}-\code{zlab} (axis labelling) or \code{col}. See \code{\link[graphics]{persp}} for additional information.

\bold{\code{"contour"}}: a contour/level plot of predicted associations on the grid of predictor-lag values.  Additional graphical arguments can be included, such as \code{plot.title}-\code{plot.axes}-\code{key.title} for titles and axis and key labelling. Arguments \code{x}-\code{y}-\code{z} and \code{col}-\code{level} are automatically set and cannot be specified by the user. See \code{\link[graphics]{filled.contour}} for additional information.

\bold{\code{"overall"}}: a plot of the overall cumulative exposure-response associations over the whole lag period. See \code{\link[graphics]{plot.default}}, \code{\link[graphics]{lines}} and \code{\link[graphics]{points}} for information on additional graphical arguments.

\bold{\code{"slices"}}: a (optionally multi-panel) plot of exposure-response association(s) for specific lag value(s), and/or lag-response association(s) for specific predictor value(s). Predictor and lag values are chosen by \code{var} and \code{lag}, respectively. See \code{\link[graphics]{plot.default}}, \code{\link[graphics]{lines}} and \code{\link[graphics]{points}} for information on additional graphical arguments.

The method function \code{plot} calls the high-level functions listed above for each \code{ptype}, while \code{lines}-\code{points} add lines or points for \code{ptype} equal to \code{"overall"} or \code{"slices"}. These methods allow a great flexibility in the choice of graphical parameters, specified through arguments of the original plotting functions. Some arguments, if not specified, are set to different default values than the original functions.

Confidence intervals are plotted for \code{ptype} equal to \code{"overall"} or \code{"slices"}. Their type is determined  by \code{ci}, with options \code{"area"} (default for \code{plot}), \code{"bars"}, \code{"lines"} or \code{"n"} (no confidence intervals, default for \code{points} and \code{lines}). Their appearance may be modified through \code{ci.arg}, a list of arguments passed to to low-level plotting functions: \code{\link[graphics]{polygon}} for \code{"area"}, \code{\link[graphics]{segments}} for \code{"bars"} and \code{\link[graphics]{lines}} for \code{"lines"}. See the original functions for a complete list of the arguments. This option offers flexibility in the choice of confidence intervals display. As above, some unspecified arguments are set to different default values.

For \code{ptype="slices"}, up to 4 plots for each dimension of predictor and lags are allowed in \code{plot}, while for \code{lines}-\code{points} a single plot in one of the two dimension must be chosen. Incremental cumulative associations along lags are reported if \code{cumul=TRUE}: in this case, the same option must have been set to obtain the prediction saved in the \code{crosspred} object (see \code{\link{crosspred}}). 

For a detailed illustration of the use of the functions, see:

\code{vignette("dlnmOverview")}
}

\references{
Gasparrini A. Distributed lag linear and non-linear models in R: the package dlnm. \emph{Journal of Statistical Software}. 2011; \bold{43}(8):1-20. [freely available \href{http://www.ag-myresearch.com/jss2011}{here}].

Gasparrini A. Modeling exposure-lag-response associations with distributed lag non-linear models. \emph{Statistics in Medicine}. 2014; \bold{33}(5):881-899. [freely available \href{http://www.ag-myresearch.com/statmed2014}{here}]
  
Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234. [freely available \href{http://www.ag-myresearch.com/statmed2010}{here}]
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>}

\note{
All the predictions are plotted using a reference value corresponding to the centering point for continuous functions or different values for the other functions (see the related help pages). This is determined by the argument \code{cen} in \code{\link{crosspred}}. Exponentiated predictions are returned by default if \code{x$model.link} is equal to \code{log} or \code{logit}.

These methods for class \code{"crosspred"} have replaced the old function \code{crossplot} since version 1.3.0.
}

\section{Warnings}{
The values in \code{var} and \code{lag} must match those specified in the object \code{crosspred} (see \code{\link{crosspred}}).
}

\seealso{
\code{\link{crossbasis}} to generate cross-basis matrices. \code{\link{crosspred}} to obtain predictions after model fitting.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
### example of application in time series analysis - see vignette("dlnmTS")

# create the crossbasis object for pm10
cb3.pm <- crossbasis(chicagoNMMAPS$pm10, lag=1, argvar=list(fun="lin"),
  arglag=list(fun="strata"))
  
# create the crossbasis object for temperature
varknots <- equalknots(chicagoNMMAPS$temp,fun="bs",df=5,degree=2)
lagknots <- logknots(30, 3)
cb3.temp <- crossbasis(chicagoNMMAPS$temp, lag=30, argvar=list(fun="bs",
  knots=varknots), arglag=list(knots=lagknots))
  
# summarize
summary(cb3.pm)
summary(cb3.temp)

# run the model and get the predictions for temperature
library(splines)
model3 <- glm(death ~  cb3.pm + cb3.temp + ns(time, 7*14) + dow,
  family=quasipoisson(), chicagoNMMAPS)
pred3.temp <- crosspred(cb3.temp, model3, cen=21, by=1)

# 3-D and contour plots
plot(pred3.temp, xlab="Temperature", zlab="RR", theta=200, phi=40, lphi=30,
  main="3D graph of temperature effect")
plot(pred3.temp, "contour", xlab="Temperature", key.title=title("RR"),
  plot.title=title("Contour plot",xlab="Temperature",ylab="Lag"))

# lag-response curves specific to different temperature values
plot(pred3.temp, "slices", var=-20, ci="n", col=1, ylim=c(0.95,1.25), lwd=1.5,
  main="Lag-response curves for different temperatures, ref. 21C")
for(i in 1:3) lines(pred3.temp, "slices", var=c(0,27,33)[i], col=i+1, lwd=1.5)
legend("topright",paste("Temperature =",c(-20,0,27,33)), col=1:4, lwd=1.5)

# in one plot
plot(pred3.temp, "slices", var=c(-20,0,27,33), lag=c(0,5,15,28), col=4,
	ci.arg=list(density=40,col=grey(0.7)))

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
pnest <- crosspred(cbnest,mnest, at=0:20*5, cen=0)

# bi-dimensional exposure-lag-response association
plot(pnest, zlab="OR", xlab="Exposure", ylab="Lag (years)")
# lag-response curve for dose 60
plot(pnest, var=50, ylab="OR for exposure 50", xlab="Lag (years)", xlim=c(0,40))
# exposure-response curve for lag 10
plot(pnest, lag=5, ylab="OR at lag 5", xlab="Exposure", ylim=c(0.95,1.15))
}

\keyword{hplot}
\keyword{aplot}
