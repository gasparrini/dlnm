###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#


#' Define Knots for Lag Space at Equally-Spaced Log-Values
#' 
#' This function defines the position of knot or cut-off values at
#' equally-spaced log-values for spline or strata functions, respectively. It
#' is expressely created for lag-response functions to set the knots or
#' cut-offs placements accordingly with the default of versions of \pkg{dlnm}
#' earlier than 2.0.0.
#' 
#' This functions has been included for consistency with versions of \pkg{dlnm}
#' earlier than 2.0.0, where the default knots or cut-off placements in the lag
#' space for functions \code{ns}, \code{bs} and \code{strata} used to be at
#' equally-spaced values in the log scale. Since version 2.0.0 on, the default
#' is equally-spaced quantiles, similarly to functions defined for the space of
#' predictor. This function can be used to replicate the results obtained with
#' old versions.
#' 
#' The argument \code{x} is usually assumed to represent the maximum lag (if a
#' scalar) or the lag range (if a vector of length 2). Otherwise is interpreted
#' as a vector variable for which the range is computed internally.
#' 
#' The number of knots is set with the argument \code{nk}, or otherwise
#' determined by the choice of function and number of degrees of freedom
#' through the arguments \code{fun} and \code{df}. Specifically, the number of
#' knots is set to \code{df-1-intercept} for \code{"ns"},
#' \code{df-degree-intercept} for \code{"bs"}, or \code{df-intercept} for
#' \code{"strata"}.
#' 
#' An intercept is included by default (\code{intercept=TRUE}), consistently
#' with the default for the lag space.
#' 
#' @param x an integer scalar or vector of length 2, defining the the maximum
#' lag or the lag range, respectively, or a vector variable.
#' @param nk number of knots or cut-offs.
#' @param fun character scalar with the name of the function for which the
#' knots or cut-offs must be created. See Details below.
#' @param df degree of freedom.
#' @param degree degree of the piecewise polynomial. Only for \code{fun="bs"}.
#' @param intercept logical. If an intercept is included in the basis function.
#' @return A numeric vector of knot or cut-off values, to be used in the
#' \code{arglag} list argument of \code{\link{crossbasis}} for reproducing the
#' default of versions of \pkg{dlnm} earlier than 2.0.0.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{equalknots}} for placing the knots at equally-spaced
#' values. \code{\link{crossbasis}} to generate cross-basis matrices.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @keywords smooth
#' @examples
#' 
#' ### setting 3 knots for lag 0-20
#' logknots(20, 3)
#' logknots(c(0,20), 3)
#' 
#' ### setting knots and cut-offs for different functions
#' logknots(20, fun="ns", df=4)
#' logknots(20, fun="bs", df=4, degree=2)
#' logknots(20, fun="strata", df=4)
#' 
#' ### with and without without intercept
#' logknots(20, fun="ns", df=4)
#' logknots(20, fun="ns", df=4, intercept=FALSE)
#' 
#' ### replicating an old example in time series analysis
#' lagknots <- logknots(30, 3)
#' cb <- crossbasis(chicagoNMMAPS$temp, lag=30, argvar=list(fun="bs",df=5,
#'   degree=2), arglag=list(knots=lagknots))
#' summary(cb)
#' library(splines)
#' model <- glm(death ~  cb + ns(time, 7*14) + dow, 
#'   family=quasipoisson(), chicagoNMMAPS)
#' pred <- crosspred(cb, model, cen=21, by=1)
#' plot(pred, xlab="Temperature", col="red", zlab="RR", shade=0.6,
#'   main="3D graph of temperature effect")
#' 
logknots <-
function(x, nk=NULL, fun="ns", df=1, degree=3, intercept=TRUE) {
#
################################################################################
#
  x <- as.vector(x)
#
  # IF LENGTH OF x 1 OR 2, INTERPRETED AS A LAG RANGE, OTHERWISE TAKE THE RANGE
  range <- if(length(x)<3L) mklag(x) else range(x,na.rm=TRUE) 
  if(diff(range)==0) stop("range must be >0")
#
  # CHOOSE NUMBER OF KNOTS IF NOT PROVIDED
  if(is.null(nk)) {
    fun <- match.arg(fun,c("ns","bs","strata"))
    nk <- switch(fun,"ns"=df-1-intercept,"bs"=df-degree-intercept,
      "strata"=df-intercept)
  }
#
  # DEFINE KNOTS AT EQUALLY-SPACED LOG-VALUES ALONG lag
  if(nk<1) stop("choice of arguments defines no knots")
  knots <- range[1] + exp(((1+log(diff(range)))/(nk+1))*seq(nk)-1)
#
  return(knots)
}
