###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#


#' Generate a Basis Matrix of Indicator Variables
#' 
#' The function generates a basis matrix including indicator variables defining
#' intervals (strata), through dummy parameterization. It is meant to be used
#' internally by \code{\link{onebasis}} and \code{\link{crossbasis}} and not
#' directly run by the users.
#' 
#' The strata are defined by right-open intervals specified through
#' \code{breaks}. If these are not provided, a number of intervals defined by
#' \code{df} is placed at equally-spaced quantiles. This step is performed
#' through an internal call to \code{\link[base]{cut}}.
#' 
#' The argument \code{ref} indentifies the reference category, specified by
#' excluding the related stratum in the dummy parameterization of the basis.
#' This defines control-treatment contrasts, where each interval is compared
#' with the baseline (see \code{\link[stats]{contrast}}). If set to 0 (when
#' \code{intercept=TRUE}), it provides a different parameterization, where each
#' interval has its own baseline.
#' 
#' If \code{intercept=TRUE}, an intercept is included in the model. The default
#' (when \code{ref} is different from 0) produces an additional variable with a
#' constant value of 1, representing the baseline.
#' 
#' @param x the predictor variable. Missing values are allowed.
#' @param df dimension of the basis, equal to the number of strata. They depend
#' on \code{breaks} if provided.
#' @param breaks internal cut-off points defining the strata as right-open
#' intervals. If provided, they determine \code{df}.
#' @param ref interval used as reference category. Default to the first
#' stratum. See Details below.
#' @param intercept logical. If \code{TRUE}, an intercept is included in the
#' basis matrix. See Details below.
#' @return A matrix object of class \code{"strata"}. It contains the attributes
#' \code{df}, \code{breaks}, \code{ref} and \code{intercept}, with values which
#' can be different than the arguments provided due to internal reset.
#' @note This function is mainly used internally thorugh \code{\link{onebasis}}
#' and \code{\link{crossbasis}} to create basis and cross-basis matrices,
#' respectively. It is not exported in the namespace to avoid conflicts with
#' the function with the same name in the package \pkg{survival}, and can be
#' accessed through the triple colon operator '\code{:::}' (see Examples
#' below).
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{onebasis}} to generate basis matrices and
#' \code{\link{crossbasis}} to generate cross-basis matrices.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @keywords smooth
#' @importFrom stats quantile
#' @examples
#' 
#' ### simple use (accessing non-exported function through ':::')
#' dlnm:::strata(1:5, breaks=3)
#' dlnm:::strata(1:5, df=3)
#' dlnm:::strata(1:5, df=3, intercept=TRUE)
#' dlnm:::strata(1:5, df=3, ref=2, intercept=TRUE)
#' 
#' ### use as an internal function in onebasis
#' b <- onebasis(chicagoNMMAPS$pm10, "strata", breaks=c(20,40))
#' summary(b)
#' model <- glm(death ~ b, family=quasipoisson(), chicagoNMMAPS)
#' pred <- crosspred(b, model, at=0:60)
#' plot(pred, xlab="PM10", ylab="RR", main="RR for PM10")
#' 
strata <-
function(x, df=1, breaks=NULL, ref=1, intercept=FALSE) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
  range <- range(x,na.rm=TRUE)
#
  # DEFINE breaks AND df IF NEEDED
  if(!is.null(breaks)) {
    breaks <- sort(unique(breaks))
  } else if(df-intercept>0) 
    breaks <- quantile(x,1/(df-intercept+1)*1:((df-intercept)),na.rm=TRUE)
  df <- length(breaks)+intercept
#
  # TRANSFORMATION
  xcat <- cut(x,c(range[1]-0.0001,breaks,range[2]+0.0001),right=FALSE)
  basis <- matrix(outer(xcat,levels(xcat),"==")+0,ncol=length(levels(xcat)))
#
  # DEFINE REFERENCE
  if(!ref%in%seq(0,ncol(basis)))
    stop("wrong value in 'ref' argument. See help('strata')")
  if(!intercept&&ref==0) ref <- 1
  if(!is.null(breaks)) {
    if(ref!=0) basis <- basis[,-ref,drop=FALSE]
    if(intercept&&ref!=0) basis <- cbind(1,basis)
  }
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(df=df,breaks=breaks,ref=ref,
    intercept=intercept))
#
  class(basis) <- c("strata","matrix")
#
  return(basis)
}
