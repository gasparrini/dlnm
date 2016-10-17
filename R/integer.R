###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#


#' Generate a Basis Matrix of Indicator Variables for Integer Values
#' 
#' The function generates a basis matrix including indicator variables defining
#' intervals for integer values. It is meant to be used internally by
#' \code{\link{onebasis}} and \code{\link{crossbasis}} and not directly run by
#' the users.
#' 
#' The function returns indicator variables for intervals defined by the
#' integer values within the range of \code{x}. It is expressly created to
#' specify an unconstrained function in the space of lags for distributed lag
#' linear or non-linear models, and probably of no use beyond that.
#' 
#' The argument \code{intercept} determines the presence of an intercept. If
#' \code{FALSE}, the interval corresponding to the first value in \code{values}
#' is excluded, and the parameterization is indentical to dummy variables with
#' the first group as a reference.
#' 
#' @param x the predictor variable. Missing values are allowed.
#' @param values the values for which the indicator variables should be
#' computed. Used internally, usually to be left as missing.
#' @param intercept logical. If \code{TRUE}, an intercept is included in the
#' basis matrix. See Details below.
#' @return A matrix object of class \code{"integer"}. It contains the
#' attributes \code{values} and \code{intercept}.
#' @note This function is mainly used internally thorugh \code{\link{onebasis}}
#' to create basis matrices. It is not exported in the namespace to avoid
#' conflicts with the function with the same name in the package \pkg{base},
#' and can be accessed through the triple colon operator '\code{:::}' (see
#' Examples below).
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{onebasis}} to generate basis matrices and
#' \code{\link{crossbasis}} to generate cross-basis matrices.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @keywords smooth
#' @examples
#' 
#' ### simple use (accessing non-exported function through ':::')
#' dlnm:::integer(1:5)
#' dlnm:::integer(1:5, intercept=TRUE)
#' 
integer <-
function(x, values, intercept=FALSE) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
#
  # DEFINE LEVELS AND TRANSFORM INTO A FACTOR
  levels <- if(!missing(values)) values else sort(unique(x))
  xfac <- factor(x,levels=levels)
#
  # TRANSFORMATION
  basis <- as.matrix(outer(xfac,levels,"==")+0L)
#
  # IF INTERCEPT IS NOT REQUIRED, DROP THE FIRST COLUMN
  if(ncol(basis)>1L) {
    if(!intercept) basis <- basis[,-1L,drop=FALSE]
  } else intercept <- TRUE
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(values=levels,
    intercept=intercept))
#
  class(basis) <- c("integer","matrix")
#
  return(basis)
}
