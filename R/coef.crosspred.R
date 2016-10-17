###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#


#' Model Coefficients and their (Co)Variance Matrix of a DLNM
#' 
#' These method functions extract the estimated model coefficients and their
#' (co)variance matrix from a DLNM from objects of class \code{"crosspred"} and
#' \code{"crossreduce"}.
#' 
#' 
#' @aliases coef.crosspred coef.crossreduce vcov.crosspred vcov.crossreduce
#' @param object an object of class \code{"crosspred"} or \code{"crossreduce"}.
#' @param \dots further arguments passed to or from other methods.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso See \code{\link{dlnm-package}} for an introduction to the package
#' and for links to package vignettes providing more detailed information.
#' @keywords methods
coef.crosspred <-
function(object, ...) return(object$coef)
