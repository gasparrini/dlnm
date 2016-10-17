### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016


#' Define Knots at Equally-Spaced Values
#' 
#' This function defines the position of knot or cut-off values at
#' equally-spaced values for spline or strata functions, respectively.
#' 
#' The number of knots is set with the argument \code{nk}, or otherwise
#' determined by the choice of function and number of degrees of freedom
#' through the arguments \code{fun} and \code{df}. Specifically, the number of
#' knots is set to \code{df-1-intercept} for \code{'ns'},
#' \code{df-degree-intercept} for \code{'bs'}, or \code{df-intercept} for
#' \code{'strata'}.
#' 
#' @param x a vector variable.
#' @param nk number of knots or cut-offs.
#' @param fun character scalar with the name of the function for which the
#' knots or cut-offs must be created. See Details below.
#' @param df degree of freedom.
#' @param degree degree of the piecewise polynomial. Only for \code{fun='bs'}.
#' @param intercept logical. If an intercept is included in the basis function.
#' @return A numeric vector of knot or cut-off values.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{logknots}} for placing the knots at equally-spaced log
#' values. \code{\link{crossbasis}} to generate cross-basis matrices.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @keywords smooth
#' @export
#' @examples
#' 
#' ### setting 3 knots for range 0-20
#' equalknots(20, 3)
#' 
#' ### setting knots and cut-offs for different functions
#' equalknots(20, fun='ns', df=4)
#' equalknots(20, fun='bs', df=4, degree=2)
#' equalknots(20, fun='strata', df=4)
#' 
#' ### with and without without intercept
#' equalknots(20, fun='ns', df=4)
#' equalknots(20, fun='ns', df=4, intercept=TRUE)
#' 
equalknots <- function(x, nk = NULL, fun = "ns", df = 1, degree = 3, intercept = FALSE) {
  # 
  x <- as.vector(x)
  # 
  range <- range(x, na.rm = TRUE)
  # CHOOSE NUMBER OF KNOTS IF NOT PROVIDED
  if (is.null(nk)) {
    fun <- match.arg(fun, c("ns", "bs", "strata"))
    nk <- switch(fun, ns = df - 1 - intercept, bs = df - degree - intercept, strata = df - intercept)
  }
  # DEFINE KNOTS AT EQUALLY-SPACED VALUES ALONG range
  if (nk < 1) 
    stop("choice of arguments defines no knots")
  knots <- range[1] + (diff(range)/(nk + 1)) * seq(nk)
  # 
  return(knots)
}
