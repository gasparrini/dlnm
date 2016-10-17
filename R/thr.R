### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016


#' Generate a Basis Matrix of Linear Threshold Transformations
#' 
#' The function generates a basis matrix including transformed variables
#' through high, low or double linear threshold parameterization. It is meant
#' to be used internally by \code{\link{onebasis}} and \code{\link{crossbasis}}
#' and not directly run by the users.
#' 
#' A linear threshold function defines a linear relationship beyond a specific
#' threshold. A high linear threshold defines a linear increase above the
#' threshold, while a low linear threshold defines a linear increase below. A
#' double linear threshold includes both of them.
#' 
#' The argument \code{thr.value} is placed at the median if not provided. If
#' \code{side} is not provided, the default is \code{side='h'} when
#' \code{thr.value} is a scalar, \code{side='d'} otherwise. Only the minimum
#' (for \code{side='h'} and \code{side='l'}) and minimum and maximum values
#' (for \code{side='d'}) of \code{thr.value} are considered.
#' 
#' If \code{intercept=TRUE}, an intercept is included in the model, namely an
#' additional variable with a constant value of 1.
#' 
#' @param x the predictor variable. Missing values are allowed.
#' @param thr.value numeric scalar or vector defining the threshold value(s).
#' @param side type of threshold parameterization: \code{'l'} for low,
#' \code{'h'} for high, \code{'d'} for double. See Details below.
#' @param intercept logical. If \code{TRUE}, an intercept is included in the
#' basis matrix. See Details below.
#' @return A matrix object of class \code{'thr'}. It contains the attributes
#' \code{thr.value}, \code{side} and \code{intercept}, with values which can be
#' different than the arguments provided due to internal reset.
#' @note This function is mainly used internally thorugh \code{\link{onebasis}}
#' and \code{\link{crossbasis}} to create basis and cross-basis matrices,
#' respectively. It is not exported in the namespace, and can be accessed
#' through the triple colon operator '\code{:::}' (see Examples below).
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{onebasis}} to generate basis matrices and
#' \code{\link{crossbasis}} to generate cross-basis matrices.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @keywords smooth
#' @importFrom stats median
#' @examples
#' 
#' ### simple use (accessing non-exported function through ':::')
#' dlnm:::thr(1:5, thr=3)
#' dlnm:::thr(1:5, side='d')
#' dlnm:::thr(1:5, side='d', intercept=TRUE)
#' 
#' ### use as an internal function in onebasis
#' b <- onebasis(chicagoNMMAPS$pm10, 'thr', thr.value=20)
#' summary(b)
#' model <- glm(death ~ b, family=quasipoisson(), chicagoNMMAPS)
#' pred <- crosspred(b, model, at=0:60)
#' plot(pred, xlab='PM10', ylab='RR', main='RR for PM10')
#' 
thr <- function(x, thr.value = NULL, side = NULL, intercept = FALSE) {
  # 
  nx <- names(x)
  x <- as.vector(x)
  # DEFINE DEFAULT VALUES
  if (is.null(thr.value)) 
    thr.value <- median(x, na.rm = FALSE) else thr.value <- sort(thr.value)
  if (is.null(side)) 
    side <- ifelse(length(thr.value) > 1, "d", "h")
  thr.value <- if (side == "d") 
    thr.value[c(1, length(thr.value))] else thr.value[1]
  side <- match.arg(side, c("h", "l", "d"))
  # TRANSFORMATION
  basis <- switch(side, h = as.matrix(pmax(x - thr.value, 0)), l = as.matrix(-pmin(x - thr.value, 0)), 
    d = cbind(-pmin(x - thr.value[1], 0), pmax(x - thr.value[2], 0)))
  if (intercept) 
    basis <- cbind(1, basis)
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx, seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis), list(thr.value = thr.value, side = side, intercept = intercept))
  # 
  class(basis) <- c("thr", "matrix")
  # 
  return(basis)
}
