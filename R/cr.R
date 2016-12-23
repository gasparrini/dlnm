### R routines for the R package dlnm (c) Antonio Gasparrini 2015-2016


#' Generate a Basis Matrix for Penalized Cubic Regression Splines
#' 
#' Generate the basis matrix for cubic regression splines with penalties on the
#' second derivatives.
#' 
#' The function has a usage similar to \code{\link{bs}} and \code{\link{ns}} in
#' the \pkg{splines} package. It produces spline transformations, however using
#' a \emph{cardinal} parameterization that represents the splines in terms of
#' values at the knots. A penalty matrix is also defined. The same results are
#' returned by the related \code{\link[=smooth.construct.cr.smooth.spec]{smooth
#' constructor}} in the package \pkg{mgcv}, which is in fact called internally.
#' 
#' The argument \code{knots} defines a vector of knots within the range of the
#' predictor \code{x}, by default at equally-spaced quantiles. The penalization
#' is defined on the second derivative of the function through a penalty matrix
#' \code{S}.
#' 
#' Similarly to \code{\link{bs}} and \code{\link{ns}}, setting
#' \code{intercept=FALSE} (default) determines the exclusion of the first
#' transformed variables, and the corresponding first row and column in
#' \code{S}, thus avoiding identifiability issues during the model fitting.
#' Note how the procedure of imposing identifiability constraints is different
#' from that adopted by \code{\link{smoothCon}} in the package \pkg{mgcv},
#' where a more complex reparameterization is produced.
#' 
#' @param x the predictor variable. Missing values are allowed.
#' @param df degrees of freedom, basically the dimension of the basis matrix.
#' If supplied in the absence of \code{knots}, it automatically selects
#' \code{df+1-intercept} knots at equally-spaced quantiles of \code{x}. The
#' minimum allowed is \code{df=3}.
#' @param knots breakpoints that define the spline. These are generally
#' automatically selected, and not defined by the user. See Details below.
#' @param intercept logical. If \code{TRUE}, an intercept is included in the
#' basis matrix. See Details below.
#' @param fx logical. If \code{TRUE}, it removes the penalization. See Details
#' below.
#' @param S penalty matrix, usually internally defined if \code{NULL}
#' (default).
#' @return A matrix object of class \code{'cr'}. It contains the attributes
#' \code{df}, \code{knots}, \code{intercept}, \code{fx}, and \code{S}, with
#' values which can be different than the arguments provided due to internal
#' reset.
#' @note The function is primarily added here to specify penalized DLMs and
#' DLNMs using the so-called \emph{external} method, \emph{i.e.} by including
#' the penalty matrix in the argument \code{paraPen} of the
#' \code{\link[mgcv]{gam}} regression function in \pkg{mgcv} (see
#' \code{\link{cbPen}}). However, this approach can be also used to fit
#' standard unidimensional penalized cubic spline models as an alternative to
#' the use of specific \code{\link[=smooth.construct.cr.smooth.spec]{smooth
#' constructor}}, as it takes advantage of the use of prediction and plotting
#' functions in \pkg{dlnm}.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>, with
#' internall calls to functions included in the package \pkg{mgcv} by Simon N.
#' Wood.
#' @seealso \code{\link{ps}} for P-splines. \code{\link{bs}} and
#' \code{\link{ns}} for B-splines and natural cubic splines, respectively.
#' \code{\link{cbPen}} for defining tensor-type bi-dimensional penalties in
#' DLNMs. The related \code{\link[=smooth.construct.cr.smooth.spec]{smooth
#' constructor}} for cubic regression spline smooths in \pkg{mgcv}. The
#' \code{\link[=smooth.construct.cb.smooth.spec]{smooth constructor}} for
#' cross-basis spline smooths.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @references Gasparrini A. A penalized framework for distributed lag
#' non-linear models. \emph{Biometrics}. 2016; in press.
#' 
#' Wood S. N. Generalized Additive Models: An Introduction with R. Chapman and
#' Hall/CRC Press, 2006.
#' @keywords smooth
#' @export
#' @importFrom stats quantile
#' @importFrom mgcv s smooth.construct.cr.smooth.spec
cr <- function(x, df = 10, knots = NULL, intercept = FALSE, fx = FALSE, S = NULL) {
  # 
  nx <- names(x)
  x <- as.vector(x)
  nax <- is.na(x)
  if (nas <- any(nax)) 
    x <- x[!nax]
  # DEFINE KNOTS AND DF
  if (is.null(knots)) {
    if (df < 3) 
      stop("'df' must be >=3")
    knots <- quantile(unique(x), seq(0, 1, length = df + (!intercept)))
  } else df <- length(knots) - (!intercept)
  # CHECK NUMBER OF UNIQUE x VALUES (ADD SOME IF NEEDED TO PREVENT ERROR IN
  # smooth.construct.cr.smooth.spec)
  if (add <- length(unique(x)) < length(knots)) 
    x <- c(seq(min(knots), max(knots), length = length(knots)), x)
  # TRANSFORMATION: CALL FUNCTION FROM MGCV
  oo <- smooth.construct.cr.smooth.spec(s(x, bs = "cr", k = df + (!intercept)), data = list(x = x), knots = list(x = knots))
  basis <- oo$X
  if (!intercept) 
    basis <- basis[, -1L, drop = FALSE]
  # REMOVE ADDED VALUES AND RE-INSERT MISSING
  if (add) 
    basis <- basis[-seq(length(knots)), , drop = FALSE]
  if (nas) {
    nmat <- matrix(NA, length(nax), ncol(basis))
    nmat[!nax, ] <- basis
    basis <- nmat
  }
  # RELATED PENALTY MATRIX
  if (fx) {
    S <- NULL
  } else if (is.null(S)) {
    S <- oo$S[[1]]
    S <- (S + t(S))/2
    if (!intercept) 
      S <- S[-1L, -1L, drop = FALSE]
  } else if (any(dim(S) != ncol(basis))) 
    stop("dimensions of 'S' not compatible")
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx, seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis), list(df = df, knots = knots, intercept = intercept, fx = fx, 
    S = S))
  # 
  class(basis) <- c("cr", "matrix")
  # 
  return(basis)
}
