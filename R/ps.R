### R routines for the R package dlnm (c) Antonio Gasparrini 2015-2016


#' Generate a Basis Matrix for P-Splines
#' 
#' Generate the basis matrix for P-splines, namely a B-spline basis with
#' difference penalties.
#' 
#' The function has a usage similar to \code{\link{bs}} and \code{\link{ns}} in
#' the \pkg{splines} package. It produces B-spline transformations through a
#' call to \code{\link{spline.des}}, plus a difference matrix to define
#' penalties. The same results are returned by the related
#' \code{\link[=smooth.construct.ps.smooth.spec]{smooth constructor}} in the
#' package \pkg{mgcv}.
#' 
#' The argument \code{knots} defines a vector of knots or (if of length 2) the
#' lower and upper limits between which the splines can be evaluated. However,
#' knots should be usually left automatically selected, and in particular these
#' P-splines only have sense with equally-spaced knots, due to the nature of
#' the penalization. It is important to highlight that, differently from
#' \code{\link{bs}} where \emph{internal} and \emph{boundary} knots are
#' defined, this function adopts a standard B-spline parameterization,
#' including by default \code{2*(degree+1)} knots beyond the range of the
#' variable.
#' 
#' The penalization is defined on the difference of adjacent coefficients
#' during fitting procedure through a penalty matrix \code{S}. The argument
#' \code{diff} selects the order difference (with the default 2 determining a
#' second order difference, and 0 producing a ridge penalty), while setting
#' \code{fx=TRUE} removes the penalization.
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
#' \code{df+degree+2-intercept} equally-spaced knots (within and beyond the
#' range of \code{x}). The minimum \code{df} allowed is
#' \code{degree+1-intercept}.
#' @param knots breakpoints that define the spline. These are generally
#' automatically selected, and not defined by the user. See Details below.
#' @param degree degree of the piecewise polynomial. Default is 3 for cubic
#' splines.
#' @param intercept logical. If \code{TRUE}, an intercept is included in the
#' basis matrix. See Details below.
#' @param fx logical. If \code{TRUE}, it removes the penalization. See Details
#' below.
#' @param S penalty matrix, usually internally defined if \code{NULL}
#' (default).
#' @param diff order difference of the penalty.
#' @return A matrix object of class \code{'ps'}. It contains the attributes
#' \code{df}, \code{knots}, \code{degree}, \code{intercept}, \code{fx},
#' \code{S}, and \code{diff}, with values which can be different than the
#' arguments provided due to internal reset.
#' @note The function is primarily added here to specify penalized DLMs and
#' DLNMs using the so-called \emph{external} method, \emph{i.e.} by including
#' the penalty matrix in the argument \code{paraPen} of the
#' \code{\link[mgcv]{gam}} regression function in \pkg{mgcv} (see
#' \code{\link{cbPen}}). However, this approach can be also used to fit
#' standard unidimensional P-spline models as an alternative to the use of
#' specific \code{\link[=smooth.construct.ps.smooth.spec]{smooth constructor}},
#' as it takes advantage of the use of prediction and plotting functions in
#' \pkg{dlnm}.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>,
#' adapting code available from functions included in the package \pkg{mgcv} by
#' Simon N. Wood.
#' @seealso \code{\link{cr}} for penalized cubic regression splines.
#' \code{\link{bs}} and \code{\link{ns}} for B-splines and natural cubic
#' splines, respectively. \code{\link{cbPen}} for defining tensor-type
#' bi-dimensional penalties in DLNMs. The related
#' \code{\link[=smooth.construct.ps.smooth.spec]{smooth constructor}} for
#' P-spline smooths in \pkg{mgcv}. The
#' \code{\link[=smooth.construct.cb.smooth.spec]{smooth constructor}} for
#' cross-basis spline smooths.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @references Gasparrini A. A penalized framework for distributed lag
#' non-linear models. \emph{Biometrics}. 2016; in press.
#' 
#' Eilers P. H. C. and Marx B. D. Flexible smoothing with B-splines and
#' penalties. \emph{Statistical Science}. 1996; \bold{11}(2):89-121.
#' 
#' Wood S. N. Generalized Additive Models: An Introduction with R. Chapman and
#' Hall/CRC Press, 2006.
#' @keywords smooth
#' @importFrom splines spline.des
#' @export
ps <- function(x, df = 10, knots = NULL, degree = 3, intercept = FALSE, fx = FALSE, S = NULL, diff = 2) {
  # 
  nx <- names(x)
  x <- as.vector(x)
  range <- range(x, na.rm = TRUE)
  nax <- is.na(x)
  if (nas <- any(nax)) 
    x <- x[!nax]
  if ((degree <- as.integer(degree)) < 1) 
    stop("'degree' must be integer >= 1")
  # DEFINE KNOTS AND DF
  if (is.null(knots) || length(knots) == 2L) {
    nik <- df - degree + 2 - intercept
    if (nik <= 1) 
      stop("basis dimension too small for b-spline degree")
    xl <- (if (length(knots) == 2L) 
      min(knots) else min(x)) - diff(range) * 0.001
    xu <- (if (length(knots) == 2L) 
      max(knots) else max(x)) + diff(range) * 0.001
    dx <- (xu - xl)/(nik - 1)
    knots <- seq(xl - dx * degree, xu + dx * degree, length = nik + 2 * degree)
  } else {
    df <- length(knots) - degree - 2 + intercept
    if (df - degree <= 1) 
      stop("basis dimension too small for b-spline degree")
  }
  if (any(x < knots[degree + 1] | knots[length(knots) - degree] < x)) 
    warning("all obs expected within inner df-degree+int knots")
  # TRANSFORMATION
  basis <- spline.des(knots, x, degree + 1, x * 0, TRUE)$design
  if (!intercept) 
    basis <- basis[, -1L, drop = FALSE]
  # RE-INSERT MISSING
  if (nas) {
    nmat <- matrix(NA, length(nax), ncol(basis))
    nmat[!nax, ] <- basis
    basis <- nmat
  }
  # RELATED PENALTY MATRIX
  if (diff < 1L) 
    stop("'diff' must be an integer >=1")
  if (fx) {
    S <- NULL
  } else if (is.null(S)) {
    S <- crossprod(diff(diag(ncol(basis) + (!intercept)), diff = diff))
    S <- (S + t(S))/2
    if (!intercept) 
      S <- S[-1L, -1L, drop = FALSE]
  } else if (any(dim(S) != ncol(basis))) 
    stop("dimensions of 'S' not compatible")
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx, seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis), list(df = df, knots = knots, degree = degree, intercept = intercept, 
    fx = fx, S = S, diff = diff))
  # 
  class(basis) <- c("ps", "matrix")
  # 
  return(basis)
}
