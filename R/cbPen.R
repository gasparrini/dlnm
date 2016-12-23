### R routines for the R package dlnm (c) Antonio Gasparrini 2015-2016


#' Generate Penalty Matrices for a DLNM
#' 
#' This function generates penalty matrices for the two dimensions of predictor
#' and lags, given the functions selected to model the relationship in each
#' space. It can also be used for generating the single penalty matrix for the
#' predictor space of a uni-dimensional basis.
#' 
#' This function is used to perform penalized regression models using the
#' \emph{external} method. This involves generating the transformation using
#' \code{\link{onebasis}} or \code{\link{crossbasis}} with functions for
#' penalized splines (either \code{\link{ps}} or \code{\link{cr}}). The
#' function \code{cbPen} is then called to generate a list of the related
#' penalty matrices. The model is performed by penalizing so-called parametric
#' terms in the \code{\link[mgcv]{gam}} function of \pkg{mgcv}, by including
#' the basis or cross-basis matrix in the regression formula and the list of
#' penalty matrices in its \code{paraPen} argument.
#' 
#' When \code{cb} is a cross-basis object, the penalty matrices for the two
#' spaces of predictor and lags are expanded accordingly to its tensor
#' product-type structure. A penalty matrix is not defined when using a
#' function different than \code{\link{ps}} or \code{\link{cr}}, thus keeping
#' one of the two dimensions unpenalized.
#' 
#' Additional penalties on the lag dimension can be added through the argument
#' \code{addSlag}, either as a single matrix or a list of matrices. If provided
#' as a vector, this is taken as the diagonal of the penalty matrix and
#' expanded accordingly. These objects must have appropriate dimensions in
#' accordance with the basis matrix for the lag space.
#' 
#' All the penalty matrices are also appropriately rescaled to improve the
#' estimation process.
#' 
#' The vector \code{sp} must have the same length as the number of penalties,
#' including additional penalties on the lags, and it is replicated accordingly
#' if of length 1. Positive or zero elements are taken as fixed smoothing
#' parameters. Negative elements signal that these parameters need to be
#' estimated.
#' 
#' @param cb an object of class \code{'onebasis'} or \code{'crossbasis'}.
#' @param sp supplied smoothing parameters. See Details below.
#' @param addSlag matrix or vector (or list of matrices and/or vectors)
#' defining additional penalties on the lag structure. See Details below.
#' @return A list including penalty matrices plus two vectors \code{rank} and
#' \code{sp} defining their rank and the smoothing parameters. This list is
#' consistent with the argument \code{paraPen} in the regression function
#' \code{\link[mgcv]{gam}} function of \pkg{mgcv}.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{ps}} and \code{\link{cr}} for penalized spline
#' functions. The \code{\link[=smooth.construct.cb.smooth.spec]{smooth
#' constructor}} for cross-basis spline smooths.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @keywords utilities
#' @export
cbPen <- function(cb, sp = -1, addSlag = NULL) {
  # 
  if (all(class(cb) != "crossbasis") & all(class(cb) != "onebasis")) 
    stop("first argument must be object of class 'crossbasis' or 'onebasis")
  # ATTRIBUTES
  attr <- attributes(cb)
  # TRANSFORM ONEBASIS
  if (one <- any(class(cb) == "onebasis")) {
    ind <- match(names(formals(attr$fun)), names(attr), nomatch = 0)
    attr <- list(df = c(ncol(cb), 1), range = attr$range, lag = c(0, 0), argvar = c(attr[c("fun", "cen")], 
      attr[ind]), arglag = list(fun = "strata", df = 1, int = TRUE))
  }
  # DEFINE PENALTY TERMS
  ff <- c(attr$argvar$fun, attr$arglag$fun)
  fx <- c(!ff[1] %in% c("ps", "cr") || attr$argvar$fx, !ff[2] %in% c("ps", "cr") || attr$arglag$fx)
  Slist <- list()
  if (!fx[1]) 
    Slist <- c(Slist, list(Svar = attr$argvar$S %x% diag(attr$df[2])))
  if (!fx[2]) 
    Slist <- c(Slist, list(Slag = diag(attr$df[1]) %x% attr$arglag$S))
  # RESCALING
  Slist <- lapply(Slist, function(X) X/eigen(X, symmetric = TRUE, only.values = TRUE)$values[1])
  # ADDITIONAL PENALTIES ON LAG
  if (one & !is.null(addSlag)) 
    stop("penalties on lag not allowed for class 'onebasis")
  if (!is.null(addSlag)) 
    Slist <- c(Slist, mkaddSlag(addSlag, attr$df))
  # RANK
  rank <- sapply(Slist, findrank)
  # SMOOTHING PARAMETERS sp MUST BE NUMERIC AND CONSISTENT WITH NUMBER AND ORDER OF PENALTY TERMS
  npen <- length(Slist)
  if (npen == 0L) 
    stop("no penalization defined")
  if (length(sp) == 1L) 
    sp <- rep(sp, npen)
  if (!is.numeric(sp) || length(sp) != npen) 
    stop("'sp' must be numeric and consistent with number of penalty terms")
  names(sp) <- names(Slist)
  # 
  res <- c(Slist, list(rank = rank, sp = sp))
  return(res)
}
