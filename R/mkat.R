###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#
#' @describeIn internals used internally in \code{\link{crosspred}} to define
#'   the values used for predictions.
#' @param at either a numeric vector representing the values of a constant 
#'   exposure throughout the lag period defined by \code{lag}, or a matrix of 
#'   exposure histories over the same lag period used for estimation.
#' @param from min of predictor values used for prediction.
#' @param to max of predictor values used for prediction.
#' @param by increment of the sequences of predictor used for prediction.
#' @param bylag increment of the sequences of lag values used for prediction.
#' @param range range of values used for prediction.
#' @param lag either an integer scalar or vector of length 2, defining the
#'   maximum lag or the lag range, respectively.
mkat <-
function(at, from, to, by, range, lag, bylag) {
#
################################################################################
#
  # IF at IS NULL
  if(is.null(at)) {
    if(is.null(from)) from <- range[1]
    if(is.null(to)) to <- range[2]
    nobs <- ifelse(is.null(by),50,max(1,diff(range)/by))
    pretty <- pretty(c(from,to),n=nobs)
    pretty <- pretty[pretty>=from&pretty<=to]	
    at <- if(is.null(by)) pretty else seq(from=min(pretty),
      to=to,by=by)
  # IF at IS A MATRIX, CHECK AND NAME ROWS
  } else if(is.matrix(at)) {
    if(dim(at)[2]!=diff(lag)+1L)
      stop("matrix in 'at' must have ncol=diff(lag)+1")
    if(bylag!=1) stop("'bylag!=1 not allowed with 'at' in matrix form")
    if(is.null(rownames(at))) rownames(at) <- seq(nrow(at))
  # IF at IS A VECTOR
  } else at <- sort(unique(at))
#
  return(at)
}

