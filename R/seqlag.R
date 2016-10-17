### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#' @describeIn internals used internally in several other functions to
#' create the vector with the sequence of lags given the range provided.
#' @param lag either an integer scalar or vector of length 2, defining the
#'   maximum lag or the lag range, respectively.
seqlag <- function(lag, by = 1) seq(from = lag[1], to = lag[2], by = by)

