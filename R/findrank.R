###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#' @describeIn internals \code{findrank} returns the rank of a matrix.
#' @param X a matrix.
findrank <- function(X) {
  ev <- eigen(X,symmetric=TRUE,only.values=TRUE)$values
  rank <- sum(ev>max(ev)*.Machine$double.eps*10)
  return(rank)
}
