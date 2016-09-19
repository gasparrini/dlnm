###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#
findrank <- function(X) {
#
################################################################################
#
  ev <- eigen(X,symmetric=TRUE,only.values=TRUE)$values
  rank <- sum(ev>max(ev)*.Machine$double.eps*10)
#  
  return(rank)
}
