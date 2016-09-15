###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
lin <-
function(x, intercept=FALSE) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
#
  # TRANSFORMATION
  basis <- as.matrix(x)
  if(intercept) basis <- cbind(1,basis)
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(intercept=intercept))
#
  class(basis) <- c("lin","matrix")
#
  return(basis)
}
