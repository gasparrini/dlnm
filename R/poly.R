###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#
poly <-
function(x, degree=1, scale, intercept=FALSE) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
#
  # TRANSFORMATION
  if(missing(scale)) scale <- max(abs(x),na.rm=TRUE)
  basis <- outer(x/scale,(1-intercept):(degree),"^")
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(degree=degree,scale=scale,
    intercept=intercept))
#
  class(basis) <- c("poly","matrix")
#
  return(basis)
}
