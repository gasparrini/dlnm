###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
integer <-
function(x, values, intercept=FALSE) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
#
  # DEFINE LEVELS AND TRANSFORM INTO A FACTOR
  levels <- if(!missing(values)) values else sort(unique(x))
  xfac <- factor(x,levels=levels)
#
  # TRANSFORMATION
  basis <- as.matrix(outer(xfac,levels,"==")+0L)
#
  # IF INTERCEPT IS NOT REQUIRED, DROP THE FIRST COLUMN
  if(ncol(basis)>1L) {
    if(!intercept) basis <- basis[,-1L,drop=FALSE]
  } else intercept <- TRUE
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(values=levels,
    intercept=intercept))
#
  class(basis) <- c("integer","matrix")
#
  return(basis)
}
