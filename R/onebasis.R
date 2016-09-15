###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
onebasis <-
function(x, fun="ns", ...) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
  range <- range(x,na.rm=TRUE)
  args <- list(...)
  args$x <- x
  cen <- args$cen
#
  # CHECK THE CONTENT AND OPTIONALLY MODIFY OBJECTS THROUGH assign
  checkonebasis(fun,args,cen)
#
###########################################################################
# TRANSFORMATION
#
  # CREATE THE BASIS
  basis <- do.call(fun,args)
  # FORCE TO BE A MATRIX (NOT WITH as.matrix AS IT DELETES ATTRIBUTES)
  if(is.null(dim(basis))) dim(basis) <- c(length(x),1)
  attr <- attributes(basis)
#
##########################################################################
#
  # NAMES AND ATTRIBUTES (KEEP cen IF PROVIDED, TO BE USED LATER FOR CENTERING)
  attributes(basis) <- c(list(fun=fun),attr,list(range=range,cen=cen))
  dimnames(basis) <- list(nx,paste("b",seq(ncol(basis)),sep=""))
#
  class(basis) <- c("onebasis","matrix")
#
  return(basis)
}
