###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
thr <-
function(x, thr.value=NULL, side=NULL, intercept=FALSE) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
#
  # DEFINE DEFAULT VALUES
  if(is.null(thr.value)) thr.value <- median(x,na.rm=FALSE) else 
    thr.value <- sort(thr.value)
  if(is.null(side)) side <- ifelse(length(thr.value)>1,"d","h")
  thr.value <- if(side=="d") thr.value[c(1,length(thr.value))] else thr.value[1]
  side <- match.arg(side,c("h","l","d"))
#
  # TRANSFORMATION
  basis <- switch(side,
    h = as.matrix(pmax(x-thr.value,0)),
    l = as.matrix(-pmin(x-thr.value,0)),
    d = cbind(-pmin(x-thr.value[1],0),pmax(x-thr.value[2],0)))
  if(intercept) basis <- cbind(1,basis)
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(thr.value=thr.value,
    side=side,intercept=intercept))
#
  class(basis) <- c("thr","matrix")
#
  return(basis)
}
