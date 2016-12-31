###
### R routines for the R package dlnm (c) Antonio Gasparrini 2015-2017
#
ps <-
function(x, df=10, knots=NULL, degree=3, intercept=FALSE, fx= FALSE,
  S=NULL, diff=2) {
#
################################################################################
#
  nx <- names(x)
  x <- as.vector(x)
  range <- range(x,na.rm=TRUE)
  nax <- is.na(x)
  if(nas <- any(nax)) x <- x[!nax]
  if((degree <- as.integer(degree)) < 1) stop("'degree' must be integer >= 1")
#
  # DEFINE KNOTS AND DF
  if(is.null(knots) || length(knots)==2L) {
    nik <- df-degree+2-intercept
    if(nik<=1) stop("basis dimension too small for b-spline degree")
    xl <- (if(length(knots)==2L) min(knots) else min(x)) - diff(range)*0.001
    xu <- (if(length(knots)==2L) max(knots) else max(x)) + diff(range)*0.001
    dx <- (xu-xl)/(nik-1)
    knots <- seq(xl-dx*degree, xu+dx*degree, length=nik+2*degree)
  } else {
    df <- length(knots)-degree-2+intercept
    if(df-degree<=1) stop("basis dimension too small for b-spline degree")
  }
  if(any(x<knots[degree+1] | knots[length(knots)-degree]<x))
    warning('all obs expected within inner df-degree+int knots')
#
  # TRANSFORMATION
  basis <- spline.des(knots,x,degree+1,x*0,TRUE)$design
  if(!intercept) basis <- basis[,-1L,drop=FALSE]
#
  # RE-INSERT MISSING
  if(nas) {
    nmat <- matrix(NA,length(nax),ncol(basis))
    nmat[!nax,] <- basis
    basis <- nmat
  }
#
  # RELATED PENALTY MATRIX
  if(diff<1L) stop("'diff' must be an integer >=1")
  if(fx) {
    S <- NULL
  } else if(is.null(S)) {
    S <- crossprod(diff(diag(ncol(basis)+!intercept),diff=diff))
    S <- (S+t(S))/2
    if(!intercept) S <- S[-1L,-1L,drop=FALSE]
  } else if(any(dim(S)!=ncol(basis))) stop("dimensions of 'S' not compatible")
#
  # NAMES AND ATTRIBUTES
  dimnames(basis) <- list(nx,seq(ncol(basis)))
  attributes(basis) <- c(attributes(basis),list(df=df,knots=knots,degree=degree,
    intercept=intercept,fx=fx,S=S,diff=diff))
#
  class(basis) <- c("ps","matrix")
#
  return(basis)
}
