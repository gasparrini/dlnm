###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
logknots <-
function(x, nk=NULL, fun="ns", df=1, degree=3, intercept=TRUE) {
#
################################################################################
#
  x <- as.vector(x)
#
  # IF LENGTH OF x 1 OR 2, INTERPRETED AS A LAG RANGE, OTHERWISE TAKE THE RANGE
  range <- if(length(x)<3L) mklag(x) else range(x,na.rm=TRUE) 
  if(diff(range)==0) stop("range must be >0")
#
  # CHOOSE NUMBER OF KNOTS IF NOT PROVIDED
  if(is.null(nk)) {
    fun <- match.arg(fun,c("ns","bs","strata"))
    nk <- switch(fun,"ns"=df-1-intercept,"bs"=df-degree-intercept,
      "strata"=df-intercept)
  }
#
  # DEFINE KNOTS AT EQUALLY-SPACED LOG-VALUES ALONG lag
  if(nk<1) stop("choice of arguments defines no knots")
  knots <- range[1] + exp(((1+log(diff(range)))/(nk+1))*seq(nk)-1)
#
  return(knots)
}
