###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
mklag <- 
function(lag) {
#
################################################################################
#
  #  lag MUST BE A POSITIVE INTEGER VECTOR 
  if(any(!is.numeric(lag))||length(lag)>2) 
    stop("'lag' must a integer vector or length 2 or 1")
  if(length(lag)==1L) lag <- if(lag<0L) c(lag,0L) else c(0L,lag)
  if(diff(lag)<0L) stop("lag[1] must be <= lag[2]")
  return(round(lag[1L:2L]))
}

