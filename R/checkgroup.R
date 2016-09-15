###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
checkgroup <- 
function(group,x,basisvar,lag) {
#
################################################################################
#
  if(NCOL(x)>1L) stop("'group' allowed only for time series data")
  if(min(tapply(x,group,length))<=diff(lag))
    stop("each group must have length > diff(lag)")
}

