###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
plot.crossreduce <-
function(x, ci="area", ci.arg, ci.level=x$ci.level, exp=NULL, ...) {
#
################################################################################
#
  if(class(x)!="crossreduce") stop("'x' must be of class 'crossreduce'")
  ci <- match.arg(ci,c("area","bars","lines","n"))
#
  if(missing(ci.arg)) {
    ci.arg <- list()
  } else if(!is.list(ci.arg)) stop("'ci.arg' must be a list")
  if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0) {
    stop("'ci.level' must be numeric and between 0 and 1")
  }
  if(!is.null(exp)&&!is.logical(exp)) stop("'exp' must be logical")
#
##########################################################################
# COMPUTE OUTCOMES
#
  # SET THE Z LEVEL EQUAL TO THAT STORED IN OBJECT IF NOT PROVIDED
  z <- qnorm(1-(1-ci.level)/2)
  x$high <- x$fit+z*x$se
  x$low <- x$fit-z*x$se
  noeff <- 0 
  # EXPONENTIAL
  if((is.null(exp)&&!is.null(x$model.link)&&x$model.link%in%c("log","logit"))||
    (!is.null(exp)&&exp==TRUE)) {
    x$fit <- exp(x$fit)
    x$high <- exp(x$high)
    x$low <- exp(x$low)
    noeff <- 1
  }
#
##########################################################################
# GRAPH
#
  if(x$type=="var") {
    xvar <- seqlag(x$lag,x$bylag)
    xlab <- "Lag"
  } else {
    xvar <- x$predvar
    xlab <- "Var"
  }
#
  # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
  plot.arg <- list(type="l",ylim=c(min(x$low),max(x$high)),
    xlab=xlab,ylab="Outcome",bty="l")
  plot.arg <- modifyList(plot.arg,list(...))
  # SET CONFIDENCE INTERVALS
  ci.list <- list(panel.first=call("fci",ci=ci,x=xvar,
    high=x$high,low=x$low,ci.arg,plot.arg,noeff=noeff))
  plot.arg <- modifyList(plot.arg,c(ci.list,list(x=xvar,y=x$fit)))
  # PLOT
  do.call("plot",plot.arg)
}
