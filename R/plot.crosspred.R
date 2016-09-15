###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
plot.crosspred <-
function(x, ptype, var=NULL, lag=NULL, ci="area", ci.arg,
  ci.level=x$ci.level, cumul=FALSE, exp=NULL, ...) {
#
################################################################################
#
  if(class(x)!="crosspred") stop("'x' must be of class 'crosspred'")
  ci <- match.arg(ci,c("area","bars","lines","n"))
#
  # SETTING DEFAULT FOR ptype: OVERALL FOR NO LAG, SLICES FOR VAR/LAG, OTHERWISE 3D
  if(missing(ptype)) {
    if(!is.null(var)||!is.null(lag)) {
      ptype <- "slices"
    }else if(diff(x$lag)==0) {
      ptype <- "overall"
    } else ptype <- "3d"
  }
  ptype <- match.arg(ptype,c("slices","3d","contour","overall"))
#
  if(is.null(var)&&is.null(lag)&&(ptype=="slices")) {
    stop("at least 'var' or 'lag' must be provided when ptype='slices'")
  }
  if(!is.null(var)&&!is.numeric(var)&&length(var)>4&&ptype=="slices") {
    stop("'var' and 'lag' must be numeric and of length <=4")
  }
  if(!is.null(lag)&&!is.numeric(lag)&&length(lag)>4&&ptype=="slices") {
    stop("'var' and 'lag' must be numeric and of length <=4")
  }
  if((!is.null(var)&!is.null(lag))&&length(var)!=length(lag)&&ptype=="slices") {
    stop("if both are provided, length of 'var' and 'lag' must be the same")
  }
  if(!is.null(var)&&sum(var%in%x$predvar)!=length(var)&&(ptype=="slices")) {
    stop("'var' must match values used for prediction")
  }
  if(!is.null(lag)&&sum(lag%in%seqlag(x$lag,x$bylag))!=length(lag)&&(ptype=="slices")) {
    stop("'lag' must match values used for prediction")
  }
  if(missing(ci.arg)) {
    ci.arg <- list()
  } else if(!is.list(ci.arg)) stop("'ci.arg' must be a list")
  if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0) {
    stop("'ci.level' must be numeric and between 0 and 1")
  }
  if(cumul==TRUE) {
    # SET THE LAG STEP EQUAL TO 1
    x$bylag <- 1
    if(is.null(x$cumfit)) {
      stop("Cumulative outcomes can be plotted if predicted in the 'crosspred'
  object. Set the argument 'cumul=TRUE' in the function crosspred()")
    }
  }
  if(!is.null(exp)&&!is.logical(exp)) stop("'exp' must be logical")
#
##########################################################################
# COMPUTE OUTCOMES
#
  # CUMULATIVE IF CUMUL==T
  if(cumul==TRUE) {
    x$matfit <- x$cumfit
    x$matse <- x$cumse
  }
  # SET THE Z LEVEL EQUAL TO THAT STORED IN OBJECT IF NOT PROVIDED
  z <- qnorm(1-(1-ci.level)/2)
  x$mathigh <- x$matfit+z*x$matse
  x$matlow <- x$matfit-z*x$matse
  x$allhigh <- x$allfit+z*x$allse
  x$alllow <- x$allfit-z*x$allse
  noeff <- 0
#
  # EXPONENTIAL
  if((is.null(exp)&&!is.null(x$model.link)&&x$model.link%in%c("log","logit"))||
    (!is.null(exp)&&exp==TRUE)) {
    x$matfit <- exp(x$matfit)
    x$mathigh <- exp(x$mathigh)
    x$matlow <- exp(x$matlow)
    x$allfit <- exp(x$allfit)
    x$allhigh <- exp(x$allhigh)
    x$alllow <- exp(x$alllow)
    noeff <- 1
  }
#
##########################################################################
# GRAPHS
#
##########
# SLICES
##########
#
  if(ptype=="slices") {
    # SET FRAME AND GREY SCALE
    mar.old <- par()$mar
    mfrow.old <- par()$mfrow
    mgp.old <- par()$mgp
    grey <- grey(0.9)
    if(length(lag)+length(var)>1) {
      layout(matrix(1:(length(var)+length(lag)),
        ncol=sum(!is.null(var),!is.null(lag))))
      grey <- grey(0.8)
      par(mgp=c(2,0.7,0),mar=c(4.1,4.1,2.1,1.1))
    }
    # LAG
    if(!is.null(lag)) {
      # START LOOP FOR LAG
      xlag <- paste("lag",lag,sep="")
      for(i in seq(along=lag)) {
        # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
        plot.arg <- list(type="l",xlab="Var",ylab="Outcome",
          ylim=c(min(x$matlow[,xlag]),max(x$mathigh[,xlag])),bty="l")
        if(length(lag)+length(var)>1)  plot.arg$cex.axis <- 0.7
        plot.arg <- modifyList(plot.arg,list(...))		
        # SET CONFIDENCE INTERVALS
        ci.list <- list(panel.first=call("fci",ci=ci,x=x$predvar,
          high=x$mathigh[,xlag[i]],low=x$matlow[,xlag[i]],ci.arg,plot.arg,
          noeff=noeff))
        plot.arg <- modifyList(plot.arg,c(ci.list,
          list(x=x$predvar,y=x$matfit[,xlag[i]])))
        if(length(lag)+length(var)>1) {
          plot.arg$main <- ""
          plot.arg$xlab <- "Var"
        }
        # PLOT
        do.call("plot",plot.arg)
        if(length(lag)>1) mtext(paste("Lag =",lag[i]),cex=0.8)
      }
    }
    # VAR
    if(!is.null(var)) {
      # START LOOP FOR VAR
      xvar <- as.character(var)
      for(i in seq(along=var)) {
        # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
        plot.arg <- list(type="l",xlab="Lag",ylab="Outcome",
          ylim=c(min(x$matlow[xvar,]),max(x$mathigh[xvar,])),bty="l")
        if(length(lag)+length(var)>1)  plot.arg$cex.axis <- 0.7
        plot.arg <- modifyList(plot.arg,list(...))		
        # SET CONFIDENCE INTERVALS
        ci.list <- list(panel.first=call("fci",ci=ci,x=seqlag(x$lag,x$bylag),
          high=x$mathigh[xvar[i],],low=x$matlow[xvar[i],],ci.arg,plot.arg,
          noeff=noeff))
        plot.arg <- modifyList(plot.arg,c(ci.list,
          list(x=seqlag(x$lag,x$bylag),y=x$matfit[xvar[i],])))
        if(length(lag)+length(var)>1) {
          plot.arg$main <- ""
          plot.arg$xlab <- "Lag"
        }
        # PLOT
        do.call("plot",plot.arg)
        if(length(lag)>1) mtext(paste("Var =",var[i]),cex=0.8)
      }
    }
    if(length(lag)+length(var)>1) {
      par(mar=mar.old,mfrow=mfrow.old,mgp=mgp.old)
    }
  }
#
##########
# OVERALL
##########
#
  if(ptype=="overall") {
    # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
    plot.arg <- list(type="l",ylim=c(min(x$alllow),max(x$allhigh)),
      xlab="Var",ylab="Outcome",bty="l")
    plot.arg <- modifyList(plot.arg,list(...))
    # SET CONFIDENCE INTERVALS
    ci.list <- list(panel.first=call("fci",ci=ci,x=x$predvar,
      high=x$allhigh,low=x$alllow,ci.arg,plot.arg,noeff=noeff))
    plot.arg <- modifyList(plot.arg,c(ci.list,
      list(x=x$predvar,y=x$allfit)))
    # PLOT
    do.call("plot",plot.arg)
  }
#
##############
# CONTOURPLOT
##############
#
  if(ptype=="contour") {
    if(x$lag[2]==0) stop("contour plot not conceivable for unlagged associations")
    # SET DEFAULT VALUES (NOT TO BE SPECIFIED BY THE USER)
    levels <- pretty(x$matfit,20)
    col1 <- colorRampPalette(c("blue","white"))
    col2 <- colorRampPalette(c("white","red"))
    col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
    filled.contour(x=x$predvar,y=seqlag(x$lag,x$bylag),z=x$matfit,col=col,
      levels=levels,...)
  }
#
#########
# 3-D
#########
#
  if(ptype=="3d") {
    if(diff(x$lag)==0) stop("3D plot not conceivable for unlagged associations")
    # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
    plot.arg <- list(ticktype="detailed",theta=210,phi=30,xlab="Var",
      ylab="Lag",	zlab="Outcome",col="lightskyblue",
      zlim=c(min(x$matfit),max(x$matfit)),ltheta=290,shade=0.75,r=sqrt(3),d=5)
    plot.arg <- modifyList(plot.arg,list(...))
    plot.arg <- modifyList(plot.arg,list(x=x$predvar,y=seqlag(x$lag,x$bylag),
      z=x$matfit))
    # PLOT
    do.call("persp",plot.arg)
  }
}
