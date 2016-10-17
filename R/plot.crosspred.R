###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
#' Plot Predictions for a DLNM
#' 
#' High and low-level method functions for graphs (3d, contour, slices and
#' overall) of predictions from distributed lag non-linear models (DLNM).
#' 
#' Different plots can be obtained by choosing the following values for the
#' argument \code{ptype}:
#' 
#' \bold{\code{"3d"}}: a 3-D plot of predicted associations on the grid of
#' predictor-lag values. Additional graphical arguments can be included, such
#' as \code{theta}-\code{phi} (perspective), \code{border}-\code{shade}
#' (surface), \code{xlab}-\code{ylab}-\code{zlab} (axis labelling) or
#' \code{col}. See \code{\link[graphics]{persp}} for additional information.
#' 
#' \bold{\code{"contour"}}: a contour/level plot of predicted associations on
#' the grid of predictor-lag values.  Additional graphical arguments can be
#' included, such as \code{plot.title}-\code{plot.axes}-\code{key.title} for
#' titles and axis and key labelling. Arguments \code{x}-\code{y}-\code{z} and
#' \code{col}-\code{level} are automatically set and cannot be specified by the
#' user. See \code{\link[graphics]{filled.contour}} for additional information.
#' 
#' \bold{\code{"overall"}}: a plot of the overall cumulative exposure-response
#' associations over the whole lag period. See
#' \code{\link[graphics]{plot.default}}, \code{\link[graphics]{lines}} and
#' \code{\link[graphics]{points}} for information on additional graphical
#' arguments.
#' 
#' \bold{\code{"slices"}}: a (optionally multi-panel) plot of exposure-response
#' association(s) for specific lag value(s), and/or lag-response association(s)
#' for specific predictor value(s). Predictor and lag values are chosen by
#' \code{var} and \code{lag}, respectively. See
#' \code{\link[graphics]{plot.default}}, \code{\link[graphics]{lines}} and
#' \code{\link[graphics]{points}} for information on additional graphical
#' arguments.
#' 
#' The method function \code{plot} calls the high-level functions listed above
#' for each \code{ptype}, while \code{lines}-\code{points} add lines or points
#' for \code{ptype} equal to \code{"overall"} or \code{"slices"}. These methods
#' allow a great flexibility in the choice of graphical parameters, specified
#' through arguments of the original plotting functions. Some arguments, if not
#' specified, are set to different default values than the original functions.
#' 
#' Confidence intervals are plotted for \code{ptype} equal to \code{"overall"}
#' or \code{"slices"}. Their type is determined by \code{ci}, with options
#' \code{"area"} (default for \code{plot}), \code{"bars"}, \code{"lines"} or
#' \code{"n"} (no confidence intervals, default for \code{points} and
#' \code{lines}). Their appearance may be modified through \code{ci.arg}, a
#' list of arguments passed to to low-level plotting functions:
#' \code{\link[graphics]{polygon}} for \code{"area"},
#' \code{\link[graphics]{segments}} for \code{"bars"} and
#' \code{\link[graphics]{lines}} for \code{"lines"}. See the original functions
#' for a complete list of the arguments. This option offers flexibility in the
#' choice of confidence intervals display. As above, some unspecified arguments
#' are set to different default values.
#' 
#' For \code{ptype="slices"}, up to 4 plots for each dimension of predictor and
#' lags are allowed in \code{plot}, while for \code{lines}-\code{points} a
#' single plot in one of the two dimension must be chosen. Incremental
#' cumulative associations along lags are reported if \code{cumul=TRUE}: in
#' this case, the same option must have been set to obtain the prediction saved
#' in the \code{crosspred} object (see \code{\link{crosspred}}).
#' 
#' For a detailed illustration of the use of the functions, see:
#' 
#' \code{vignette("dlnmOverview")}
#' 
#' @aliases plot.crosspred lines.crosspred points.crosspred crossplot
#' @param x an object of class \code{"crosspred"}.
#' @param ptype type of plot. Default to \code{"3d"} for lagged relationship,
#' otherwise \code{"overall"}. See Details below.
#' @param var,lag vectors (for \code{plot}) or numeric scalars (for
#' \code{lines}-\code{points}) of predictor or lag values at which specific
#' associations must be plotted. Used only if \code{ptype="slices"}.
#' @param ci type of confidence intervals representation: one of \code{"area"},
#' \code{"bars"}, \code{"lines"} or \code{"n"}. Default to \code{"area"} in
#' high level functions, \code{"n"} for low-level functions.
#' @param ci.arg list of arguments to be passed to low-level plotting functions
#' to draw the confidence intervals. See Details.
#' @param ci.level confidence level for the computation of confidence
#' intervals.
#' @param cumul logical. If \code{TRUE}, incremental cumulative associations
#' along lags are plotted. Used only if \code{type="slices"}. See Details.
#' @param exp logical. It forces the choice about the exponentiation. See
#' Details.
#' @param \dots optional graphical arguments. See Details.
#' @note All the predictions are plotted using a reference value corresponding
#' to the centering point for continuous functions or different values for the
#' other functions (see the related help pages). This is determined by the
#' argument \code{cen} in \code{\link{crosspred}}. Exponentiated predictions
#' are returned by default if \code{x$model.link} is equal to \code{log} or
#' \code{logit}.
#' 
#' These methods for class \code{"crosspred"} have replaced the old function
#' \code{crossplot} since version 1.3.0.
#' @section Warnings: The values in \code{var} and \code{lag} must match those
#' specified in the object \code{crosspred} (see \code{\link{crosspred}}).
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{crossbasis}} to generate cross-basis matrices.
#' \code{\link{crosspred}} to obtain predictions after model fitting.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @references Gasparrini A. Distributed lag linear and non-linear models in R:
#' the package dlnm. \emph{Journal of Statistical Software}. 2011;
#' \bold{43}(8):1-20, freely available at
#' \url{http://www.ag-myresearch.com/jss2011}.
#' 
#' Gasparrini A. Modeling exposure-lag-response associations with distributed
#' lag non-linear models. \emph{Statistics in Medicine}. 2014;
#' \bold{33}(5):881-899, freely available at
#' \url{http://www.ag-myresearch.com/statmed2014}.
#' 
#' Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear
#' models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234,
#' freely available at \url{http://www.ag-myresearch.com/statmed2010}.
#' @keywords hplot aplot
#' @importFrom graphics par layout mtext filled.contour
#' @importFrom grDevices colorRampPalette
#' @examples
#' 
#' ### example of application in time series analysis - see vignette("dlnmTS")
#' 
#' # create the crossbasis object for pm10
#' cb3.pm <- crossbasis(chicagoNMMAPS$pm10, lag=1, argvar=list(fun="lin"),
#'   arglag=list(fun="strata"))
#'   
#' # create the crossbasis object for temperature
#' varknots <- equalknots(chicagoNMMAPS$temp,fun="bs",df=5,degree=2)
#' lagknots <- logknots(30, 3)
#' cb3.temp <- crossbasis(chicagoNMMAPS$temp, lag=30, argvar=list(fun="bs",
#'   knots=varknots), arglag=list(knots=lagknots))
#'   
#' # summarize
#' summary(cb3.pm)
#' summary(cb3.temp)
#' 
#' # run the model and get the predictions for temperature
#' library(splines)
#' model3 <- glm(death ~  cb3.pm + cb3.temp + ns(time, 7*14) + dow,
#'   family=quasipoisson(), chicagoNMMAPS)
#' pred3.temp <- crosspred(cb3.temp, model3, cen=21, by=1)
#' 
#' # 3-D and contour plots
#' plot(pred3.temp, xlab="Temperature", zlab="RR", theta=200, phi=40, lphi=30,
#'   main="3D graph of temperature effect")
#' plot(pred3.temp, "contour", xlab="Temperature", key.title=title("RR"),
#'   plot.title=title("Contour plot",xlab="Temperature",ylab="Lag"))
#' 
#' # lag-response curves specific to different temperature values
#' plot(pred3.temp, "slices", var=-20, ci="n", col=1, ylim=c(0.95,1.25), lwd=1.5,
#'   main="Lag-response curves for different temperatures, ref. 21C")
#' for(i in 1:3) lines(pred3.temp, "slices", var=c(0,27,33)[i], col=i+1, lwd=1.5)
#' legend("topright",paste("Temperature =",c(-20,0,27,33)), col=1:4, lwd=1.5)
#' 
#' # in one plot
#' plot(pred3.temp, "slices", var=c(-20,0,27,33), lag=c(0,5,15,28), col=4,
#' 	ci.arg=list(density=40,col=grey(0.7)))
#' 
#' ### example of application beyond time series - see vignette("dlnmExtended")
#' 
#' # generate the matrix of exposure histories from the 5-year periods
#' Qnest <- t(apply(nested, 1, function(sub) exphist(rep(c(0,0,0,sub[5:14]), 
#'   each=5), sub["age"], lag=c(3,40))))
#' 
#' # define the cross-basis
#' cbnest <- crossbasis(Qnest, lag=c(3,40), argvar=list("bs",degree=2,df=3),
#'   arglag=list(fun="ns",knots=c(10,30),intercept=FALSE))
#' summary(cbnest)
#' 
#' # run the model and predict
#' library(survival)
#' mnest <- clogit(case~cbnest+strata(riskset), nested)
#' pnest <- crosspred(cbnest,mnest, at=0:20*5, cen=0)
#' 
#' # bi-dimensional exposure-lag-response association
#' plot(pnest, zlab="OR", xlab="Exposure", ylab="Lag (years)")
#' # lag-response curve for dose 60
#' plot(pnest, var=50, ylab="OR for exposure 50", xlab="Lag (years)", xlim=c(0,40))
#' # exposure-response curve for lag 10
#' plot(pnest, lag=5, ylab="OR at lag 5", xlab="Exposure", ylim=c(0.95,1.15))
#' 
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
