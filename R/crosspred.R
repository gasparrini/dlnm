###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#


#' Generate Predictions for a DLNM
#' 
#' The function generates predictions from a distributed lag non-linear model
#' (DLNM). These are interpreted as estimated associations defined on a grid of
#' values of the original predictor and lags, computed versus a reference
#' predictor value.
#' 
#' \code{model} is the model object including \code{basis} in its formula. The
#' object \code{basis} must be the same representing the basis or cross-basis
#' matrix included in \code{model}, preserving its attributes and class.
#' 
#' The function computes predictions for specific combinations of predictor and
#' lag values, and the net overall predictions accounting for the whole lag
#' period. By default, predictor values are set internally as approximately 50
#' equally-spaced points within the range, or alternatively directly defined
#' through \code{at} or \code{from}/\code{to}/\code{by}. Lag values are are set
#' by default at all the integer values within the lag period, or determined by
#' \code{lag} and \code{bylag}.
#' 
#' The values in \code{at} can be provided as a vector, and in this case they
#' are replicated for each lag. As an alternative usage, \code{at} can be
#' provided as a matrix of complete exposure histories over the same lag period
#' used for estimation, in order to compute the association with a specific
#' exposure pattern (see also \code{\link{exphistint}}).
#' 
#' Predictions are computed versus a reference value, with default values
#' dependent on the function used in \code{basis}, or manually set through
#' \code{cen}. Briefly, sensible default values are automatically defined for
#' \code{\link{strata}}, \code{\link{thr}} and \code{\link{integer}}
#' (corresponding to the reference region), and for \code{\link{lin}}
#' (corresponding to 0). For other choices, such as \code{\link{ns}},
#' \code{\link{bs}}, \code{\link{poly}} or other existing or user-defined
#' functions, the centering value is set by default to the mid-range. The
#' inclusion of the intercept in \code{basis} term nullifies the centering.
#' 
#' Exponentiated predictions are included if \code{model.link} is equal to
#' \code{log} or \code{logit}. Confidence intervals computed using a normal
#' approximation and a confidence level of \code{ci.level}. \code{model.link}
#' is automatically selected from \code{model} for some classes, but needs to
#' be provided for different classes. Matrices with incremental cumulative
#' predicted associations along lags at each values used for prediction are
#' included if \code{cumul=TRUE}.
#' 
#' The function automatically works with model objects from regression function
#' \code{\link{lm}} and \code{\link{glm}}, \code{\link[mgcv]{gam}} (package
#' \pkg{mgcv}), \code{\link[survival]{coxph}} and
#' \code{\link[survival]{clogit}} (package \pkg{survival}),
#' \code{\link[nlme]{lme}} and \code{\link[nlme]{nlme}} (package \pkg{nlme}),
#' \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}} and
#' \code{\link[lme4]{nlmer}} (package \pkg{lme4}), \code{\link[gee]{gee}}
#' (package \pkg{gee}), \code{\link[geepack]{geeglm}} (package \pkg{geepack}).
#' The function also works with any regression function for which
#' \code{\link{coef}} and \code{\link{vcov}} methods are available. Otherwise,
#' the user needs to input the coefficients and associated (co)variance matrix
#' related to the parameters of the crossbasis as arguments \code{coef} and
#' \code{vcov}, and information on the link function in \code{model.link}. In
#' this case, dimensions and order must match the variables included in
#' \code{basis}.
#' 
#' The function can be used to compute predictions for models with simple basis
#' functions not including lag, computed with \code{\link{onebasis}}. In this
#' case, only unlagged predicted associations are returned.
#' 
#' @aliases crosspred summary.crosspred
#' @param basis an object of class \code{"onebasis"} or \code{"crossbasis"}.
#' @param model a model object for which the prediction is desired. See Details
#' below.
#' @param coef,vcov,model.link user-provided coefficients, (co)variance matrix
#' and model link for the prediction. See Details below.
#' @param at either a numeric vector representing the values of a constant
#' exposure throughout the lag period defined by \code{lag}, or a matrix of
#' exposure histories over the same lag period used for estimation.
#' @param from,to range of predictor values used for prediction.
#' @param lag either an integer scalar or vector of length 2, defining the lag
#' range used for prediction. Defalut to values used for estimation.
#' @param by,bylag increment of the sequences of predictor and lag values used
#' for prediction.
#' @param cen logical or a numeric scalar. It specifies the centering value,
#' then used as a reference for predictions. See Details below.
#' @param ci.level confidence level for the computation of confidence
#' intervals.
#' @param cumul logical. If \code{TRUE}, incremental cumulative associations
#' along lags are also predicted. See Details.
#' @param \dots additional arguments to be passed to \code{summary}.
#' @return A list object of class \code{"crosspred"} with the following
#' (optional) components: \item{predvar }{ vector or matrix of values used for
#' prediction, depending on the format of the argument \code{at} (see Details
#' above).} \item{cen }{ (optional) numeric scalar defining the centering
#' value.} \item{lag }{ integer vector defining the lag range used for
#' prediction.} \item{bylag }{ increment of the sequence of lag values.}
#' \item{coefficients, vcov }{ coefficients and their variance-covariance
#' matrix.} \item{matfit, matse }{ matrices of predictions and standard errors
#' at the chosen combinations of predictor and lag values.} \item{matlow,
#' mathigh }{ matrices of confidence intervals for \code{matfit}.}
#' \item{allfit, allse }{ vectors of the overall cumulative predicted
#' association and standard errors.} \item{alllow, allhigh }{ vectors of
#' confidence intervals for \code{allfit}.} \item{cumfit, cumse }{ matrices of
#' incremental cumulative predicted associations along lags and related
#' standard errors at the chosen combinations of predictor and lag values.
#' Computed if \code{cumul=TRUE}.} \item{cumlow, cumhigh }{ matrices of
#' confidence intervals for \code{cumfit}. Computed if \code{cumul=TRUE}.}
#' \item{matRRfit }{ matrix of exponentiated specific associations from
#' \code{matfit}.} \item{matRRlow, matRRhigh }{ matrices of confidence
#' intervals for \code{matRRfit}.} \item{allRRfit }{ vector of exponentiated
#' overall cumulative associations from \code{allfit}.} \item{allRRlow,
#' allRRhigh }{ vectors of confidence intervals for \code{allRRfit}.}
#' \item{cumRRfit }{ matrix of exponentiated incremental cumulative
#' associations from \code{cumfit}. Computed if \code{cumul=TRUE}.}
#' \item{cumRRlow, cumRRhigh }{ matrix of confidence intervals for . Computed
#' if \code{cumul=TRUE}.} \item{ci.level }{ confidence level used for the
#' computation of confidence intervals for \code{cumRRfit}.} \item{model.class
#' }{ class of the model command used for estimation.} \item{model.link }{ a
#' specification for the model link function.} The function
#' \code{\link{summary.crosspred}} returns a summary of the list.
#' @note All the predictions are generated using a reference value, which if
#' not directly specific by \code{cen} is given default values corresponding to
#' the mid-range point for continuous functions. Before version 2.2.0 of
#' \pkg{dlnm}, centering was produced in \code{\link{onebasis}} or
#' \code{\link{crossbasis}} (see the related help pages), and for backward
#' compatibility this information is kept (with a warning) and used in
#' \code{crosspred} unless \code{cen} is directly defined as an argument.
#' 
#' Exponentiated predictions are included if \code{model.link} (specified
#' automatically by \code{model} or selected by the user) is equal to
#' \code{log} or \code{logit}.
#' @section Warnings: In case of collinear variables in the \code{basis}
#' object, some of them are discarded and the related parameters not included
#' in \code{model}. Then, \code{crosspred} will return an error. Check that the
#' specification of the variables is meaningful through
#' \code{\link{summary.crossbasis}} or \code{\link{summary.onebasis}}.
#' 
#' The name of the object \code{basis} will be used to extract the related
#' estimated parameters from \code{model}. If more than one variable is
#' transformed by cross-basis functions in the same model, different names must
#' be specified.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{onebasis}} to generate one-dimensional basis matrices.
#' \code{\link{crossbasis}} to generate cross-basis matrices.
#' \code{\link{crossreduce}} to reduce the fit to one dimension.  The method
#' function \code{\link[=plot.crosspred]{plot}} to plot several type of graphs.
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
#' @keywords smooth ts
#' @importFrom stats qnorm
#' @examples
#' 
#' ### default usage - see vignette("dlnmTS")
#' 
#' # seasonal analysis: select summer months only
#' chicagoNMMAPSseas <- subset(chicagoNMMAPS, month>5 & month<10)
#' 
#' # create the crossbasis objects, including info on groups
#' cb2.o3 <- crossbasis(chicagoNMMAPSseas$o3, lag=5,
#'   argvar=list(fun="thr",thr=40.3), arglag=list(fun="integer"),
#'   group=chicagoNMMAPSseas$year)
#' cb2.temp <- crossbasis(chicagoNMMAPSseas$temp, lag=10,
#'   argvar=list(fun="thr",thr=c(15,25)), arglag=list(fun="strata",breaks=c(2,6)),
#'   group=chicagoNMMAPSseas$year)
#' summary(cb2.o3)
#' summary(cb2.temp)
#' 
#' # run the model
#' library(splines)
#' model2 <- glm(death ~  cb2.o3 + cb2.temp + ns(doy, 4) + ns(time,3) + dow,
#'   family=quasipoisson(), chicagoNMMAPSseas)
#'   
#' # get the predictions for o3 at specific exposure values
#' pred2.o3 <- crosspred(cb2.o3, model2, at=c(0:65,40.3,50.3))
#' 
#' # get figures for the overall cumulative association, with ci
#' pred2.o3$allRRfit["50.3"]
#' cbind(pred2.o3$allRRlow, pred2.o3$allRRhigh)["50.3",]
#' 
#' # plot the estimated lag-response curve (with 80%CI)
#' plot(pred2.o3, "slices", var=50.3, ci="bars", type="p", col=2, pch=19,
#'   ci.level=0.80, main="Lag-response a 10-unit increase above threshold (80CI)")
#' # plot the estimated overall cumulative exposure-response curve
#' plot(pred2.o3,"overall",xlab="Ozone", ci="l", col=3, ylim=c(0.9,1.3), lwd=2,
#'   ci.arg=list(col=1,lty=3), main="Overall cumulative association for 5 lags")
#'   
#' # plot the estimated exposure-lag-response surface
#' plot(pred2.o3, xlab="Ozone", main="3D: default perspective")
#' plot(pred2.o3, xlab="Ozone", main="3D: different perspective", theta=250, phi=40)
#' 
#' ### extended usage - see vignette("dlnmExtended")
#' 
#' # generate the matrix of exposure histories from the weekly data
#' Qdrug <- as.matrix(drug[,rep(7:4, each=7)])
#' colnames(Qdrug) <- paste("lag", 0:27, sep="")
#' 
#' # define the decay function
#' fdecay <- function(x, scale=5, ...) {
#'   basis <- exp(-x/scale)
#'   attributes(basis)$scale <- scale
#'   return(basis)
#' }
#' 
#' # define the cross-basis
#' cbdrug2 <- crossbasis(Qdrug, lag=27, argvar=list("lin"),
#'   arglag=list(fun="fdecay",scale=6))
#' summary(cbdrug2)
#' 
#' # run the model and predict
#' mdrug2 <- lm(out~cbdrug2+sex, drug)
#' pdrug2 <- crosspred(cbdrug2, mdrug2, at=0:20*5)
#' 
#' # dose 20 for 10 days
#' histdrug <- exphist(rep(20,10), time=10, lag=27)
#' pdrug4 <- crosspred(cbdrug2, mdrug2, at=histdrug)
#' with(pdrug4,c(allfit,alllow,allhigh))
#' 
#' # define exposure profile with weekly exposures to 10, 50, 0 and 20
#' expdrug <- rep(c(10,50,0,20),c(2,1,1,2)*7)
#' 
#' # define the exposure histories for all the time points
#' dynhist <- exphist(expdrug, lag=27)
#' 
#' # predict the effects
#' pdyndrug <- crosspred(cbdrug2, mdrug2, at=dynhist)
#' 
#' # plot of the evolution of the effects along time given the doses
#' plot(pdyndrug,"overall", ylab="Effect", xlab="Time (days)", ylim=c(-5,27), 
#'   xlim=c(1,50))
#' 
crosspred <-
function(basis, model=NULL, coef=NULL, vcov=NULL, model.link=NULL, at=NULL,
  from=NULL, to=NULL, by=NULL, lag, bylag=1, cen=NULL, ci.level=0.95,
  cumul=FALSE) {
#
################################################################################
# DETERMINE THE TYPE OF MODEL AND CHECKS
#
  # TYPE OF PREDICTION: CROSSBASIS, ONEBASIS, OR PENALIZED GAM
  type <- if(any(class(basis)%in%"crossbasis")) "cb" else 
    if(any(class(basis)%in%"onebasis")) "one" else "gam"
#
  # CHECKS ON TYPE, AND SET name, basis AND RESET type
  errormes <- "arguments 'basis' and 'model' not consistent. See help(crosspred)"
  if(type=="gam") {
    if(!is.character(basis)||length(basis)>1L) stop(errormes)
    if(is.null(model)||!any(class(model)%in%"gam")) stop(errormes)
    name <- basis
    sterms <- sapply(model$smooth,function(x) x$term[1])
    if(name%in%sterms) basis <- model$smooth[[which(sterms==name)[1]]] else 
      stop(errormes)
    if(length(which(sterms==name))>1)
      warning(paste(name,"included in multiple smoothers, only the first one taken"))
    if(!"cb.smooth"%in%class(basis) && basis$dim>1L)
      stop("predictions not provided for multidimensional smoothers")
  } else name <- deparse(substitute(basis))
#
  #  EXTRACT lag (DEPENDENT ON TYPE)
  lag <- if(missing(lag)) switch(type,
    cb = attr(basis,"lag"),
    one = c(0,0),
    gam = if(is.null(basis$lag)) c(0,0) else basis$lag
  ) else mklag(lag)
  if(type=="cb" && lag!=attr(basis,"lag") && attr(basis,"arglag")$fun=="integer")
    stop("prediction for lag sub-period not allowed for type 'integer'")
#
  # OTHER COHERENCE CHECKS
  if(is.null(model)&&(is.null(coef)||is.null(vcov)))
    stop("At least 'model' or 'coef'-'vcov' must be provided")
  if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0)
    stop("'ci.level' must be numeric and between 0 and 1")
#
  # CUMULATIVE EFFECTS ONLY WITH LAGGED EFFECTS AND lag[1]==0
  if(cumul==TRUE && (diff(lag)==0L || lag[1]!=0L)) {
    cumul <- FALSE
    warning("Cumulative predictions only computed if diff(lag)>0 and lag[1]=0")
  }
#
################################################################################
# SET COEF, VCOV CLASS AND LINK FOR EVERY TYPE OF MODELS
#
  # WRITE CONDITIONS (DEPENDENT ON TYPE AND IF MATRIX/VECTOR)
  cond <- if(type=="gam") with(basis,first.para:last.para) else
    if(ncol(basis)==1L) name else
    if(type=="one") paste(name,"[[:print:]]*b[0-9]{1,2}",sep="") else
      paste(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
#
  # IF MODEL PROVIDED, EXTRACT FROM HERE, OTHERWISE DIRECTLY FROM COEF AND VCOV
  if(!is.null(model)) {
    model.class <- class(model)
    coef <- getcoef(model,model.class)
    ind <- if(type=="gam") cond else grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- getlink(model,model.class)
  } else model.class <- NA
#
  # CHECK
  npar <- if(type=="gam") length(ind) else ncol(basis)
  if(length(coef)!=npar || length(coef)!=dim(vcov)[1] || any(is.na(coef)) ||
      any(is.na(vcov)))
    stop("coef/vcov not consistent with basis matrix. See help(crosspred)")
#
##########################################################################
# AT, PREDVAR, PREDLAG AND CENTERING
#
  # RANGE
  range <- if(type=="gam") range(model$model[[basis$term[1]]]) else
    attr(basis,"range")
#
  # SET at, predvar AND predlag
  at <- mkat(at,from,to,by,range,lag,bylag)
  predvar <- if(is.matrix(at)) rownames(at) else at
  predlag <- seqlag(lag,bylag)
#
  # DEFINE CENTERING VALUE (NULL IF UNCENTERED), AND REMOVE INFO FROM BASIS
  cen <- mkcen(cen,type,basis,range)
  if(type=="one") attributes(basis)$cen <- NULL
  if(type=="cb") attributes(basis)$argvar$cen <- NULL
#
################################################################################
# PREDICTION OF LAG-SPECIFIC EFFECTS
#
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON TYPE)
  Xpred <- mkXpred(type,basis,at,predvar,predlag,cen)
#
  # CREATE LAG-SPECIFIC EFFECTS AND SE
  matfit <- matrix(Xpred%*%coef,length(predvar),length(predlag)) 
  matse <- matrix(sqrt(pmax(0,rowSums((Xpred%*%vcov)*Xpred))),length(predvar),
    length(predlag)) 
#
  # NAMES
  rownames(matfit) <- rownames(matse) <- predvar
  colnames(matfit) <- colnames(matse) <- outer("lag",predlag,paste,sep="")
#
################################################################################
# PREDICTION OF OVERALL+CUMULATIVE EFFECTS
#
  # RE-CREATE LAGGED VALUES (NB: ONLY LAG INTEGERS)
  predlag <- seqlag(lag)
#
  # CREATE THE MATRIX OF TRANSFORMED VARIABLES (DEPENDENT ON TYPE)
  Xpred <- mkXpred(type,basis,at,predvar,predlag,cen)
#
  # CREATE OVERALL AND (OPTIONAL) CUMULATIVE EFFECTS AND SE
  Xpredall <- 0
  if(cumul) {
    cumfit <- cumse <- matrix(0,length(predvar),length(predlag))
  }
  for (i in seq(length(predlag))) {
    ind <- seq(length(predvar))+length(predvar)*(i-1)
    Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    if(cumul) {
      cumfit[, i] <- Xpredall %*% coef
      cumse[, i] <- sqrt(pmax(0,rowSums((Xpredall%*%vcov)*Xpredall)))
    }
  }
  allfit <- as.vector(Xpredall %*% coef)
  allse <- sqrt(pmax(0,rowSums((Xpredall%*%vcov)*Xpredall)))
#
  # NAMES
  names(allfit) <- names(allse) <- predvar
  if(cumul) {
    rownames(cumfit) <- rownames(cumse) <- predvar
    colnames(cumfit) <- colnames(cumse) <- outer("lag",seqlag(lag),paste,sep="")
  }
#
################################################################################
# CREATE THE OBJECT
#
  # INITIAL LIST, THEN ADD COMPONENTS
  list <- list(predvar=predvar)
  if(!is.null(cen)) list$cen <- cen
  list <- c(list,list(lag=lag,bylag=bylag,coefficients=coef,vcov=vcov,
    matfit=matfit,matse=matse,allfit=allfit,allse=allse))
  if(cumul) list <- c(list,list(cumfit=cumfit,cumse=cumse))
#
  # MATRICES AND VECTORS WITH EXPONENTIATED EFFECTS AND CONFIDENCE INTERVALS
  z <- qnorm(1-(1-ci.level)/2)
  if(!is.null(model.link) && model.link %in% c("log","logit")) {
    list$matRRfit <- exp(matfit)
    list$matRRlow <- exp(matfit-z*matse)
    list$matRRhigh <- exp(matfit+z*matse)
    list$allRRfit <- exp(allfit)
    list$allRRlow <- exp(allfit-z*allse)
    names(list$allRRlow) <- names(allfit)
    list$allRRhigh <- exp(allfit+z*allse)
    names(list$allRRhigh) <- names(allfit)
    if(cumul) {
      list$cumRRfit <- exp(cumfit)
      list$cumRRlow <- exp(cumfit-z*cumse)
      list$cumRRhigh <- exp(cumfit+z*cumse)
    }
  } else {
    list$matlow <- matfit-z*matse
    list$mathigh <- matfit+z*matse
    list$alllow <- allfit-z*allse
    names(list$alllow) <- names(allfit)
    list$allhigh <- allfit+z*allse
    names(list$allhigh) <- names(allfit)
    if(cumul) {
      list$cumlow <- cumfit-z*cumse
      list$cumhigh <- cumfit+z*cumse
    }
  }
#
  list$ci.level <- ci.level
  list$model.class <- model.class
  list$model.link <- model.link
#
  class(list) <- "crosspred"
#
  return(list)
}

