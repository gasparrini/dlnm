###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#


#' Reduce the Fit of a DLNM to One-Dimensional Summaries
#' 
#' The function reduces the fit of a bi-dimensional DLNM to summaries defined
#' in the the dimension of predictor or lags only, and re-expresses it in terms
#' of modified parameters of the one-dimensional basis functions chosen for
#' that space.
#' 
#' The dimension to which the fit is reduced is chosen by \code{type},
#' computing summaries for overall cumulative or lag-specific associations
#' defining an exposure-response relationship in the predictor space, or
#' predictor-specific associations defining a lag-response relationship in the
#' lag space. The function re-expresses the original fit of the model, defined
#' by the parameters of the bi-dimensional cross-basis functions, in summaries
#' defined by the one-dimensional basis for the related space and a (usually
#' smaller) set of modified parameters.
#' 
#' Similarly to \code{\link{crosspred}}, the object \code{basis} must be the
#' same containing the cross-basis matrix included in \code{model}, with its
#' attributes and class. The function computes predictions for specific values
#' of predictor (for \code{type} equal to \code{"overall"} and \code{"lag"}) or
#' lag (for for \code{type} equal to \code{"var"}). Values are set to default
#' or chosen thorugh \code{at}/\code{from}/\code{to}/\code{by} and
#' \code{lag}/\code{bylag}, respectively.
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
#' \code{vcov}. In this case, their dimensions and order must match the
#' variables included in \code{basis}.
#' 
#' @aliases crossreduce summary.crossreduce
#' @param basis an object of class \code{"crossbasis"}.
#' @param model a model object for which the reduction and prediction are
#' desired. See Details below.
#' @param coef,vcov,model.link user-provided coefficients, (co)variance matrix
#' and model link for the reduction and then prediction. See Details below.
#' @param type type of reduction. Possible options are \code{"overall"}
#' (default) for reduction to the overall cumulative exposure-response
#' association, \code{"lag"} for reduction to a lag-specific exposure-response
#' association, or \code{"var"} for reduction to a predictor-specific
#' lag-response association. See Details below.
#' @param value the single value of predictor or lag at which
#' predictor-specific or lag-specific associations must be defined,
#' respectively. See Details below.
#' @param at vector of values used for prediction in the dimension of
#' predictor.
#' @param from,to range of predictor values used for prediction.
#' @param lag either an integer scalar or vector of length 2, defining the lag
#' range used for prediction. Defalut to values used for estimation.
#' @param by,bylag increment of the sequences of predictor and lag values used
#' for prediction.
#' @param cen logical or a numeric scalar. It specifies the centering value,
#' then used as a reference for predictions. See Details below.
#' @param ci.level confidence level for the computation of confidence
#' intervals.
#' @param \dots additional arguments to be passed to \code{summary}.
#' @return A list object of class \code{"crossreduce"} with the following
#' (optional) components: \item{coefficients, vcov }{ reduced parameters of the
#' original fitted model for the chosen dimension.} \item{basis }{ basis matrix
#' computed at \code{predvar} or for the sequence of lags defined by
#' \code{lag}, depending on the chosen dimension.} \item{type, value }{ type of
#' reduction and (optional) value, as arguments above.} \item{cen }{ (optional)
#' numeric scalar defining the centering value.} \item{predvar }{ vector of
#' observations used for prediction, if the reduction is in the dimension of
#' predictor.} \item{lag }{ integer vector defining the lag range.} \item{bylag
#' }{ increment of the sequence of lag values.} \item{fit, se }{ vectors of the
#' predicted association and related standard errors.} \item{low, high }{
#' vectors of confidence intervals for \code{fit}.} \item{RRfit }{ vector of
#' exponentiated predicted associations from \code{fit}.} \item{RRlow, RRhigh
#' }{ vectors of confidence intervals for \code{RRfit}.} \item{ci.level }{
#' confidence level used for the computation of confidence intervals.}
#' \item{model.class }{ class of the model command used for estimation.}
#' \item{model.link }{ a specification for the model link function.}
#' @note All the predictions are generated using a reference value, which if
#' not directly specific by \code{cen} is given default values corresponding to
#' the mid-range point for continuous functions. Before version 2.2.0 of
#' \pkg{dlnm}, centering was produced in \code{\link{crossbasis}} (see the
#' related help page), and for backward compatibility this information is kept
#' (with a warning) and used in \code{crossreduce} unless \code{cen} is
#' directly defined as an argument.
#' 
#' Exponentiated predictions are included if \code{model.link} (specified
#' automatically by \code{model} or selected by the user) is equal to
#' \code{log} or \code{logit}.
#' @section Warnings: In case of collinear variables in the \code{basis}
#' object, some of them are discarded and the related parameters not included
#' in \code{model}. Then, \code{crossreduce} will return an error. Check that
#' the specification of the variables is meaningful through
#' \code{\link[=summary.crossbasis]{summary}}.
#' 
#' The name of the object \code{basis} will be used to extract the related
#' estimated parameters from \code{model}. If more than one variable is
#' transformed by cross-basis functions in the same model, different names must
#' be specified.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{crossbasis}} to generate cross-basis matrices.
#' \code{\link{crosspred}} to obtain predictions after model fitting. The
#' method function \code{\link[=plot.crossreduce]{plot}} to plot the
#' association.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @references Gasparrini A., Armstrong, B., Kenward M. G. Reducing and
#' meta-analyzing estimates from distributed lag non-linear models.\emph{BMC
#' Medical Research Methodology}. 2013; \bold{13}(1):1, freely available at
#' \url{http://www.ag-myresearch.com/bmcmrm2013}.
#' @keywords smooth ts
#' @importFrom stats qnorm
#' @examples
#' 
#' # create the crossbasis object
#' lagnk <- 3
#' lagknots <- exp(((1+log(30))/(lagnk+1) * seq(lagnk))-1)
#' cb4 <- crossbasis(chicagoNMMAPS$temp, lag=30, argvar=list(fun="thr",
#'   thr=c(10,25)), arglag=list(knots=lagknots))
#' 
#' # # run the model and get the predictions
#' library(splines)
#' model4 <- glm(death ~  cb4 + ns(time, 7*14) + dow, family=quasipoisson(),
#'   chicagoNMMAPS)
#' pred4 <- crosspred(cb4, model4, by=1)
#' 
#' # reduce to overall cumulative association
#' redall <- crossreduce(cb4, model4)
#' summary(redall)
#' # reduce to exposure-response association for lag 5
#' redlag <- crossreduce(cb4, model4, type="lag", value=5)
#' # reduce to lag-response association for value 33
#' redvar <- crossreduce(cb4, model4, type="var", value=33)
#' 
#' # compare number of parameters
#' length(coef(pred4))
#' length(coef(redall))
#' length(coef(redlag))
#' length(coef(redvar))
#' 
#' # test
#' plot(pred4, "overall", xlab="Temperature", ylab="RR",
#'   ylim=c(0.8,1.6), main="Overall cumulative association")
#' lines(redall, ci="lines",col=4,lty=2)
#' legend("top",c("Original","Reduced"),col=c(2,4),lty=1:2,ins=0.1)
#' 
#' # reconstruct the fit in terms of uni-dimensional function
#' b4 <- onebasis(0:30,knots=attributes(cb4)$arglag$knots,int=TRUE)
#' pred4b <- crosspred(b4,coef=coef(redvar),vcov=vcov(redvar),model.link="log",by=1)
#' 
#' # test
#' plot(pred4, "slices", var=33, ylab="RR", ylim=c(0.9,1.2),
#'   main="Lag-response association at 33C")
#' lines(redvar, ci="lines", col=4, lty=2)
#' points(pred4b, pch=19, cex=0.6)
#' legend("top",c("Original","Reduced","Reconstructed"),col=c(2,4,1),lty=c(1:2,NA),
#'   pch=c(NA,NA,19),pt.cex=0.6,ins=0.1)
#' 
crossreduce <-
function(basis, model=NULL, type="overall", value=NULL, coef=NULL, vcov=NULL,
  model.link=NULL, at=NULL, from=NULL, to=NULL, by=NULL, lag, bylag=1, cen=NULL,
  ci.level=0.95) {
#
################################################################################
# CHECK BASIS AND WRITE CONDITION (REGULAR EXPRESSION) TO EXTRACT COEF-VCOV
#
  if(all(class(basis)!="crossbasis")) {
    stop("the first argument must be an object of class 'crossbasis'")
  }
  name <- deparse(substitute(basis))
  attr <- attributes(basis)
  if(ncol(basis)==1) cond <- name
#
###########################################################################
# COHERENCE CHECKS (SEE CROSSPRED)
#
  if(is.null(model)&&(is.null(coef)||is.null(vcov))) {
    stop("At least 'model' or 'coef'-'vcov' must be provided")
  }
  type <- match.arg(type,c("overall","var","lag"))
  if(type!="overall") {
    if(is.null(value)) stop("'value' must be provided for type 'var' or 'lag'")
    else if(!is.numeric(value)||length(value)>1) {
      stop("'value' must be a numeric scalar")
    }
    if(type=="lag" && (any(value<attr$lag[1]) ||any(value>attr$lag[2]))) {
      stop("'value' of lag-specific effects must be within the lag range")
    }
  } else value <- NULL
#
  #  lag MUST BE A POSITIVE INTEGER VECTOR, BY DEFAULT THAT USED FOR ESTIMATION
  lag <- if(missing(lag)) attr$lag else mklag(lag)
  if(lag!=attr$lag && attr$arglag$fun=="integer")
      stop("prediction for lag sub-period not allowed for type 'integer'")
#
  if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0) {
    stop("'ci.level' must be numeric and between 0 and 1")
  }
#
###########################################################################
# SET COEF, VCOV CLASS AND LINK FOR EVERY TYPE OF MODELS
#
  # WRITE CONDITIONS (DEPENDENT ON TYPE AND IF MATRIX/VECTOR)
  cond <- if(ncol(basis)==1L) name else 
    paste(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
#
  # IF MODEL PROVIDED, EXTRACT FROM HERE, OTHERWISE DIRECTLY FROM COEF AND VCOV
  if(!is.null(model)) {
    model.class <- class(model)
    coef <- getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- getlink(model,model.class)
  } else model.class <- NA
#
  # CHECK COEF AND VCOV
  npar <- if(type=="gam") length(ind) else ncol(basis)
  npar <- if(type=="gam") length(ind) else ncol(basis)
  if(length(coef)!=npar || length(coef)!=dim(vcov)[1] || any(is.na(coef)) ||
      any(is.na(vcov)))
    stop("coef/vcov do not consistent with basis matrix. See help(crossreduce)")
#
##########################################################################
# AT AND CENTERING
#
  # SET at
  if(is.matrix(at)) stop("argument 'at' must be a vector")
  range <- attr$range
  at <- mkat(at,from,to,by,range,lag,bylag)
#
  # DEFINE CENTERING VALUE (NULL IF UNCENTERED), AND REMOVE INFO FROM BASIS
  cen <- mkcen(cen,type="cb",basis,range)
  attributes(basis)$argvar$cen <- attr$argvar$cen <- NULL
#
##########################################################################
# REDUCTION
#
  # CREATE (CENTERED) TRANSFORMATION MATRIX AND BASIS
  # NB: ORDER OF THE TENSOR REVERSED SINCE VERSION 2.2.4
  # NOW ORDER COMPATIBLE WITH crossbasis BUT REVERSE OF GASPARRINI BMCmrm 2013
  if(type=="overall") {
    lagbasis <- do.call("onebasis",c(list(x=seqlag(lag)),attr$arglag))
    M <- diag(ncol(basis)/ncol(lagbasis)) %x%
      (t(rep(1,diff(lag)+1)) %*% lagbasis)  
    newbasis <- do.call("onebasis",c(list(x=at),attr$argvar))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr$argvar))
      newbasis <- scale(newbasis,center=basiscen,scale=FALSE)
    }
  } else if(type=="lag") {
    lagbasis <- do.call("onebasis",c(list(x=value),attr$arglag))
    M <- diag(ncol(basis)/ncol(lagbasis)) %x% lagbasis
    newbasis <- do.call("onebasis",c(list(x=at),attr$argvar))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr$argvar))
      newbasis <- scale(newbasis,center=basiscen,scale=FALSE)
    }
  } else if(type=="var") {
    varbasis <- do.call("onebasis",c(list(x=value),attr$argvar))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr$argvar))
      varbasis <- scale(varbasis,center=basiscen,scale=FALSE)
    }
    M <- varbasis %x% diag(ncol(basis)/ncol(varbasis))
    newbasis <- do.call("onebasis",c(list(x=seqlag(lag,bylag)),attr$arglag))
  }
#
  # NAMES
  dimnames(newbasis) <- list(seq(nrow(newbasis)),paste0("b",seq(ncol(newbasis))))
#
  # CREATE NEW SET OF COEF AND VCOV
  newcoef <- as.vector(M%*%coef)
  names(newcoef) <- colnames(newbasis)
  newvcov <- M%*%vcov%*%t(M)
  dimnames(newvcov) <- list(colnames(newbasis),colnames(newbasis))
#
##########################################################################
# PREDICTION
#
  fit <- as.vector(newbasis%*%newcoef)
  se <- sqrt(pmax(0,rowSums((newbasis%*%newvcov)*newbasis)))
  if(type=="var") {
    names(fit) <- names(se) <- outer("lag",seqlag(lag,bylag),paste,sep="")
  }else names(fit) <- names(se) <- at
#
###########################################################################
# CREATE THE OBJECT
#
  # INITIAL LIST
  list <- list(coefficients=newcoef,vcov=newvcov,basis=newbasis,type=type,
    value=value)
  if(type!="var") list$predvar <- at
  if(!is.null(cen)) list$cen <- cen
  list <- c(list,list(lag=lag,bylag=bylag,fit=fit,se=se))
#
  # VECTORS WITH EXPONENTIATED EFFECTS AND CONFIDENCE INTERVALS
  z <- qnorm(1-(1-ci.level)/2)
  if(model.link %in% c("log","logit")) {
    list$RRfit <- exp(fit)
    list$RRlow <- exp(fit-z*se)
    names(list$RRlow) <- names(fit)
    list$RRhigh <- exp(fit+z*se)
    names(list$RRhigh) <- names(fit)
  } else {
    list$low <- fit-z*se
    names(list$low) <- names(fit)
    list$high <- fit+z*se
    names(list$high) <- names(fit)
  }
#
  list$ci.level <- ci.level
  list$model.class <- model.class
  list$model.link <- model.link
#
  class(list) <- "crossreduce"
#
  return(list)
}
