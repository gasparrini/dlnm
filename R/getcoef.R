###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016

#' @describeIn internals extracts coefficients depending on the class of the model, 
#' and returns a message error if the process fails. Used internally in \code{\link{crosspred}} and
#' \code{\link{crossreduce}}.
#' @importFrom nlme fixef
#' @importFrom lme4 fixef
#' @param model a regression model object.
#' @param class a regression model class.
getcoef <-
  function(model, class) {
    #
    ################################################################################
    #
    # EXTRACT COEF
    # NB: gam, gee AND geeglm HAVE CLASS glm AS WELL
    coef <- if(any(class%in%c("glm","gam","coxph"))) coef(model) else
      if(any(class%in%c("lme","lmerMod","glmerMod"))) fixef(model) else
        tryCatch(coef(model),error=function(w) "error")
    if(identical(coef,"error")) stop("methods for coef() and vcov() must",
      "exist for the class of object 'model'. If not, extract them manually and",
      "use the arguments 'coef' and 'vcov'")
    #
    return(coef)
  }

