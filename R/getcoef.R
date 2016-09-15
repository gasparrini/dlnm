###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
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

