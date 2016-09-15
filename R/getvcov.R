###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
getvcov <-
  function(model, class) {
    #
    ################################################################################
    #
    # EXTRACT VCOV
    # NB: gam, gee AND geeglm HAVE CLASS glm AS WELL
    vcov <- if(any(class%in%c("lm","glm","lme","coxph")) &&
        !any(class%in%c("gee"))) vcov(model) else if(identical(class,c("gee","glm")))
          model$robust.variance else if(any(class%in%c("geeglm")))
            summary(model)$cov.scaled else if(any(class%in%c("lmerMod","glmerMod")))
              as.matrix(vcov(model)) else tryCatch(vcov(model),error=function(w) "error")
    if(identical(vcov,"error")) stop("methods for coef() and vcov() must",
      "exist for the class of object 'model'. If not, extract them manually and",
      "use the arguments 'coef' and 'vcov'")
    #
    return(vcov)
  }

