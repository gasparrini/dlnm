###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2017
#
getlink <-
function(model, class) {
#
################################################################################
#
  # EXTRACT MODEL LINK
  link <- if(all(class%in%c("lm")) || all(class%in%c("lme")) ||
    any(class%in%"nlme") || any(class%in%"lmerMod")) "identity" else 
    if(any(class %in% c("clogit"))) "logit" else
    if(any(class %in% c("coxph","coxme"))) "log" else
    if(any(class %in% c("glm")) || any(class %in% c("glmmPQL")))
    model$family$link else if(any(class %in% c("glmerMod")))
    model@resp$family$link else NA
#
  return(link)
}

