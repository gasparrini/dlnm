###
### R routines for the R package dlnm (c)
#
getlink <-
function(model, class, model.link=NULL) {
#
################################################################################
#
  # IF PROVIDED, JUST RETURN
  if(!is.null(model.link)) return(model.link)
#
  # OTHERWISE, EXTRACT FROM MODEL (IF AVAILABLE)
  link <- if(all(class%in%c("lm")) || all(class%in%c("lme")) ||
    any(class%in%"nlme") || any(class%in%"lmerMod")) "identity" else 
    if(any(class %in% c("clogit"))) "logit" else
    if(any(class %in% c("coxph"))) "log" else
    if(any(class %in% c("glm")) || any(class %in% c("glmmPQL")))
    model$family$link else if(any(class %in% c("glmerMod")))
    model@resp$family$link else NA
#
  return(link)
}

