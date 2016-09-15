###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
summary.crosspred <-
function(object, ...) {
#
################################################################################
#
  cat("PREDICTIONS:\n")
  cat("values:",length(object$predvar),"\n")
  if(!is.null(object$cen)) cat("centered at:",object$cen,"\n")
  cat("range:",min(object$predvar),",",max(object$predvar),"\n")
  cat("lag:",object$lag,"\n")
  cat("exponentiated:",ifelse(!is.null(object$allRRfit),"yes","no"),"\n")
  cat("cumulative:",ifelse(!is.null(object$cumfit),"yes","no"),"\n")
#
  cat("\nMODEL:\n")
  cat("parameters:",length(object$coef),"\n")
  cat("class:",object$model.class,"\n")
  cat("link:",object$model.link,"\n")
  cat("\n")
}

