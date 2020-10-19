###
### R routines for the R package dlnm (c)
#
smooth.construct.cb.smooth.spec <- function(object, data, knots) {
#
################################################################################
#
  # CHECK THAT THE SMOOTHER IS APPLIED TO A SINGLE TERM
  if(length(object$term)!=2L) stop("'cb' smoother only accepts two terms")
#
  # CHECK FOR argvar AND arglag OBJECTS IN xt
  argvar <- object$xt$argvar
  arglag <- object$xt$arglag
#
  # PREVENT USE OF by
  if(object$by!="NA") stop("'by' argument not (yet) applicable with 'cb' smoother")
#
################################################################################
# DEFINE ARGUMENTS FOR BUILDING MARGINAL BASES: term, k, fx, bs, m (NO xt NEEDED)
#
  # TERMS AND DIMENSIONS
  term <- object$term
  dim <- length(term)
#
  # EVALUATE k (FROM bs.dim, DEFAULT TO 10)
  k <- object$bs.dim
  k[k<0] <- 10
  if(length(k)==1) k <- rep(k,2)
#
  # EVALUATE bs (STORED IN xt, DEFAULT TO 'ps', AND ONLY 'ps'-'cr' ACCEPTED)
  bs <- object$xt$bs
  if(is.null(bs)) bs <- "ps" else if(!all(bs%in%c("ps","cr", "cs"))) 
      stop("only 'ps', 'cr', and 'cs' smoothers accepted within 'cb'")
  if(length(bs)==1) bs <- rep(bs,2)
#
  # EVALUATE m (FROM p.order)
  m <- object$p.order
  if(!is.list(m)&&length(m)==1) m <- rep(m,2)
#
  # EVALUATE fx (FROM fixed)
  fx <- object$fixed
  if(sum(is.na(fx))||is.null(fx)) fx <- rep(FALSE,2) else 
    if(length(fx)==1) fx <- rep(fx,2)
#
################################################################################
# BUILD THE MARGINAL BASES
#
  # SET EMPTY LISTS TO STORE THE RESULTS
  margin <- dat <- knt <- Xm <- Sm <- list(var=NULL,lag=NULL)
  # SET EMPTY OBJECT TO STORE INFO ON DIMENSIONS, RANK, NULL SPACE
  # NB: SOME LEFT 0 (THEREFORE NOT SET) FOR PARAMETRIC SMOOTHERS
  d <- r <- nr <- rep(0,2)
  # SET CONSTRAINTS
  mc <- c(TRUE,FALSE)
  C <- matrix(0,0,0)
#
  # MARGINAL BASES
  # IF SMOOTHER NOT DEFINED THROUGH xt LIST, USE THE STANDARD METHOD
  # OTHERWISE, USE onebasis FOR PARAMETRIC SMOOTHERS 
  for(i in seq(dim)) {
    xtarg <- if(i==1) argvar else arglag
    dat <- data[term[i]]
    knt <- knots[term[i]]
    if(is.null(xtarg)) {
      sobj <- do.call(s,list(as.name(term[i]),k=k[i],fx=fx[i],bs=bs[i],m=m[[i]]))
      margin[[i]] <- 
        if(mc[i]) {
          smoothCon(sobj,dat,knt,absorb.cons=TRUE,
                    n=length(dat[[1]]))[[1]] 
        } else {
          smooth.construct(sobj,dat,knt)
        }
      Xm[[i]] <- margin[[i]]$X
      if(!fx[i]) Sm[[i]] <- margin[[i]]$S[[1]]
      d[i] <- ncol(margin[[i]]$X)
      r[i] <- margin[[i]]$rank
      nr[i] <- margin[[i]]$null.space.dim
    } else {
      #TODO: warn if user supplies basis in `bs = ` as well as `arglag` or `argvar` because this is going to ignore the `bs` argument
      # IF arglag, ADD AN INTERCEPT IF APPROPRIATE
      if (i == 2 &&
          (is.null(xtarg$fun) ||
           "intercept" %in% names(formals(xtarg$fun))) &&
          sum(pmatch(names(xtarg), "intercept", nomatch = 0)) == 0) {
        xtarg$intercept <- TRUE
      }
      Xm[[i]] <- do.call("onebasis",modifyList(xtarg,list(x=dat[[term[i]]])))
      attr <- attributes(Xm[[i]])
      ind <- match(c("fun",names(formals(attr$fun))),names(attr),nomatch=0)
      margin[[i]] <- attr[ind]
      class(margin[[i]]) <- "onebasis"
      if(!attr$fun%in%c("ps","cr", "cs")) fx[i] <- TRUE
      if(!fx[i]) Sm[[i]] <- attr$S
      d[i] <- nr[i] <- ncol(Xm[[i]])
    }
  }
#
################################################################################
# BUILD TENSOR AND PENALTY MATRICES
#
  # TENSOR (USING mgcv FUNCTION)
  # NB: REMOVING NAMES SPEEDS UP THE TENSOR COMPUTATION SUBSTANTIALLY
  X <- tensor.prod.model.matrix(unname(Xm))
#
  # PENALTY MATRICES: FIRST RESCALE, THEN EXPAND (AS tensor.prod.penalties)
  S <- list()
  if(!fx[1]) {
    Sm[["var"]] <- Sm[["var"]]/eigen(Sm[["var"]],symmetric=TRUE,only.values=TRUE)$values[1]
    S <- c(S,list(Svar=Sm[["var"]]%x%diag(d[2])))
  }
  if(!fx[2]) {
    Sm[["lag"]] <- Sm[["lag"]]/eigen(Sm[["lag"]],symmetric=TRUE,only.values=TRUE)$values[1]
    S <- c(S,list(Slag=diag(d[1])%x%Sm[["lag"]]))
  }
#
  # RANK
  max.rank <- prod(d)
  r <- max.rank*r/d
  r <- r[!fx]
  nr <-  prod(nr)
#
  # ADDITIONAL PENALTIES ON THE LAG STRUCTURE
  if(!is.null(object$xt$addSlag)) {
    addS <- mkaddSlag(object$xt$addSlag,d)
    S <- c(S,addS)
    r <- c(r,sapply(addS,findrank))
  }
#
  # ERASE MARGINAL BASIS AND PENALTY MATRICES TO SAVE MEMORY
  for(i in seq(dim)) margin[[i]]$X <- margin[[i]]$S <- NULL
#
################################################################################
# ADD OTHER INFO AND SET OTHER FEATURES OF TENSOR
#
  # BUILD THE OBJECT (SOME INFO IN THE ORIGINAL SMOOTH SPEC OBJECT)
  # NB:
  #   - np SET TO FALSE (NO REPARAMETERIZATION), SO XP=list()
  #   - plot.me DOES NOT ALLOW (YET) PLOTTING USING mgcv FUNCTIONS
  ret <-
    list(
      margin = margin,
      term = term,
      by = object$by,
      fx = fx,
      label = object$label,
      dim = dim,
      mp = TRUE,
      np = FALSE,
      id = object$id,
      sp = object$sp,
      inter = TRUE,
      mc = mc,
      plot.me = FALSE,
      X = X,
      S = S,
      C = C,
      df = ncol(X),
      null.space.dim = nr,
      rank = r,
      XP = list()
    )
#
  # ADD ADDITIONAL ATTRIBUTES SPECIFIC TO cb SMOOTHER
  ret$lag <- range(data[[term[2]]])
#
  # CLASS
  class(ret) <- c("cb.smooth","tensor.smooth")
#
  return(ret)
}
