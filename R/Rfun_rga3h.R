# Rfun_rga3h
# 2020-04-12
#
#' @name rga3h
#' @title reverse graphical approach for three hypotheses
#' @param w a vector of initial weights
#' @param G a matrix of initial transaction weights
#' @param p a vector of p-values
#' @param alpha a number of significance level
#' @return a logical vector indicating whether the hypothesis is rejected: TRUE = rejected, FALSE = accepted
#' @author Jiangtao Gou
#' @export
#' @references
#' Gou, J. (2020). Reverse graphical approaches for multiple test procedures. Technical Report.
#'
#' @examples
#' w <- c(0.3,0.5,0.2)
#' G <- matrix(c(0,1/3,2/3, 1/2,0,1/2, 1/5,4/5,0),nrow=3,byrow=TRUE)
#' p <- c(0.012, 0.051, 0.021)
#' p <- c(0.012, 0.051, 0.019)
#' alpha <- 0.05
#' rga3h(w=w,G=G,p=p, alpha=alpha)
#
rga3h <- function (w,G,p,alpha) {
  result <- rep(FALSE, times=3)
  # Step 1
  cmpr <- rep(FALSE, times=3)
  for (k in 1:3) {
    vec01 <- rep(0, times=3)
    vec01[k] <- 1
    rdcG <- graphUpdate(w=w, G=G, vec01=vec01)
    cmpr[k] <- (p[k] <= rdcG$w * alpha)
  }
  if (all(cmpr)) {
    result <- c(TRUE, TRUE, TRUE)
    return (result)
  }
  #
  # print(cmpr)
  #
  # Step 2&3
  if (sum(cmpr) == 0) {
    result <- c(FALSE, FALSE, FALSE)
    return (result)
  }
  if (sum(cmpr) == 1) {
    result[which(cmpr)] <- (p[which(cmpr)] <= w[which(cmpr)]*alpha)
    return (result)
  }
  if (sum(cmpr) == 2) {
    vec01 <- rep(0, times=3)
    vec01[which(cmpr)] <- 1
    rdcG <- graphUpdate(w=w, G=G, vec01=vec01)
    # print(vec01); print(rdcG)
    cmprAB <- (p[which(cmpr)] <= rdcG$w * alpha)
    #
    tidx <- which(cmpr)
    fidx <- which(!cmpr)
    cmprS <- rep(FALSE, times=length(tidx))
    for (i in 1:length(tidx)) {
      vertex <- c(tidx[i],fidx)
      vec01 <- rep(0, times=3)
      vec01[vertex] <- 1
      rdcG <- graphUpdate(w=w, G=G, vec01=vec01)
      # print(vec01); print(rdcG)
      if (tidx[i] < fidx) {
        cmprS[i] <- (p[tidx[i]] <= rdcG$w[1] * alpha)
      } else {
        cmprS[i] <- (p[tidx[i]] <= rdcG$w[2] * alpha)
      }
    } # End of for i
    cmprNew <- c(cmprAB,cmprS)
    # print(cmprNew)
    if (all(cmprNew)) {
      result[tidx] <- c(TRUE, TRUE)
      return (result)
    } else {
      result <- (p <= w*alpha)
      return (result)
    }
  }
}

###
#' @title Graphical approach for three hypotheses
#' @param w a vector of initial weights
#' @param G a matrix of initial transaction weights
#' @param p a vector of p-values
#' @param alpha a number of significance level
#' @return a logical vector indicating whether the hypothesis is rejected: TRUE = rejected, FALSE = accepted
#' @author Jiangtao Gou
#' @export
#' @references
#' Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical approach to sequentially rejective multiple test procedures. Statistics in Medicine 28, 586–-604. <doi:10.1002/sim.3495>
#' @examples
#' w <- c(0.3,0.5,0.2)
#' G <- matrix(c(0,1/3,2/3, 1/2,0,1/2, 1/5,4/5,0),nrow=3,byrow=TRUE)
#' p <- c(0.012, 0.051, 0.021)
#' p <- c(0.012, 0.051, 0.019)
#' alpha <- 0.05
#' ga3h(w=w,G=G,p=p, alpha=alpha)
#'
ga3h <- function (w,G,p,alpha) {
  # Step 1
  result <- logical(length=3L)
  result <- (p <= w*alpha)
  if (all(!result)) {
    return (result)
  }
  if (all(result)) {
    return (result)
  }
  #
  vec01 <- rep(0, times=3)
  vec01[which(!result)] <- 1
  rdcG <- graphUpdate(w=w, G=G, vec01=vec01)
  if (sum(vec01) == 2) {
    rejidx <- which(result)
    p_undetermined <- p[-rejidx]
    cmpr <- (p_undetermined <= (rdcG$w * alpha))
    if (all(!cmpr)) {
      return (result)
    } # All FALSE
    result[-rejidx] <- cmpr
    if (all(result)) {
      return (result)
    } # All TRUE
    # only the case Two TRUE and One FALSE is left
  }
  vec01 <- rep(0, times=3)
  vec01[which(!result)] <- 1
  rdcG <- graphUpdate(w=w, G=G, vec01=vec01)
  idx_undetermined <- which(!result)
  result[idx_undetermined] <- (p[idx_undetermined] <= rdcG$w * alpha)
  return(result)
}
