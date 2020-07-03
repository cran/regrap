# Rfun_rga2h
# rga2h, ga2h
# 2020-04-11
#
#' @name rga2h
#' @title reverse graphical approach for two hypotheses
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
#' w <- c(0.3,0.7)
#' G <- matrix(c(0,1,1,0),nrow=2,byrow=TRUE)
#' p <- c(0.032, 0.038)
#' alpha <- 0.05
#' rga2h(w=w,G=G,p=p, alpha=alpha)
#'
rga2h <- function(w,G,p,alpha){
  result <- logical(length=2L)
  TF1 <- (p[1] <= (w[1] + G[2,1]*w[2])*alpha)
  TF2 <- (p[2] <= (w[2] + G[1,2]*w[1])*alpha)
  if (TF1 & TF2) {
    result <- c(TRUE, TRUE)
    return (result)
  } else if (p[1] <= w[1]*alpha) {
    result <- c(TRUE, FALSE)
    return ( result)
  } else if (p[2] <= w[2]*alpha) {
    result <- c(FALSE, TRUE)
    return (result)
  } else {
    result <- c(FALSE, FALSE)
    return (result)
  }
}
#'
#' @name ga2h
#' @title Graphical approach for two hypotheses
#' @param w a vector of initial weights
#' @param G a matrix of initial transaction weights
#' @param p a vector of p-values
#' @param alpha a number of significance level
#' @return a logical vector indicating whether the hypothesis is rejected: TRUE = rejected, FALSE = accepted
#' @export
#' @references
#' Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical approach to sequentially rejective multiple test procedures. Statistics in Medicine 28, 586–-604. <doi:10.1002/sim.3495>
#'
#' @examples
#' w <- c(0.3,0.7)
#' G <- matrix(c(0,1,1,0),nrow=2,byrow=TRUE)
#' p <- c(0.032, 0.038)
#' alpha <- 0.05
#' ga2h(w=w,G=G,p=p, alpha=alpha)
#'
ga2h <- function(w,G,p,alpha){
  result <- logical(length=2L)
  if (p[1] <= w[1]*alpha) {
    result[1] <- TRUE
    TF2 <- (p[2] <= (w[2] + G[1,2]*w[1])*alpha)
    if (TF2) {
      result[2] <- TRUE
    } else {
      result[2] <- FALSE
    }
  } else if (p[2] <= w[2]*alpha) {
    result[2] <- TRUE
    TF1 <- (p[1] <= (w[1] + G[2,1]*w[2])*alpha)
    if (TF1) {
      result[1] <- TRUE
    } else {
      result[1] <- FALSE
    }
  } else {
    result <- c(FALSE, FALSE)
  }
  return (result)
}

