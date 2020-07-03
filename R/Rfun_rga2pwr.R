# Rfun_rga2pwr
# 2020-04-11
#
#
#' @name rga2pwr
#' @title Power Analysis for Graphical Approaches and Reverse Graphical Approaches with Two Hypotheses
#' @description Power Analysis for Graphical Approaches and Reverse Graphical Approaches with Two Hypotheses
#' @param w a vector of initial weights
#' @param G a matrix of initial transaction weights
#' @param alpha a number of significance level
#' @param delta a vector of effect sizes
#' @param corr a correlation matrix
#' @param method a string specified the method: "rga" for Reverse Graphical Approaches and "ga" for Graphical Approaches
#' @return a numerical matrix including the probabilities of four combinations of being rejected and being accepted for two hypotheses. Row indices stand for the first hypothesis, and column indices stand for the second hypothesis. The first index stands for the probability of acceptance, and the second index stands for the probability of rejection.
#' @export
#' @import mvtnorm
#' @author Jiangtao Gou
#' @references
#' Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical approach to sequentially rejective multiple test procedures. Statistics in Medicine 28, 586–-604. <doi:10.1002/sim.3495>
#'
#' Gou, J. (2020). Reverse graphical approaches for multiple test procedures. Technical Report.
#'
#' @examples
#' w <- c(0.3,0.7)
#' G <- matrix(c(0,1,1,0),nrow=2,byrow=TRUE)
#' alpha <- 0.05
#' delta <- c(0,2)
#' rho <- 0.0
#' corr <- matrix(c(1,rho,rho,1), nrow=2)
#' method="rga"
#' rga2pwr(w=w, G=G, alpha=alpha, delta=delta, corr=corr, method=method)
#'
#'
#'
#
rga2pwr <- function (w, G, alpha, delta, corr, method="rga") {
  result <- matrix(numeric(length=4L),nrow=2)
  #
  meanV <- (-delta)
  corrM <- corr
  lowerB <- c(qnorm(sum(w)*alpha), -Inf)
  upperB <- c(Inf, qnorm(w[2]*alpha))
  tempIntgl <- mvtnorm::pmvnorm(lowerB,upperB,meanV,corrM,algorithm=Miwa(steps=128))
  result[1,2] <- tempIntgl[1]
  #
  meanV <- (-delta)
  corrM <- corr
  lowerB <- c(-Inf, qnorm(sum(w)*alpha))
  upperB <- c(qnorm(w[1]*alpha), Inf)
  tempIntgl <- mvtnorm::pmvnorm(lowerB,upperB,meanV,corrM,algorithm=Miwa(steps=128))
  result[2,1] <- tempIntgl[1]
  #
  if (method == "rga") {
    meanV <- (-delta)
    corrM <- corr
    lowerB <- c(-Inf, -Inf)
    upperB <- c(qnorm(sum(w)*alpha), qnorm(sum(w)*alpha))
    tempIntgl <- mvtnorm::pmvnorm(lowerB,upperB,meanV,corrM,algorithm=Miwa(steps=128))
    result[2,2] <- tempIntgl[1]
    result[1,1] <- 1 - result[1,2] - result[2,1] - result[2,2]
  } else if (method == "ga") {
    meanV <- (-delta)
    corrM <- corr
    lowerB <- c(qnorm(w[1]*alpha), qnorm(w[2]*alpha))
    upperB <- c(Inf, Inf)
    tempIntgl <- mvtnorm::pmvnorm(lowerB,upperB,meanV,corrM,algorithm=Miwa(steps=128))
    result[1,1] <- tempIntgl[1]
    result[2,2] <- 1 - result[1,2] - result[2,1] - result[1,1]
  } else {
    result[1,1] <- NA
    result[2,2] <- NA
  }
  return (result)
}
