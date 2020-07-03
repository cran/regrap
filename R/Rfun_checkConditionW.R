# Rfun_checkConditionW
# 2020-03-31
#
#' @name checkConditionW
#' @title Condition check for weak FWER control in RGA
#' @description Check the sufficient condition in RGA for the weak FWER control.
#' @param w a vector of initial weights
#' @param G a matrix of initial transaction weights
#' @return a logical value indicating whether the RGA's conditions are satisfied or not for the weak FWER control
#' @export
#' @import stats
#' @author Jiangtao Gou
#' @author Fengqing Zhang
#' @details The conditions verified here are sufficient conditions. If a logical value \code{TRUE} is returned, then the weak control of the FWER is guaranteed. The weak control of the FWER may still hold even if the output is \code{FALSE}. 
#' @examples
#' w <- c(0.31, 0.33, 0.36)
#' G <- matrix(c(0,0.4,0.6, 0.7,0,0.3, 0.5,0.5,0),nrow=3,byrow=TRUE)
#' checkConditionW(w=w,G=G)
#' w <- c(0.5,0.5,0)
#' G <- matrix(c(0,1,0, 0.25,0,0.75, 1,0,0), nrow=3, byrow=TRUE)
#' checkConditionW(w=w,G=G)
# 
#
checkConditionW <- function(w,G){
  n <-length(w)
  wsum <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      wsum <- wsum + w[i]*w[j]
    }
  } # End of for i
  wgsum <- 0
  for (k in 1:n) {
    for (i in 1:(n-1)) {
      if (i != k) {
        for (j in (i+1):n) {
          if (j != k) {
          wgsum <- wgsum + w[k]*w[k]*G[k,i]*G[k,j]
          }
        }
      }
    }
  } # End of for k
  # print(c(wsum,wgsum))
  logical <- (wsum >= wgsum)
  return(logical)
}