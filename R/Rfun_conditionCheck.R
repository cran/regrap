# Rfun_conditionCheck
# 2020-03-31
#
#' @name conditionCheck
#' @title Condition check for strong FWER control in RGA
#' @description Check the sufficient condition in RGA for the strong FWER control.
#' @param w a vector of initial weights
#' @param G a matrix of initial transaction weights
#' @return a logical value indicating whether the RGA's conditions are satisfied or not for the strong FWER control
#' @export
#' @import stats
#' @author Jiangtao Gou
#' @details The conditions verified here are sufficient conditions. If a logical value \code{TRUE} is returned, then the strong control of the FWER is guaranteed. The strong control of the FWER may still hold even if the output is \code{FALSE}. 
#' @examples
#' w <- c(0.1,0.2,0.3,0.4)
#' G <- matrix(c(0,0.3,0.3,0.4, 0.6,0,0.2,0.2, 0.5,0.2,0,0.3, 0.3,0.4,0.3,0),nrow=4,byrow=TRUE)
#' conditionCheck(w=w,G=G)
#
conditionCheck <- function (w, G) {
  n <-length(w)
  logical <- TRUE
  #
  MyIntToBit <- function(x, dig) {
    i <- 0L
    string <- numeric(dig)
    while (x > 0) {
      string[dig - i] <- x %% 2L
      x <- x %/% 2L
      i <- i + 1L
    }
    string
  } # End of function MyIntToBit
  #
  list01 <- sapply(0:(2^n - 1), function(x) MyIntToBit(x,dig=n))
  # print(list01)
  for (k in 1:2^n) {
    vec01 <- list01[,k]
    if (sum(vec01) >= 3) {
      smallgraph <- graphUpdate(w=w, G=G, vec01=vec01)
      logical <- checkConditionW(w=smallgraph$w,G=smallgraph$G)
      if (!logical) {
        return (logical)
      }
    }
  } # End of if k
  #
  return (logical)
}
#<https://chenqx.github.io/2014/09/29/Algorithm-Recursive-Programming/>
#<https://stackoverflow.com/questions/18705153/generate-list-of-all-possible-combinations-of-elements-of-vector>