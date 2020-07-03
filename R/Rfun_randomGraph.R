# Rfun_randomGraph
# 2020-04-01
#
#' @name randomGraph
#' @title Generate a Random Graph
#' @description Generate a random graph from uniform distribution
#' @param n an integer: number of vertices
#' @param seed an integer: a seed for random number generator
#' @param wlim a vector of two numbers: range of vertex weights
#' @param Glim a vector of two numbers: range of transition weights
#' @return A list of one vector for vertex weights and one matrix for transition weights
#' @author Jiangtao Gou
#' @author Fengqing Zhang
#' @export
#' @import stats
#' @examples
#' wG <- randomGraph(n=5)
#
randomGraph <- function (n, seed=as.numeric(Sys.time()), wlim=c(0,1), Glim=c(0,1)) {
  set.seed(seed=seed)
  wtmp <- runif(n=n, min=wlim[1], max=wlim[2])
  w <- wtmp/sum(wtmp)
  G <- matrix(rep(0,times=n*n),nrow=n)
  for (k in 1:n) {
    Gtmp <- runif(n=n-1, min=Glim[1], max=Glim[2])
    Gvec <- Gtmp/sum(Gtmp)
    G[k,-k] <- Gvec
  } # End of for k
  result <- list(w=w, G=G)
  return (result)
}
