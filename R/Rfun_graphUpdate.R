# Rfun_graphUpdate
# 2020-03-31
#
#' @name graphUpdateOne
#' @title Single Step Graph Update
#' @description Update the graph by removing one vertex
#' @param w a numeric vector of vertex weights
#' @param G a matrix of transition weights
#' @param vec01 a binary vector indicating the set of vertices planned to be removed: the vertex corresponding to the first zero in this vector will be removed
#' @return a list of one updated vertex weight vector, one updated transition weight matrix, one updated indicator vector, and a binory TRUE/FALSE indicater to show whether a node has been removed 
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @examples
#' w <- c(0.1,0.2,0.3,0.4)
#' G <- matrix(c(0,0.3,0.3,0.4, 0.6,0,0.2,0.2, 0.5,0.2,0,0.3, 0.3,0.4,0.3,0),nrow=4,byrow=TRUE)
#' vec01 <- c(1,0,0,1)
#' graphUpdateOne(w=w,G=G,vec01=vec01)
#' 
#
graphUpdateOne <- function (w, G, vec01) {
  n <-length(w)
  locZero <- which(vec01 == 0)
  if (length(locZero) == 0) {
    result <- list(w=w, G=G, vec01=vec01, isNodeRemoved=FALSE)
    return (result)
  }
  rmIndex <- locZero[1]
  #
  for (i in 1:n) {
    if (i != rmIndex) {
      w[i] <- w[i] + w[rmIndex]*G[rmIndex,i]
      for (j in 1:n) {
        if ( (j != i) & (j != rmIndex)) {
          G[i,j] <- (G[i,j] + G[i,rmIndex]*G[rmIndex,j])/(1-G[i,rmIndex]*G[rmIndex,i])
        }
      } # End of for j
    }
  } #End of for i
  result <- list(w=w[-rmIndex], G=G[-rmIndex,-rmIndex], vec01=vec01[-rmIndex], isNodeRemoved=TRUE)
  return (result)
}

#
#' @name graphUpdate
#' @title Graph Update
#' @description Update the graph by removing a set of vertices
#' @param w a numeric vector of vertex weights
#' @param G a matrix of transition weights
#' @param vec01 a binary vector indicating the set of vertices planned to be removed: the vertex corresponding to the zeros in this vector will be removed
#' @return a list of one updated vertex weight vector, one updated transition weight matrix, and a binory TRUE/FALSE indicater to show whether a node has been removed 
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @examples
#' w <- c(0.1,0.2,0.3,0.4)
#' G <- matrix(c(0,0.3,0.3,0.4, 0.6,0,0.2,0.2, 0.5,0.2,0,0.3, 0.3,0.4,0.3,0),nrow=4,byrow=TRUE)
#' vec01 <- c(1,0,0,1)
#' graphUpdate(w=w,G=G,vec01=vec01)
#' 
###
graphUpdate <- function (w, G, vec01) {
  n <-length(w)
  locZero <- which(vec01 == 0)
  if (length(locZero) == 0) {
    result <- list(w=w, G=G, isNodeRemoved=FALSE)
    return (result)
  }
  #
  for (k in 1:length(locZero)) {
    tmp <- graphUpdateOne(w=w,G=G,vec01=vec01)
    w <- tmp$w
    G <- tmp$G
    vec01 <- tmp$vec01
  }
  result <- list(w=w, G=G, isNodeRemoved=TRUE)
  return (result)
}