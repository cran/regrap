% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rfun_graphUpdate.R
\name{graphUpdate}
\alias{graphUpdate}
\title{Graph Update}
\usage{
graphUpdate(w, G, vec01)
}
\arguments{
\item{w}{a numeric vector of vertex weights}

\item{G}{a matrix of transition weights}

\item{vec01}{a binary vector indicating the set of vertices planned to be removed: the vertex corresponding to the zeros in this vector will be removed}
}
\value{
a list of one updated vertex weight vector, one updated transition weight matrix, and a binory TRUE/FALSE indicater to show whether a node has been removed
}
\description{
Update the graph by removing a set of vertices
}
\examples{
w <- c(0.1,0.2,0.3,0.4)
G <- matrix(c(0,0.3,0.3,0.4, 0.6,0,0.2,0.2, 0.5,0.2,0,0.3, 0.3,0.4,0.3,0),nrow=4,byrow=TRUE)
vec01 <- c(1,0,0,1)
graphUpdate(w=w,G=G,vec01=vec01)

}
\author{
Jiangtao Gou
}
