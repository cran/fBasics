\name{vec}
\alias{vec}

\alias{vech}


\title{Stacking vectors and matrices}

\description{
    
  Stacks either a lower triangle matrix or a matrix.
     
}

\usage{
vec(x)
vech(x)
}

\arguments{
   
  \item{x}{
    a numeric matrix.
  }
}

\details{
    
  The function \code{vec} implements the operator that stacks a matrix
  as a column vector, to be more precise in a matrix with one column.
  \eqn{vec(X) = (X_{11}, X_{21}, ..., X_{N1}, X_{12}, X_{22}, ..., X_{NN})}.
    
  The function \code{vech} implements the operator that stacks the lower
  triangle of a NxN matrix as an N(N+1)/2x1 vector:
  \eqn{vech(X) =(X_{11}, X_{21}, X_{22}, X_{31}, ..., X_{NN})},
  to be more precise in a matrix with one row.
  
}

\examples{
## Create Pascal Matrix:
P = pascal(3)
   
## Stack the matrix
vec(P) 
   
## Stack the lower triangle
vech(P)                                        
}

\keyword{math}
