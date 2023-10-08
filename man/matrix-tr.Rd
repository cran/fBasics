\name{tr}
\alias{tr}


\title{Trace of a matrix}

\description{
    Computes the trace of a matrix.
}

\usage{
tr(x)
}

\arguments{

  \item{x}{
    a numeric matrix.
  }
        
}

\details{
    
    \code{tr} computes the trace of a square matrix, i.e., the sum of
    its diagonal elements.

    If the matrix is not square, \code{tr} returns \code{NA}.
  
}

\references{

Golub, van Loan, (1996);
    \emph{Matrix Computations}, 
    3rd edition. Johns Hopkins University Press. 
    
}

\examples{
## Create Pascal Matrix:
   P = pascal(3)
   P
  
## Trace:
   tr(P)                                  
}

\keyword{math}
