\name{kron}
\alias{kron}


\title{Kronecker product}

\description{
  Computes the Kronecker product of two matrices.
}

\usage{
kron(x, y)
}

\arguments{

  \item{x, y}{
    numeric matrices.
  }
}

\details{
    
  The \emph{Kronecker product} can be computed using the operator 
  \code{\%x\%} or alternatively using the function \code{kron}
  for SPlus compatibility.
   
}

\note{
  \code{kron} is a synonym to \code{\%x\%}.
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
   
## Return the Kronecker Product                     
   kron(P, diag(3))
   P \%x\% diag(3)                        
                           
}

\keyword{math}
