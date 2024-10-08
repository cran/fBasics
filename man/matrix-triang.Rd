\name{triang}
\alias{triang}

\alias{Triang}


\title{Upper and lower triangular matrices}

\description{
    
  Extracs the upper or lower triangular part from a matrix.    

}

\usage{
triang(x)
Triang(x)
}

\arguments{

  \item{x}{
    a numeric matrix.
  }
}

\details{
    
  \code{triang} and \code{Triang} transform a square matrix to a lower
  or upper triangular form. The functions just replace the remaining
  values with zeroes and work with non-square matrices, as well.
  
  A triangular matrix is either an upper triangular matrix or lower
  triangular matrix. For the first case all matrix elements
  \code{a[i,j]} of matrix \code{A} are zero for \code{i>j}, whereas in
  the second case we have just the opposite situation. A lower
  triangular matrix is sometimes also called left triangular.

  In fact, triangular matrices are so useful that much of computational
  linear algebra begins with factoring or decomposing a general matrix
  or matrices into triangular form. Some matrix factorization methods
  are the Cholesky factorization and the LU-factorization. Even
  including the factorization step, enough later operations are
  typically avoided to yield an overall time savings.

  Triangular matrices have the following properties: the inverse of a
  triangular matrix is a triangular matrix, the product of two
  triangular matrices is a triangular matrix, the determinant of a
  triangular matrix is the product of the diagonal elements, the
  eigenvalues of a triangular matrix are the diagonal elements.
    
}

\value{
  a matrix of the same dimensions as \code{x} with the elements above or
  below the main diagonal set to zeroes
}

\references{

Higham, N.J., (2002);
    \emph{Accuracy and Stability of Numerical Algorithms}, 
    2nd ed., SIAM.

Golub, van Loan, (1996);
    \emph{Matrix Computations}, 
    3rd edition. Johns Hopkins University Press. 
  
}

\examples{
## Create Pascal Matrix:
P = pascal(3)
P
   
## Create lower triangle matrix
L = triang(P)
L                                
}

\keyword{math}
