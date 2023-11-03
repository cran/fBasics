\name{hilbert}
\alias{hilbert}


\title{Hilbert matrix}

\description{
  Creates a Hilbert matrix.
}

\usage{
hilbert(n)
}

\arguments{

  \item{n}{
    an integer value, the dimension of the square matrix.
  }
}

\details{

  An \eqn{n,n} matrix with \eqn{(i,j)}th element equal to
  \eqn{1/(i+j-1)} is said to be a Hilbert matrix of order \eqn{n}.
  Hilbert matrices are symmetric and positive definite.

  They are canonical examples of ill-conditioned matrices, making them
  notoriously difficult to use in numerical computation. For example,
  the 2-norm condition number of a 5x5 Hilbert matrix above is about
  4.8e5.
    
}

\value{
  a matrix
}

\references{

Hilbert D., 
    \emph{Collected papers}, vol. II, article 21.
    
Beckermann B, (2000);
    \emph{The condition number of real Vandermonde, Krylov and positive 
        definite Hankel matrices}, 
    Numerische Mathematik 85, 553--577, 2000.
    
Choi, M.D., (1983);
    \emph{Tricks or Treats with the Hilbert Matrix},
    American Mathematical Monthly 90, 301--312, 1983.
    
Todd, J., (1954);
    \emph{The Condition Number of the Finite Segment of the Hilbert Matrix},
    National Bureau of Standards, Applied Mathematics Series 39, 109--116. 
    
Wilf, H.S., (1970);
    \emph{Finite Sections of Some Classical Inequalities}, 
    Heidelberg, Springer.

}

\examples{
## Create a Hilbert Matrix:
   H = hilbert(5)
   H                              
}

\keyword{math}
