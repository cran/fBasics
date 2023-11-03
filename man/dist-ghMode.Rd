\name{ghMode}
\alias{ghMode}


\title{Generalized Hyperbolic Mode}

\description{
    
  Computes the mode of the generalized hyperbolic function.
    
}

\usage{
ghMode(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
}

\arguments{
  \item{alpha}{
    first shape parameter.
  }
  \item{beta}{
    second shape parameter, should in the range \code{(0, alpha).}
  }
  \item{delta}{
    scale parameter, must be zero or positive.
  }
  \item{mu}{
    location parameter, by default 0.
  }
  \item{lambda}{
    defines the sublclass, by default \eqn{-1/2}.
  }
}

\details{

  The meanings of the parameters correspond to the first
  parameterization, see \code{\link{gh}} for further details.
    
}

\value{
  a numeric value, the mode of the generalized hyperbolic distribution
}

\references{
Atkinson, A.C. (1982); 
    \emph{The simulation of generalized inverse Gaussian and hyperbolic 
    random variables},
    SIAM J. Sci. Stat. Comput. 3, 502--515. 

Barndorff-Nielsen O. (1977);
    \emph{Exponentially decreasing distributions for the logarithm of 
    particle size}, 
    Proc. Roy. Soc. Lond., A353, 401--419. 

Barndorff-Nielsen O., Blaesild, P. (1983); 
    \emph{Hyperbolic distributions. In Encyclopedia of Statistical 
    Sciences}, 
    Eds., Johnson N.L., Kotz S. and Read C.B., 
    Vol. 3, pp. 700--707. New York: Wiley. 

Raible S. (2000);
    \emph{Levy Processes in Finance: Theory, Numerics and Empirical Facts},
    PhD Thesis, University of Freiburg, Germany, 161 pages.
}

\examples{   
## ghMode -
   ghMode()
}

\keyword{distribution}
