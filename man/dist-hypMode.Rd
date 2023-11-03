\name{hypMode}
\alias{hypMode}


\concept{hyperbolic distribution}
\concept{mode}


\title{Hyperbolic mode}

\description{
    
  Computes the mode of the hyperbolic distribution.
    
}

\usage{
hypMode(alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1)
}

\arguments{
  \item{alpha}{
    
    shape parameter, a positive number. \code{alpha} can also be a
    vector of length four, containing \code{alpha}, \code{beta},
    \code{delta} and \code{mu} (in that order).
    
  }
  \item{beta}{

    skewness parameter, \code{abs(beta)} is in the 
    range \code{(0, alpha)}.

  }
  \item{delta}{
    scale parameter, must be zero or positive.
  }
  \item{mu}{
    location parameter, by default 0.
  }
  \item{pm}{

    an integer value between \code{1} and \code{4} for the selection of
    the parameterization. The default takes the first parameterization.

  }
  
}

\value{

  a numeric value, the mode in the appropriate parameterization for the
  hyperbolic distribution.
    
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

\author{
  David Scott for code implemented from \R's contributed package
  \code{HyperbolicDist}.
}

\examples{   
## hypMode -
   hypMode()
}

\keyword{distribution}
