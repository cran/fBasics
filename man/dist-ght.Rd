\name{ght}

\alias{dght}
\alias{pght}
\alias{qght}
\alias{rght}


\concept{generalized hyperbolic Student-t distribution}


\title{Generalized Hyperbolic Student-t distribution}

\description{
    
  Density, distribution function, quantile function and random
  generation for the generalized hyperbolic Student-t distribution.
    
}

\usage{
dght(x, beta = 0.1, delta = 1, mu = 0, nu = 10, log = FALSE)
pght(q, beta = 0.1, delta = 1, mu = 0, nu = 10)
qght(p, beta = 0.1, delta = 1, mu = 0, nu = 10)
rght(n, beta = 0.1, delta = 1, mu = 0, nu = 10)
}

\arguments{

  \item{x, q}{
    a numeric vector of quantiles.
  }
  \item{p}{
    a numeric vector of probabilities.
  }
  \item{n}{
    number of observations.
  }
  \item{beta}{
    numeric value, the skewness parameter in the range \code{(0, alpha)}.
  }
  \item{delta}{
    numeric value, the scale parameter, must be zero or positive.
  }
  \item{mu}{
    numeric value, the location parameter, by default 0.
  }
  \item{nu}{

    a numeric value, the number of degrees of freedom.  Note,
    \code{alpha} takes the limit of \code{abs(beta)}, and \code{lambda=-nu/2}.

  }
  \item{log}{
    a logical, if TRUE, probabilities \code{p} are given as \code{log(p)}.
  }
}

\details{

  \code{dght} gives the density,
  \code{pght} gives the distribution function,
  \code{qght} gives the quantile function, and
  \code{rght} generates random deviates.
    
  The parameters are as in the first parameterization.

}

\value{
  numeric vector
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
## ght -
   #
}

\keyword{distribution}
