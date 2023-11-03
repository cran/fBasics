\name{sgh}
\alias{sgh}

\alias{dsgh}
\alias{psgh}
\alias{qsgh}
\alias{rsgh}


\concept{standardized generalized hyperbolic distribution}


\title{Standardized Generalized Hyperbolic Distribution}

\description{
    
  Density, distribution function, quantile function and random
  generation for the standardized generalized hyperbolic distribution.
    
}

\usage{
dsgh(x, zeta = 1, rho = 0, lambda = 1, log = FALSE)
psgh(q, zeta = 1, rho = 0, lambda = 1)
qsgh(p, zeta = 1, rho = 0, lambda = 1)
rsgh(n, zeta = 1, rho = 0, lambda = 1)
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
  \item{zeta}{shape parameter, a positive number.}
  \item{rho}{skewness parameter, a number in the range \eqn{(-1, 1)}.}
  \item{lambda}{
    ??
  }
  \item{log}{
    a logical flag by default \code{FALSE}. 
    If \code{TRUE}, log values are returned.
  }
}

\value{
  numeric vector
}

\details{
  \code{dsgh} gives the density,
  \code{psgh} gives the distribution function,
  \code{qsgh} gives the quantile function, and
  \code{rsgh} generates random deviates.
    
  The generator \code{rsgh} is based on the GH algorithm given by Scott (2004).
}

\author{
  Diethelm Wuertz
}

\examples{   
## rsgh -
   set.seed(1953)
   r = rsgh(5000, zeta = 1, rho = 0.5, lambda = 1)
   plot(r, type = "l", col = "steelblue",
     main = "gh: zeta=1 rho=0.5 lambda=1")
 
## dsgh - 
   # Plot empirical density and compare with true density:
   hist(r, n = 50, probability = TRUE, border = "white", col = "steelblue",
     ylim = c(0, 0.6))
   x = seq(-5, 5, length = 501)
   lines(x, dsgh(x, zeta = 1, rho = 0.5, lambda = 1))
 
## psgh -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, psgh(x, zeta = 1, rho = 0.5, lambda = 1))
   
## qsgh -
   # Compute Quantiles:
   round(qsgh(psgh(seq(-5, 5, 1), zeta = 1, rho = 0.5), zeta = 1, rho = 0.5), 4)
}

\keyword{distribution}
