\name{hypRobMoments}
\alias{hypRobMoments}

\alias{hypMED}
\alias{hypIQR}
\alias{hypSKEW}
\alias{hypKURT}


\concept{hyperbolic distribution}


\title{Robust moments for the HYP}

\description{

  Computes the first four robust moments for the hyperbolic
  distribution.

}

\usage{
hypMED(alpha = 1, beta = 0, delta = 1, mu = 0)
hypIQR(alpha = 1, beta = 0, delta = 1, mu = 0)
hypSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)
hypKURT(alpha = 1, beta = 0, delta = 1, mu = 0)
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
}

\value{

  a named numerical value.  The name is
  one of \code{MED}, \code{IQR}, \code{SKEW}, or \code{KURT}, obtained
  by dropping the \code{hyp} prefix from the name of the corresponding
  function.

}

\author{
  Diethelm Wuertz
}

\examples{
## hypMED -
   # Median:
   hypMED(alpha = 1, beta = 0, delta = 1, mu = 0)

## hypIQR -
   # Inter-quartile Range:
   hypIQR(alpha = 1, beta = 0, delta = 1, mu = 0)

## hypSKEW -
   # Robust Skewness:
   hypSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)

## hypKURT -
   # Robust Kurtosis:
   hypKURT(alpha = 1, beta = 0, delta = 1, mu = 0)

}

\keyword{distribution}
