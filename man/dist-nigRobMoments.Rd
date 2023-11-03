\name{nigRobMoments}
\alias{nigRobMoments}

\alias{nigMED}
\alias{nigIQR}
\alias{nigSKEW}
\alias{nigKURT}


\title{Robust Moments for the NIG}

\description{

    Computes the first four robust moments for the
    Normal Inverse Gaussian Distribution.

}

\usage{
nigMED(alpha = 1, beta = 0, delta = 1, mu = 0)
nigIQR(alpha = 1, beta = 0, delta = 1, mu = 0)
nigSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)
nigKURT(alpha = 1, beta = 0, delta = 1, mu = 0)
}

\arguments{
    \item{alpha}{
      shape parameter.
    }
    \item{beta}{
      skewness parameter \code{beta}, \code{abs(beta)} is in the range
      \code{(0, alpha)}.
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
  by dropping the \code{nig} prefix from the name of the corresponding
  function.

}

\author{
  Diethelm Wuertz.
}

\examples{
## nigMED -
   # Median:
   nigMED(alpha = 1, beta = 0, delta = 1, mu = 0)

## nigIQR -
   # Inter-quartile Range:
   nigIQR(alpha = 1, beta = 0, delta = 1, mu = 0)

## nigSKEW -
   # Robust Skewness:
   nigSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)

## nigKURT -
   # Robust Kurtosis:
   nigKURT(alpha = 1, beta = 0, delta = 1, mu = 0)
}

\keyword{distribution}
