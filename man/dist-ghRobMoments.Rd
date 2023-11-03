\name{ghRobMoments}
\alias{ghRobMoments}

\alias{ghMED}
\alias{ghIQR}
\alias{ghSKEW}
\alias{ghKURT}


\title{Robust Moments for the GH}

\description{

  Computes the first four robust moments for the generalized hyperbolic
  distribution.

}

\usage{
ghMED(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
ghIQR(alpha= 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
ghSKEW(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
ghKURT(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
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

  a named numerical value.  The name is
  one of \code{MED}, \code{IQR}, \code{SKEW}, or \code{KURT}, obtained
  by dropping the \code{gh} prefix from the name of the corresponding
  function.

}

\author{
  Diethelm Wuertz.
}

\examples{
## ghMED -
   # Median:
   ghMED(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

## ghIQR -
   # Inter-quartile Range:
   ghIQR(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

## ghSKEW -
   # Robust Skewness:
   ghSKEW(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

## ghKURT -
   # Robust Kurtosis:
   ghKURT(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

}

\keyword{distribution}
