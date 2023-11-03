\name{ghtRobMoments}
\alias{ghtRobMoments}

\alias{ghtMED}
\alias{ghtIQR}
\alias{ghtSKEW}
\alias{ghtKURT}


\title{Robust Moments for the GHT}

\description{

  Computes the first four robust moments for the
  generalized hyperbolic Student-t.

}

\usage{
ghtMED(beta = 0.1, delta = 1, mu = 0, nu = 10)
ghtIQR(beta = 0.1, delta = 1, mu = 0, nu = 10)
ghtSKEW(beta = 0.1, delta = 1, mu = 0, nu = 10)
ghtKURT(beta = 0.1, delta = 1, mu = 0, nu = 10)
}

\arguments{
    \item{beta}{skewness parameter in the range \code{(0, alpha)}.}
    \item{delta}{scale parameter, must be zero or positive.}
    \item{mu}{location parameter, by default 0.}
    \item{nu}{

      a numeric value, the number of degrees of freedom.
      Note, \code{alpha} takes the limit of \code{abs(beta)},
      and \code{lambda=-nu/2}.

    }
}

\details{

  The parameters are those of the first parameterization.

}

\value{

  a named numerical value.  The name is
  one of \code{MED}, \code{IQR}, \code{SKEW}, or \code{KURT}, obtained
  by dropping the \code{ght} prefix from the name of the corresponding
  function.

}

\author{
  Diethelm Wuertz.
}

\examples{
## ghtMED -
   # Median:
   ghtMED(beta = 0.1, delta = 1, mu = 0, nu = 10)

## ghtIQR -
   # Inter-quartile Range:
   ghtIQR(beta = 0.1, delta = 1, mu = 0, nu = 10)

## ghtSKEW -
   # Robust Skewness:
   ghtSKEW(beta = 0.1, delta = 1, mu = 0, nu = 10)

## ghtKURT -
   # Robust Kurtosis:
   ghtKURT(beta = 0.1, delta = 1, mu = 0, nu = 10)

}

\keyword{distribution}
