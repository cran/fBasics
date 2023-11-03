\name{normRobMoments}
\alias{normRobMoments}

\alias{normMED}
\alias{normIQR}
\alias{normSKEW}
\alias{normKURT}


\title{Robust moments for the Normal distribution}

\description{

    Computes the first four robust moments for the
    Normal distribution.

}

\usage{
normMED(mean = 0, sd = 1)
normIQR(mean = 0, sd = 1)
normSKEW(mean = 0, sd = 1)
normKURT(mean = 0, sd = 1)
}

\arguments{
  \item{mean}{
    locaiton parameter.
  }
  \item{sd}{
    scale parameter.
  }
}

\value{

  a named numerical value.  The name is one of \code{MED}, \code{IQR},
  \code{SKEW}, or \code{KURT}, obtained by dropping the \code{gh} prefix
  from the name of the corresponding function.

}

\author{
  Diethelm Wuertz
}

\examples{
## normMED -
   # Median:
   normMED(mean = 0, sd = 1)

## normIQR -
   # Inter-quartile Range:
   normIQR(mean = 0, sd = 1)

## normSKEW -
   # Robust Skewness:
   normSKEW(mean = 0, sd = 1)

## normKURT -
   # Robust Kurtosis:
   normKURT(mean = 0, sd = 1)

}

\keyword{distribution}
