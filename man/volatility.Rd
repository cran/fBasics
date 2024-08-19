\name{volatility}
\alias{volatility}
\alias{volatility.default}

\title{Compute volatility}

\description{

  Generic function for volatility computations.
    
}

\usage{
volatility(object, \dots)

\method{volatility}{default}(object, \dots)
}

\arguments{
  \item{object}{
    an object from which to extract or compute the volatility.
  }

  \item{\dots}{
    arguments for methods. Ignored by the default method.
  }
}

\details{

  \code{volatility} is a generic function, whose default method centers
  and squares the values in \code{object}. Other packages can (and do)
  define methods for it.

}

\keyword{stats}
