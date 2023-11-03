\name{ghMoments}
\alias{ghMoments}

\alias{ghMean}
\alias{ghVar}
\alias{ghSkew}
\alias{ghKurt}
\alias{ghMoments}


\title{Generalized Hyperbolic Distribution Moments}

\description{
  Calculates moments of the generalized hyperbolic distribution.
}

\usage{
ghMean(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)
ghVar(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)
ghSkew(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)
ghKurt(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)

ghMoments(order, type = c("raw", "central", "mu"),
    alpha = 1, beta=0, delta=1, mu=0, lambda=-1/2)
}

\arguments{

  \item{alpha}{
    numeric value, the first shape parameter.
  }
  \item{beta}{
    numeric value, the second shape parameter in the range \code{(0, alpha)}.
  }
  \item{delta}{
    numeric value, the scale parameter, must be zero or positive.
  }
  \item{mu}{
    numeric value, the location parameter, by default 0.
  }
  \item{lambda}{
    numeric value, defines the sublclass, by default \eqn{-1/2}.
  }
  \item{order}{
    an integer value, the order of the moment.
  }
  \item{type}{
    a character value, 
    \code{"raw"} gives the moments about zero,
    \code{"central"} gives the central moments about the mean, and
    \code{"mu"} gives the moments about the location parameter \code{mu}.
  }
}

\value{
  a named numerical value.  The name is one
  of \code{mean}, \code{var}, \code{skew}, or \code{kurt}, obtained by
  dropping the \code{nig} prefix from the name of the corresponding
  function and lowercasing it.

  for \code{ghMoments}, the name is obtained by \code{paste0("m", order, type)}.
}

\references{
Scott, D. J., Wuertz, D. and Tran, T. T. (2008) 
    \emph{Moments of the Generalized Hyperbolic Distribution}. 
    Preprint. 
}

\author{
  Diethelm Wuertz
}

\examples{   
## ghMean -
   ghMean(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
   
## ghKurt -
   ghKurt(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
   
## ghMoments -
   ghMoments(4, 
     alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
   ghMoments(4, "central",
     alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
}

\keyword{distribution}
