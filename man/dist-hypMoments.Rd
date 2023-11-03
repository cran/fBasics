\name{hypMoments}
\alias{hypMoments}

\alias{hypMean}
\alias{hypVar}
\alias{hypSkew}
\alias{hypKurt}
\alias{hypMoments}


\concept{hyperbolic distribution}


\title{Hyperbolic distribution moments}

\description{
    
  Calculates moments of the hyperbolic distribution function.
    
}

\usage{
hypMean(alpha=1, beta=0, delta=1, mu=0)
hypVar(alpha=1, beta=0, delta=1, mu=0)
hypSkew(alpha=1, beta=0, delta=1, mu=0)
hypKurt(alpha=1, beta=0, delta=1, mu=0)

hypMoments(order, type = c("raw", "central", "mu"),
    alpha=1, beta=0, delta=1, mu=0)
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
  \item{order}{
    an integer value, the order of the moment.
  }
  \item{type}{
    a character string, 
    \code{"raw"} returns the moments about zero,
    \code{"central"} returns the central moments about the mean, and
    \code{"mu"} returns the moments about the location parameter \code{mu}.
  }    
}

\value{

  a named numerical value.  The name is one
  of \code{mean}, \code{var}, \code{skew}, or \code{kurt}, obtained by
  dropping the \code{hyp} prefix from the name of the corresponding
  function and lowercasing it.

  for \code{hypMoments}, the name is obtained by \code{paste0("m", order, type)}.

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
## hypMean -
   hypMean(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
   
## ghKurt -
   hypKurt(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
   
## hypMoments -
   hypMoments(4, alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
   hypMoments(4, "central", alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
}

\keyword{distribution}
