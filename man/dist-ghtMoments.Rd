\name{ghtMoments}
\alias{ghtMoments}

\alias{ghtMean}
\alias{ghtVar}
\alias{ghtSkew}
\alias{ghtKurt}
\alias{ghtMoments}


\title{Generalized Hyperbolic Student-t Moments}

\description{
    
  Calculates moments of the generalized hyperbolic Student-t
  distribution.
    
}

\usage{
ghtMean(beta=0.1, delta=1, mu=0, nu=10)
ghtVar(beta=0.1, delta=1, mu=0, nu=10)
ghtSkew(beta=0.1, delta=1, mu=0, nu=10)
ghtKurt(beta=0.1, delta=1, mu=0, nu=10)

ghtMoments(order, type = c("raw", "central", "mu"),
    beta=0.1, delta=1, mu=0, nu=10)
}

\arguments{

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

    a numeric value, the number of degrees of freedom.
    Note, \code{alpha} takes the limit of \code{abs(beta)}, 
    and \code{lambda=-nu/2}.

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
  dropping the \code{nig} prefix from the name of the corresponding
  function and lowercasing it.

  for \code{ghtMoments}, the name is obtained by \code{paste0("m", order, type)}.
}

\references{
Scott, D.J., Wuertz, D. and Tran, T.T. (2008) 
    \emph{Moments of the Generalized Hyperbolic Distribution}. 
    Preprint. 
}

\author{
  Diethelm Wuertz
}

\examples{   
## ghtMean -
   ghtMean(beta=0.2, delta=1.2, mu=-0.5, nu=4)
   
## ghtKurt -
   ghtKurt(beta=0.2, delta=1.2, mu=-0.5, nu=4)
   
## ghtMoments -
   ghtMoments(4, 
     beta=0.2, delta=1.2, mu=-0.5, nu=4)
   ghtMoments(4, "central",
     beta=0.2, delta=1.2, mu=-0.5, nu=4)
}

\keyword{distribution}
