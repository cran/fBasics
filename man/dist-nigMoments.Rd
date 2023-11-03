\name{nigMoments}
\alias{nigMoments}

\alias{nigMean}
\alias{nigVar}
\alias{nigSkew}
\alias{nigKurt}


\title{Moments for the Normal Inverse Gaussian}

\description{
    
    Computes the first four moments for the
    normal inverse Gaussian distribution. 
    
}

\usage{
nigMean(alpha = 1, beta = 0, delta = 1, mu = 0)
nigVar(alpha = 1, beta = 0, delta = 1, mu = 0)
nigSkew(alpha = 1, beta = 0, delta = 1, mu = 0)
nigKurt(alpha = 1, beta = 0, delta = 1, mu = 0)
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

  a named numerical value.  The name is one
  of \code{mean}, \code{var}, \code{skew}, or \code{kurt}, obtained by
  dropping the \code{nig} prefix from the name of the corresponding
  function and lowercasing it.
  
}

\references{
Scott, D. J., Wuertz, D. and Tran, T. T. (2008) 
    \emph{Moments of the Generalized Hyperbolic Distribution}. 
    Preprint. 
}

\author{
  Diethelm Wuertz.
}

\examples{   
## nigMean -
   # Median:
   nigMean(alpha = 1, beta = 0, delta = 1, mu = 0)
 
## nigVar - 
   # Inter-quartile Range:
   nigVar(alpha = 1, beta = 0, delta = 1, mu = 0)
 
## nigSKEW -  
   # Robust Skewness:
   nigSkew(alpha = 1, beta = 0, delta = 1, mu = 0)
   
## nigKurt -
   # Robust Kurtosis:
   nigKurt(alpha = 1, beta = 0, delta = 1, mu = 0)
}

\keyword{distribution}
