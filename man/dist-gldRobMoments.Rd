\name{gldRobMoments}
\alias{gldRobMoments}

\alias{gldMED}
\alias{gldIQR}
\alias{gldSKEW}
\alias{gldKURT}


\title{Robust Moments for the GLD}

\description{
    
    Computes the first four robust moments for the
    Generalized Lambda Distribution. 
    
}

\usage{
gldMED(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
gldIQR(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
gldSKEW(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
gldKURT(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
}

\arguments{
  \item{lambda1}{location parameter}
  \item{lambda2}{scale parameter}
  \item{lambda3}{first shape parameter}
  \item{lambda4}{second shape parameter}
}

\value{

  a named numerical value.  The name is
  one of \code{MED}, \code{IQR}, \code{SKEW}, or \code{KURT}, obtained
  by dropping the \code{gld} prefix from the name of the corresponding
  function.

}

\author{
  Diethelm Wuertz.
}

\examples{   
## gldMED -
   # Median:
   gldMED(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
 
## gldIQR - 
   # Inter-quartile Range:
   gldIQR(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
 
## gldSKEW -  
   # Robust Skewness:
   gldSKEW(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
   
## gldKURT -
   # Robust Kurtosis:
   gldKURT(lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
}

\keyword{distribution}
