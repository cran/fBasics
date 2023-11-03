\name{sampleRobMoments}
\alias{sampleRobMoments}

\alias{sampleMED}
\alias{sampleIQR}
\alias{sampleSKEW}
\alias{sampleKURT}


\title{Robust moments for the GLD}

\description{
    
    Computes the first four robust moments for the
    Normal Inverse Gaussian Distribution. 
    
}

\usage{
sampleMED(x)
sampleIQR(x)
sampleSKEW(x)
sampleKURT(x)
}

\arguments{
  \item{x}{
    numeric vector, the sample values.
  }
}

\value{

  a named numerical value.  The name is one of \code{MED}, \code{IQR},
  \code{SKEW}, or \code{KURT}, obtained by dropping the \code{sample} prefix
  from the name of the corresponding function.

}

\author{
  Diethelm Wuertz
}

\examples{   
## Sample:
   x = rt(100, 4)
   
## sampleMED -
   # Median:
   sampleMED(x)
 
## sampleIQR - 
   # Inter-quartile Range:
   sampleIQR(x)
 
## sampleSKEW -  
   # Robust Skewness:
   sampleSKEW(x)
   
## sampleKURT -
   # Robust Kurtosis:
   sampleKURT(x)
}

\keyword{distribution}
