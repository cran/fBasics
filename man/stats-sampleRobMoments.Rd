\name{sampleLMoments}
\alias{sampleLMoments}

\alias{sampleLmoments}


\title{Sample L-moments}

\description{
    
  Computes L-moments from an empirical sample data set.
    
}

\usage{
sampleLmoments(x, rmax = 4)
}

\arguments{

  \item{x}{
    numeric vector, the sample values.
  }
  \item{rmax}{
    an integer value, the number of L-moments to be returned.
  }
}

\value{

  a named numeric vector of length \code{rmax} with names
  \code{c("L1", "L2", ..., "L<rmax>")}
    
}

\author{
  Diethelm Wuertz
}

\examples{   
## Sample:
   x = rt(100, 4)
   
## sampleLmoments -
   sampleLmoments(x)
}

\keyword{distribution}
