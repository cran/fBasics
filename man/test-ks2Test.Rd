\name{ks2Test}
\alias{ks2Test}


\title{Two sample Kolmogorov-Smirnov test}

\description{
    
  Tests if two series are distributionally equivalent using two sample
  Kolmogorov-Smirnov test.
    
}

\usage{
ks2Test(x, y, title = NULL, description = NULL)
}

\arguments{
  
  \item{x, y}{
    numeric vectors of data values.
  }
  \item{title}{
    an optional title string, if not specified the inputs data 
    name is deparsed.
  }
  \item{description}{
    optional description string, or a vector of character strings.
  } 
       
}

\details{
     
  The test \code{ks2Test} performs a Kolmogorov-Smirnov two sample
  test that the two data samples, \code{x} and \code{y}, come from the
  same distribution, not necessarily a normal distribution. That means
  that it is not specified what that common distribution is.

  \code{ks2Test} calls several times base R's \code{\link[stats]{ks.test}}
  p-values for all three alternatives (two-sided, less, and greater),
  as well as the exact p-value for the two-sided case.

  Note that the p-values are computed under a hypothesis of i.i.d.,
  which is rarely the case for time series. So, the results should be
  interpreted cautiosly if that is the case. The same applies when the
  data are residuals from fitted models.

}

\value{
  an object from class \code{\link{fHTEST}}
}

\references{

Conover, W. J. (1971);
    \emph{Practical nonparametric statistics},
    New York: John Wiley & Sons.

Lehmann E.L. (1986); 
    \emph{Testing Statistical Hypotheses}, 
    John Wiley and Sons, New York.

}

\examples{
\dontshow{set.seed(1234)}
## rnorm - 
   # Generate Series:
   x = rnorm(50)
   y = rnorm(50)
  
## ks2Test - 
   ks2Test(x, y)
}

\keyword{htest}
