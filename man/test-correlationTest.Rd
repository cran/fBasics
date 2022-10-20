\name{correlationTest}
\alias{correlationTest}

\alias{pearsonTest}
\alias{kendallTest}
\alias{spearmanTest}


\title{Correlation tests}

\description{
    
  Tests if two series are correlated.
    
}

\usage{    
correlationTest(x, y, method = c("pearson", "kendall", "spearman"), 
    title = NULL, description = NULL)

pearsonTest(x, y, title = NULL, description = NULL) 
kendallTest(x, y, title = NULL, description = NULL)
spearmanTest(x, y, title = NULL, description = NULL)
}

\arguments{
  
  \item{x, y}{
    numeric vectors of data values.
  }
  \item{method}{
    a character string naming which test should be applied.
  } 
  \item{title}{
    an optional title string, if not specified the input's data 
    name is deparsed.
  }
  \item{description}{
    optional description string, or a vector of character strings.
  } 

}

\details{
     
  The function \code{correlationTest} tests for association between
  paired samples allowing to compute Pearson's product moment
  correlation coefficient, Kendall's tau, or Spearman's rho.
    
}

\value{
  an object from class \code{\link{fHTEST}}
}


\note{

    Some of the test implementations are selected from package
    \code{ctest}.
    
}


\references{

Conover, W. J. (1971);
    \emph{Practical nonparametric statistics},
    New York: John Wiley & Sons.

Lehmann E.L. (1986); 
    \emph{Testing Statistical Hypotheses}, 
    John Wiley and Sons, New York.

}

\author{

    R-core team for hypothesis tests implemented from \R's 
    package \code{ctest}.
    
}


\seealso{

    \code{\link{locationTest}},
    \code{\link{scaleTest}},
    \code{\link{varianceTest}}.
    
}


\examples{
\dontshow{
set.seed(1234)
}
## x, y -
   x = rnorm(50)
   y = rnorm(50)
  
## correlationTest - 
   correlationTest(x, y, "pearson")
   correlationTest(x, y, "kendall")
   spearmanTest(x, y)
}


\keyword{htest}
