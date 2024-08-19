\name{varianceTest}
\alias{varianceTest}
\alias{Ftest}

\concept{Bartlett's test for equal variances}
\concept{Fligner-Killeen test for equal variances}


\title{Two sample variance tests}

\description{
    
  Tests if two series differ in their distributional
  variance parameter.
    
}

\usage{ 
varianceTest(x, y, method = c("varf", "bartlett", "fligner"), 
    title = NULL, description = NULL)
}

\arguments{
  \item{x, y}{
    numeric vectors of data values.
  }
  \item{method}{
    a character string naming which test should be applied.
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
    
  The \code{method="varf"} can be used to compare variances of two 
  normal samples performing an F test. The null hypothesis is that 
  the ratio of the variances of the populations from which they were 
  drawn is equal to one. 
  
  The \code{method="bartlett"} performs the Bartlett test of the 
  null hypothesis that the variances in each of the samples are the 
  same. This fact of equal variances across samples is also called 
  \emph{homogeneity of variances}. Note, that Bartlett's test is 
  sensitive to departures from normality. That is, if the samples 
  come from non-normal distributions, then Bartlett's test may simply 
  be testing for non-normality. The Levene test (not yet implemented)
  is an alternative to the Bartlett test that is less sensitive to 
  departures from normality. 
  
  The \code{method="fligner"} performs the Fligner-Killeen test of 
  the null that the variances in each of the two samples are the same. 
    
}

\value{
  an object from class \code{\link{fHTEST}}
}

\note{

  Some of the test implementations are selected from \R's \code{ctest} 
  package.  
       
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

\examples{
\dontshow{set.seed(1234)}
x <- rnorm(50)
y <- rnorm(50)
   
varianceTest(x, y, "varf")
varianceTest(x, y, "bartlett")
varianceTest(x, y, "fligner")
}

\keyword{htest}
