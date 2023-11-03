\name{correlationTest}
\alias{correlationTest}

\alias{pearsonTest}
\alias{kendallTest}
\alias{spearmanTest}


\concept{Pearson's product moment correlation test}
\concept{Kendall's tau correlation test}
\concept{Spearman's rho correlation test}

\concept{Pearson's correlation}
\concept{Kendall's correlation}
\concept{Spearman's correlation}


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
  
  \item{x,y}{
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

  These functions test for association/correlation between paired
  samples based on the Pearson's product moment correlation coefficient
  (a.k.a. sample correlation), Kendall's tau, and Spearman's rho
  coefficients.
    
  \code{pearsonTest}, \code{kendallTest}, and \code{spearmanTest} are
  wrappers of base R's \code{\link[stats]{cor.test}} with simplified
  interface.  They provide 'exact' and approximate p-values for all
  three alternatives (two-sided, less, and greater), as well as 95\%
  confidence intervals. This is particularly convenient in interactive
  use.

  Instead of calling the individual functions, one can use
  \code{correlationTest} and specify the required test with argument
  \code{method}.

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
