\name{locationTest}
\alias{locationTest}

\concept{two sample t-test}
\concept{Kruskal-Wallis rank sum test}


\title{Two sample location tests}

\description{
    
  Tests if two series differ in their distributional location parameter.

}


\usage{
locationTest(x, y, method = c("t", "kw2"), title = NULL,
    description = NULL)
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
    
  
    The \code{method = "t"} can be used to determine if the two sample 
    means are equal for unpaired data sets. Two variants are used,
    assuming equal or unequal variances. 
  
    The \code{method = "kw2"} performs a Kruskal-Wallis rank sum 
    test of the null hypothesis that the central tendencies or medians of 
    two samples are the same. The alternative is that they differ. 
    Note, that it is not assumed that the two samples are drawn from the 
    same distribution. It is also worth to know that the test assumes 
    that the variables under consideration have underlying continuous 
    distributions.

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
## rnorm - 
   # Generate Series:
   x = rnorm(50)
   y = rnorm(50)
  
## locationTest -
   locationTest(x, y, "t")
   locationTest(x, y, "kw2")

}


\keyword{htest}
