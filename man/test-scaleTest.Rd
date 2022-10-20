\name{scaleTest}
\alias{scaleTest}


\title{Two sample scale tests}


\description{
    
    Tests if two series differ in their distributional
    scale parameter.
    
}


\usage{
scaleTest(x, y, method = c("ansari", "mood"), 
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
    
    
    The \code{method="ansari"} performs the Ansari--Bradley two--sample 
    test for a difference in scale parameters. The test returns for 
    any sizes of the series \code{x} and \code{y} the exact p value 
    together with its asymptotic limit.  
    
    The \code{method="mood"}, is another test which performs a 
    two--sample test for a difference in scale parameters. The underlying 
    model is that the two samples are drawn from \emph{f(x-l)} and 
    \emph{f((x-l)/s)/s}, respectively, where \emph{l} is a common 
    location parameter and \emph{s} is a scale parameter. The null 
    hypothesis is \emph{s=1}. 
    
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
    
Moore, D.S. (1986);
    \emph{Tests of the chi-squared type}, 
    In: D'Agostino, R.B. and Stephens, M.A., eds., 
    Goodness-of-Fit Techniques, Marcel Dekker, New York. 

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
   
## scaleTest -
   scaleTest(x, y, "ansari")
   scaleTest(x, y, "mood")
}


\keyword{htest}

