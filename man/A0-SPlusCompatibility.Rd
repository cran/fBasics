\name{SPlusCompatibility}

\alias{SPlusCompatibility}


\title{R Functions for SPlus Compatibility}


\description{
    
    A collection and description of functions for 
    the usage of Rmetrics under SPlus.
    \cr
    
    The functions are in alphabetical order:
    
    \tabular{ll}{
    \code{\%x\%} \tab Computes generalised kronecker product of two arrays, \cr
    \code{as.Date} \tab Converts date represenatation, \cr
    \code{cov} \tab Computes covariance of vectors, \cr
    \code{data} \tab Loads or lists specified data sets, \cr
    \code{download.file} \tab Downloads files from Internet using "lynx" or "wget", \cr
    \code{embed} \tab Embeds time series into a low-dimensional space, \cr
    \code{forwardsolve} \tab Solves linear equations for lower triangular systems, \cr
    \code{match.fun} \tab Verifies function from a 'function argument', \cr
    \code{model.weights} \tab Returns weights of a model frame, \cr
    \code{nlm} \tab Wraps R's "nlm" on S's "nlminb" optimizer, \cr
    \code{NCOL} \tab Returns number of columns in a vector, array or data frame, \cr
    \code{NROW} \tab Returns number of rows in a vector, array or data frame, \cr
    \code{optim} \tab Wraps R's "optim" on S's "nlminb" optimizer, \cr
    \code{sd} \tab Computes the standard deviation, \cr 
    \code{strsplit} \tab Splits elements of a character vector into substrings, \cr
    \code{which.max} \tab Determines index of the maximum of a numeric vector, \cr
    \code{which.min} \tab Determines index of the minimum of a numeric vector.}

}


\note{
  
    These functions are only used when running Rmetrics
    under SPlus. Note, that only the functionality is 
    supported which is required running these R functions
    for Rmetrics under Splus.
  
}


\examples{
## SOURCE("fBasics.A0-SPlusCompatibility")
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{programming}

