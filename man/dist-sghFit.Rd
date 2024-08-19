\name{sghFit}
\alias{sghFit}

\concept{Standardized generalized hyperbolic distribution}


\title{Standardized GH distribution fit}

\description{

  Estimates the distributional parameters for a standardized generalized
  hyperbolic distribution.

}

\usage{
sghFit(x, zeta = 1, rho = 0, lambda = 1, include.lambda = TRUE,
    scale = TRUE, doplot = TRUE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, \dots) 
}

\arguments{
  
  \item{x}{
      a numeric vector. 
      }
  \item{zeta, rho, lambda}{
      shape parameter \code{zeta} is positive,
      skewness parameter \code{rho} is in the range (-1, 1).
      and index parameter \code{lambda}, by default 1.
      }
  \item{include.lambda}{
      a logical flag, by default \code{TRUE}. Should the index
      parameter \code{lambda} included in the parameter estimate?
      }
  \item{scale}{
      a logical flag, by default \code{TRUE}. Should the time series
      be scaled by its standard deviation to achieve a more stable
      optimization?
      }
  \item{doplot}{
      a logical flag. Should a plot be displayed?
      }        
  \item{span}{
      x-coordinates for the plot, by default 100 values 
      automatically selected and ranging between the 0.001, 
      and 0.999 quantiles. Alternatively, you can specify
      the range by an expression like \code{span=seq(min, max,
      times = n)}, where, \code{min} and \code{max} are the 
      left and right endpoints of the range, and \code{n} gives 
      the number of the intermediate points.
      }        
  \item{trace}{
      a logical flag. Should the parameter estimation process be
      traced?
      }
  \item{title}{
      a character string which allows for a project title.
      }     
  \item{description}{
      a character string which allows for a brief description.
      }
  \item{\dots}{
      parameters to be parsed.
      }

}

\value{
  an object from class \code{"fDISTFIT"}.
  Slot \code{fit} is a list, currently with components
  \code{estimate}, \code{minimum}, \code{code},
  \code{param},
  \code{mean} (mean of the original data), \code{var} (variance of
  original data).
}

\examples{
set.seed(1953)
s <- rsgh(n = 2000, zeta = 0.7, rho = 0.5, lambda = 0) 

sghFit(s, zeta = 1, rho = 0, lambda = 1, include.lambda = TRUE, 
       doplot = TRUE, trace = FALSE)
}

\keyword{distribution}
