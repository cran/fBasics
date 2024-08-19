\name{snigFit}
\alias{snigFit}

\concept{Standardized normal inverse Gaussian distribution}


\title{Fit of a Standardized NIG Distribution}

\description{

  Estimates the parameters of a standardized normal inverse Gaussian
  distribution.

}

\usage{   
snigFit(x, zeta = 1, rho = 0, scale = TRUE, doplot = TRUE, 
    span = "auto", trace = TRUE, title = NULL, description = NULL, \dots) 
}

\arguments{
  
  \item{zeta, rho}{
      shape parameter \code{zeta} is positive,
      skewness parameter \code{rho} is in the range (-1, 1).
      }
  \item{description}{
      a character string which allows for a brief description.
      }
  \item{doplot}{
      a logical flag. Should a plot be displayed?
      }
  \item{scale}{
      a logical flag, by default \code{TRUE}. Should the time series
      be scaled by its standard deviation to achieve a more stable
      optimization?
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
  \item{title}{
      a character string which allows for a project title.
      }
  \item{trace}{
      a logical flag. Should the parameter estimation process be
      traced?
      }
  \item{x}{
      a numeric vector. 
      }
  \item{\dots}{
      parameters to be parsed.
      }

}

\value{
  an object from class \code{"\linkS4class{fDISTFIT}"}.

  Slot \code{fit} is a list with the same components as the result from
  \code{\link{snigFit}}.
               
}

\examples{
## Simulate Random Variates:
set.seed(1953)
s <- rsnig(n = 2000, zeta = 0.7, rho = 0.5) 

## snigFit -  
   # Fit Parameters:
   snigFit(s, zeta = 1, rho = 0, doplot = TRUE) 
}

\keyword{distribution}
