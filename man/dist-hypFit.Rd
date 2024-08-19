\name{hypFit}
\alias{hypFit}

\concept{hyperbolic distribution}


\title{Fit a hyperbolic distribution}

\description{

  Estimates the parameters of a hyperbolic distribution.

}

\usage{    
hypFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, 
    scale = TRUE, doplot = TRUE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, \dots) 
}

\arguments{
  \item{x}{
    a numeric vector. 
  }
  \item{alpha}{
    
    shape parameter, a positive number.

  }
  \item{beta}{

    skewness parameter, \code{abs(beta)} is in the 
    range \code{(0, alpha)}.

  }
  \item{delta}{

    scale parameter, must be zero or positive.

  }
  \item{mu}{

    location parameter, by default 0.
    
  }
  \item{scale}{

    a logical flag, by default \code{TRUE}. Should the time series be
    scaled by its standard deviation to achieve a more stable
    optimization?

  }
  \item{doplot}{

    a logical flag. Should a plot be displayed?

  }
  \item{span}{
    
    x-coordinates for the plot, by default 100 values automatically
    selected and ranging between the 0.001, and 0.999
    quantiles. Alternatively, you can specify the range by an expression
    like \code{span=seq(min, max, times = n)}, where, \code{min} and
    \code{max} are the left and right endpoints of the range, and
    \code{n} gives the number of the intermediate points.

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

\details{

  The meaning of the parameters given above corresponds to the first
  parameterization, see \code{\link{dhyp}} for details.
  
  The function \code{\link{nlm}} is used to minimize the "negative" 
  maximum log-likelihood function. \code{nlm} carries out a minimization 
  using a Newton-type algorithm.
       
}

\value{
  an object from class \code{"\linkS4class{fDISTFIT}"}.
  Slot \code{fit} is a list, currently with components
  \code{estimate}, \code{minimum} and \code{code}.
}

\examples{
set.seed(1953)
s <- rhyp(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1.0) 

hypFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), doplot = TRUE,
       trace = FALSE) 
}

\keyword{distribution}
