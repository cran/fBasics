\name{ssd}
\alias{ssd}

\alias{dssd}
\alias{pssd}
\alias{qssd}
\alias{rssd}


\concept{Spline Smoothed Distribution}


\title{Spline Smoothed Distribution}

\description{
    
  Density, distribution function, quantile function and random
  generation from smoothing spline estimates.
    
}

\usage{
dssd(x, param, log = FALSE)
pssd(q, param)
qssd(p, param)
rssd(n, param)
}

\arguments{
 
  \item{x, q}{
    a numeric vector of quantiles.
  }
  \item{p}{
    a numeric vector of probabilities.
  }
  \item{n}{
    number of observations.
  } 
  \item{param}{
    an object as returned by the function \code{ssdFit}.
  }
  \item{log}{
    a logical flag by default \code{FALSE}. 
    Should labels and a main title drawn to the plot?
  }
}

\details{
  \code{dssd} gives the density,
  \code{pssd} gives the distribution function,
  \code{qssd} gives the quantile function, and
  \code{rssd} generates random deviates.
}

\value{
  numeric vector
}

\author{
  Diethelm Wuertz, Chong Gu for the underlying \code{gss} package.
}

\references{

Gu, C. (2002), 
    \emph{Smoothing Spline ANOVA Models}, 
    New York Springer--Verlag.

Gu, C. and Wang, J. (2003), 
    \emph{Penalized likelihood density estimation: 
    Direct cross-validation and scalable approximation},
    Statistica Sinica, 13, 811--826. 
    
}

\examples{   
## ssdFit -
   set.seed(1953)
   r = rnorm(500)
   hist(r, breaks = "FD", probability = TRUE,
     col = "steelblue", border = "white")
 
## ssdFit - 
   param = ssdFit(r)
   
## dssd -  
   u = seq(min(r), max(r), len = 301)
   v = dssd(u, param)
   lines(u, v, col = "orange", lwd = 2)
}

\keyword{distribution}
