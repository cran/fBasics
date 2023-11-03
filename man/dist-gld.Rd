\name{gld}
\alias{gld}

\alias{dgld}
\alias{pgld}
\alias{qgld}
\alias{rgld}


\concept{generalized lambda distribution}


\title{Generalized Lambda Distribution}

\description{
    
  Density, distribution function, quantile function and random
  generation for the generalized lambda distribution.
    
}

\usage{
dgld(x, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8, log = FALSE)
pgld(q, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
qgld(p, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
rgld(n, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
}

\arguments{
  \item{lambda1}{
    location parameter.
  }
  \item{lambda2}{
    scale parameter.
  }
  \item{lambda3}{
    first shape parameter.
  }
  \item{lambda4}{
    second shape parameter.
  }
  \item{n}{
    number of observations.
  } 
  \item{p}{
    a numeric vector of probabilities.
  }
  \item{x, q}{
    a numeric vector of quantiles.
  }
  \item{log}{

    a logical, if TRUE, probabilities \code{p} are given as
    \code{log(p)}.
    
  }
}

\details{
  \code{dgld} gives the density,
  \code{pgld} gives the distribution function,
  \code{qgld} gives the quantile function, and
  \code{rgld} generates random deviates.
}

\value{
  numeric vector
}

\author{
  Chong Gu for code implemented from \R's contributed package \code{gld}.
}

\examples{   
## rgld -
   set.seed(1953)
   r = rgld(500, 
     lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8)
   plot(r, type = "l", col = "steelblue",
     main = "gld: lambda1=0 lambda2=-1 lambda3/4=-1/8")
 
## dgld - 
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", 
     col = "steelblue")
   x = seq(-5, 5, 0.25)
   lines(x, dgld(x, 
     lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8))
 
## pgld -  
   # Plot df and compare with true df:
   plot(sort(r), ((1:500)-0.5)/500, main = "Probability", 
     col = "steelblue")
   lines(x, pgld(x, 
     lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8))
   
## qgld -
   # Compute Quantiles:
   qgld(pgld(seq(-5, 5, 1), 
     lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8), 
     lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 
}

\keyword{distribution}
