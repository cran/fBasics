\name{nig}
\alias{nig}

\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}


\title{Normal Inverse Gaussian Distribution}

\description{
    
    Density, distribution function, quantile function 
    and random generation for the normal inverse
    Gaussian distribution.
    
}


\usage{
dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0, log = FALSE)
pnig(q, alpha = 1, beta = 0, delta = 1, mu = 0)
qnig(p, alpha = 1, beta = 0, delta = 1, mu = 0)
rnig(n, alpha = 1, beta = 0, delta = 1, mu = 0)
}


\arguments{
    \item{x,q}{
      a numeric vector of quantiles.
    }
    \item{p}{
      a numeric vector of probabilities.
    }
    \item{n}{
      number of observations.
    } 
    \item{alpha}{
      shape parameter.
    }
    \item{beta}{
      skewness parameter \code{beta}, \code{abs(beta)} is in the range
      \code{(0, alpha)}.
    }
    \item{delta}{
      scale parameter, must be zero or positive.
    }
    \item{mu}{
      location parameter, by default 0.
    }
    \item{log}{
      a logical flag by default \code{FALSE}.  Should labels and a main
      title be drawn to the plot?
    }
}


\details{

  The parameters \code{alpha, beta, delta, mu} are in the first
  parameterization of the distribution.
  
  The random deviates are calculated with the method described by
  Raible (2000).
    
}

\value{

  \code{dnig} gives the density,
  \code{pnig} gives the distribution function,
  \code{qnig} gives the quantile function, and
  \code{rnig} generates random deviates.
    
  Each function returns a numeric vector with attribute \code{"param"}
  containing the parameters of the distribution.

  \strong{GNB TODO:} It seems that only \code{rnig} has an attribute and
  that is called \code{"control"} (but it indeed contains the
  parameters).
    
}

\author{

    David Scott for code implemented from \R's 
    contributed package \code{HyperbolicDist}.
    
}


\references{

Atkinson, A.C. (1982); 
    \emph{The simulation of generalized inverse Gaussian and hyperbolic 
    random variables},
    SIAM J. Sci. Stat. Comput. 3, 502--515. 

Barndorff-Nielsen O. (1977);
    \emph{Exponentially decreasing distributions for the logarithm of 
    particle size}, 
    Proc. Roy. Soc. Lond., A353, 401--419. 

Barndorff-Nielsen O., Blaesild, P. (1983); 
    \emph{Hyperbolic distributions. In Encyclopedia of Statistical 
    Sciences}, 
    Eds., Johnson N.L., Kotz S. and Read C.B., 
    Vol. 3, pp. 700--707. New York: Wiley. 

Raible S. (2000);
    \emph{Levy Processes in Finance: Theory, Numerics and Empirical Facts},
    PhD Thesis, University of Freiburg, Germany, 161 pages.
    
}


\examples{   
## nig -
   set.seed(1953)
   r = rnig(5000, alpha = 1, beta = 0.3, delta = 1)
   plot(r, type = "l", col = "steelblue",
     main = "nig: alpha=1 beta=0.3 delta=1")
 
## nig - 
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, 0.25)
   lines(x, dnig(x, alpha = 1, beta = 0.3, delta = 1))
 
## nig -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, pnig(x, alpha = 1, beta = 0.3, delta = 1))
   
## nig -
   # Compute Quantiles:
   qnig(pnig(seq(-5, 5, 1), alpha = 1, beta = 0.3, delta = 1), 
     alpha = 1, beta = 0.3, delta = 1) 
}


\keyword{distribution}

