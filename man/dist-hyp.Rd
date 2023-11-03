\name{hyp}
\alias{hyp}

\alias{dhyp}
\alias{phyp}
\alias{qhyp}
\alias{rhyp}


\concept{hyperbolic distribution}


\title{Hyperbolic distribution}

\description{
    
  Density, distribution function, quantile function 
  and random generation for the hyperbolic distribution.
    
}

\usage{
dhyp(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1, log = FALSE)
phyp(q, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1, \dots)
qhyp(p, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1, \dots)
rhyp(n, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1)
}

\arguments{
  \item{x, q}{
    numeric vector of quantiles.
  }
  \item{p}{
    numeric vector of probabilities.
  }
  \item{n}{
    number of observations.
  } 
  \item{alpha}{
    
    shape parameter, a positive number. \code{alpha} can also be a
    vector of length four, containing \code{alpha}, \code{beta},
    \code{delta} and \code{mu} (in that order).
    
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
  \item{pm}{

    integer number specifying the parameterisation, one of
    \code{1}, \code{2}, \code{3}, or \code{4}. The default is the
    first parameterization.

  }
  \item{log}{
	
    a logical value, if \code{TRUE}, probabilities \code{p} are given as
    \code{log(p)}.

  }
  \item{\dots}{
    arguments to be passed to the function \code{integrate}.
  }
}

\details{

  \code{dhyp} gives the density,
  \code{phyp} gives the distribution function,
  \code{qhyp} gives the quantile function, and
  \code{rhyp} generates random deviates.

  The meaning of the parameters given above corresponds to the first
  parameterization, \code{pm = 1}, which is the default.
      
  In the second parameterization, \code{pm=2}, \code{alpha} and
  \code{beta} take the meaning of the shape parameters (usually named)
  \code{zeta} and \code{rho}.
      
  In the third parameterization, \code{pm=3}, \code{alpha} and
  \code{beta} take the meaning of the shape parameters (usually named)
  \code{xi} and \code{chi}.
      
  In the fourth parameterization, \code{pm=4}, \code{alpha} and
  \code{beta} take the meaning of the shape parameters (usually named)
  \code{a.bar} and \code{b.bar}.
   
  The generator \code{rhyp} is based on the HYP algorithm given by
  Atkinson (1982).
    
}

\value{
  numeric vector
}

\author{
  David Scott for code implemented from \R's contributed package
  \pkg{HyperbolicDist}.
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
## hyp -
   set.seed(1953)
   r = rhyp(5000, alpha = 1, beta = 0.3, delta = 1)
   plot(r, type = "l", col = "steelblue",
     main = "hyp: alpha=1 beta=0.3 delta=1")
 
## hyp - 
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, 0.25)
   lines(x, dhyp(x, alpha = 1, beta = 0.3, delta = 1))
 
## hyp -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, phyp(x, alpha = 1, beta = 0.3, delta = 1))
   
## hyp -
   # Compute Quantiles:
   qhyp(phyp(seq(-5, 5, 1), alpha = 1, beta = 0.3, delta = 1), 
     alpha = 1, beta = 0.3, delta = 1) 
}

\keyword{distribution}
