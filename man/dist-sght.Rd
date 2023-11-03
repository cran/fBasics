\name{sght}
\alias{sght}

\alias{dsght}
\alias{psght}
\alias{qsght}
\alias{rsght}


\concept{standardized generalized hyperbolic Student-t distribution}


\title{Standardized generalized hyperbolic Student-t Distribution}

\description{

  Density, distribution function, quantile function and random
  generation for the standardized generalized hyperbolic Student-t
  distribution.

}

\usage{
dsght(x, beta = 0.1, delta = 1, mu = 0, nu = 10, log = FALSE)
psght(q, beta = 0.1, delta = 1, mu = 0, nu = 10)
qsght(p, beta = 0.1, delta = 1, mu = 0, nu = 10)
rsght(n, beta = 0.1, delta = 1, mu = 0, nu = 10)
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
  \item{beta}{

    numeric value, \code{beta} is the skewness parameter in the range
    \code{(0, alpha)}.

  }
  \item{delta}{

    numeric value, the scale parameter, must be zero or positive.

  }
  \item{mu}{

    numeric value, the location parameter, by default 0.

  }
  \item{nu}{

    a numeric value, the number of degrees of freedom.  Note,
    \code{alpha} takes the limit of \code{abs(beta)}, and
    \code{lambda=-nu/2}.

  }
  \item{log}{
    a logical, if TRUE, probabilities \code{p} are given as
    \code{log(p)}.
  }
}

\details{
  \code{dsght} gives the density,
  \code{psght} gives the distribution function,
  \code{qsght} gives the quantile function, and
  \code{rsght} generates random deviates.

  These are the parameters in the first parameterization.
}

\value{
  numeric vector
}

\author{
  Diethelm Wuertz
}

\examples{
## rsght -
   set.seed(1953)
   r = rsght(5000, beta = 0.1, delta = 1, mu = 0, nu = 10)
   plot(r, type = "l", col = "steelblue",
     main = "gh: zeta=1 rho=0.5 lambda=1")

## dsght -
   # Plot empirical density and compare with true density:
   hist(r, n = 50, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, length = 501)
   lines(x, dsght(x, beta = 0.1, delta = 1, mu = 0, nu = 10))

## psght -
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, psght(x, beta = 0.1, delta = 1, mu = 0, nu = 10))

## qsght -
   # Compute Quantiles:
   round(qsght(psght(seq(-5, 5, 1), beta = 0.1, delta = 1, mu = 0, nu =10),
               beta = 0.1, delta = 1, mu = 0, nu = 10), 4)
}

\keyword{distribution}
