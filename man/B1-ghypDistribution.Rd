\name{ghypDistribution}

\alias{ghypDistribution}

\alias{dhyp}
\alias{phyp}
\alias{qhyp}
\alias{rhyp}

\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}

\title{Generalized Hyperbolic Distribution}


\description{
	
	A collection of functions to compute density, distribution function, 
	quantile function and to generate random variates for two special 
	cases of the generalized hyperbolic distribution: the hperbolic 
	distribution and the normal inverse Gaussian distribution. 
	\cr
	
	The functions are:
	
	\tabular{rll}{
		1	\tab  	\code{[dpqr]hyp} 	
			\tab  	The hyperbolic distribution, \cr
		2	\tab  	\code{[dpqr]nig} 	
			\tab  	The normal inverse Gaussian distribution.}
}


\usage{
dhyp(x, alpha , beta , delta , mu = 0)
phyp(q, alpha , beta , delta , mu = 0, ...)
qhyp(p, alpha , beta , delta , mu = 0)
rhyp(n, alpha , beta , delta , mu = 0)

dnig(x, alpha, beta, delta, mu = 0)
pnig(q, alpha, beta, delta, mu = 0)
qnig(p, alpha, beta, delta, mu = 0)
rnig(n, alpha, beta, delta, mu = 0)
}


\arguments{

  	\item{alpha, beta, delta, mu}{
		shape parameter \code{alpha};
		skewness parameter \code{beta}, \code{abs(beta)} is in the 
		range (0,alpha);
		scale parameter \code{delta}, \code{delta} must be zero or 
		positive; and
		location parameter \code{mu}, by default 0.
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
	\item{\dots}{
		arguments to be passed to the function \code{integrate}.
		}
	
}


\value{

	All values are numeric vectors:
	\code{d*} returns the density,
	\code{p*} returns the distribution function,
	\code{q*} returns the quantile function, and
	\code{r*} generates random deviates.
	
}

\details{
  
	\bold{Hyperbolic Distibution:}
	\cr\cr
	The generator \code{rhyperb} is based on the HYP algorithm given 
	by Atkinson (1982).
    \cr
    
    \bold{Normal Inverse Gaussian Distribution:}
    \cr\cr
    The random deviates are calculated with the method described by 
    Raible (2000).
    
}


\examples{
## hyp -
   xmpBasics("\nStart: Hyperbolic Distribution > ")
   par(mfcol = c(3, 2), cex = 0.5)
   set.seed(1953)
   r = rhyp(1000, alpha = 1, beta = 0.3, delta = 1)
   plot(r, type = "l", col = "steelblue4",
   	 main = "hyp: alpha =1 beta=0.3 delta=1")
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4") 
   x = seq(-10, 50, 0.2)
   lines(x, dhyp(x, alpha = 1, beta = 0.3, delta = 1))
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(x, phyp(x, alpha = 1, beta = 0.3, delta = 1))
   # Compute quantiles:
   qhyp(phyp(seq(-5, 7, 1), alpha = 1, beta = 0.3, delta = 1), 
     alpha = 1, beta = 0.3, delta = 1) 
 
     
## nig -
   xmpBasics("\nNext: Normal Inverse Gaussian Distribution > ")
   set.seed(1953)
   r = rnig(5000, alpha = 1, beta = 0.3, delta = 1)
   plot(r, type = "l", col = "steelblue4",
     main = "nig: alpha=1 beta=0.3 delta=1")
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
   x = seq(-10, 10, 0.1)
   lines(x, dnig(x, alpha = 1, beta = 0.3, delta = 1))
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue4")
   lines(x, pnig(x, alpha = 1, beta = 0.3, delta = 1))
   # Compute Quantiles:
   qnig(pnig(seq(-5, 7, 1), alpha = 1, beta = 0.3, delta = 1), 
     alpha = 1, beta = 0.3, delta = 1) 
}


\author{

	David Scott for the HYP Generator from R's "HyperbolicDist" package, \cr
	Diethelm Wuertz for this \R-port.
	
}


\references{
Atkinson, A.C. (1982); 
	\emph{The simulation of generalized inverse Gaussian and hyperbolic 
	random variables},
	SIAM J. Sci. Stat. Comput. 3, 502–-515. 

Barndorff-Nielsen O. (1977);
	\emph{Exponentially decreasing distributions for the logarithm of 
	particle size}, 
	Proc. Roy. Soc. Lond., A353, 401–-419. 

Barndorff-Nielsen O., Blaesild, P. (1983); 
	\emph{Hyperbolic distributions. In Encyclopedia of Statistical 
	Sciences}, 
	Eds., Johnson N.L., Kotz S. and Read C.B., 
	Vol. 3, pp. 700–-707. New York: Wiley. 

Raible S. (2000);
	\emph{Levy Processes in Finance: Theory, Numerics and Empirical Facts},
	PhD Thesis, University of Freiburg, Germany, 161 pages.
}


\seealso{
  
	\code{\link{dstable}},
  	\code{\link{dsymstb}}.
  	
}


\keyword{distribution}

