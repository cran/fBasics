\name{stableDistribution}

\alias{stableDistribution}

\alias{dsymstb}
\alias{psymstb}
\alias{qsymstb}
\alias{rsymstb}

\alias{dstable}
\alias{pstable}
\alias{qstable}
\alias{rstable}

\title{Stable Distribution Function}


\description{
	
	A collection of functions to Compute density, distribution function, 
	quantile function and to generate random variates from the stable 
	distribution. Two different algorithms are used for the symmetric 
	and skewed distribution.
	\cr
	
	The functions are:
	
	\tabular{rll}{
		1	\tab  	\code{[dpqr]symstb} 	
			\tab  	The symmetric stable distribution, \cr
    	2	\tab	\code{[dpqr]stable} 	
    		\tab  	The skewed stable distribution using parametrizition 0.}

}


\usage{
dsymstb(x, alpha)
psymstb(q, alpha)
qsymstb(p, alpha)
rsymstb(n, alpha)

dstable(x, alpha, beta, gamma=1, delta=0, pm = 0)
pstable(q, alpha, beta, gamma=1, delta=0, pm = 0)
qstable(p, alpha, beta, gamma=1, delta=0, pm = 0)
rstable(n, alpha, beta, gamma=1, delta=0, pm = 0)
}


\arguments{

  	\item{alpha, beta, gamma, delta}{
		value of the index parameter \code{alpha} with \code{alpha = (0,2]};
		skewness parameter \code{beta}, in the range [-1, 1];
		scale parameter \code{gamma}; and
		shift parameter \code{delta}.
		}
	\item{n}{
		number of observations, an integer value.
		} 
	\item{p}{
		a numeric vector of probabilities.
		}
  	\item{pm}{
		parameterization, at the moment only the 0-th 
		parameterization is supported.
		}
    \item{x, q}{
		a numeric vector of quantiles.
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

	\bold{Symmetric Stable Distribution:}
	\cr\cr
	For the density and probability the approach of McCulloch is 
	implemented. Note, that McCulloch's approach has a density 
	precision of 0.000066 and a distribution precision of 0.000022 
	for \code{alpha} in the  range [0.84, 2.00]. 
	Quantiles are evaluated from a root finding process via the
	probability function. Thus, this leads to nonnegligible 
	errors for small quantiles, since the quantile evaluation 
	depends on the quality of the probability function.To achieve 
	higher precisions use the function \code{stable} with argument 
	\code{beta=0}. \cr
	For generation of random deviates the results of Chambers, 
	Mallows, and Stuck are used.
	\cr
	
	\bold{Skew Stable Distribution:}
	\cr\cr
	The function uses the approach of J.P. Nolan for general 
 	stable distributions. Nolan derived expressions in form of 
	integrals based on the charcteristic function for standardized 
	stable random variables. These integrals are numerically 
	evaluated using R's function \code{integrate}. 
	\cr
	"S0" parameterization: based on the (M) representation
   	of Zolotarev for an alpha stable distribution with skewness
   	beta. Unlike the Zolotarev (M) parameterization, gamma and 
 	delta are straightforward scale and shift parameters. This
   	representation is continuous in all 4 parameters, and gives 
   	an intuitive meaning to gamma and delta that is lacking in 
  	other parameterizations.} 
  	\cr
	"S" or "S1" parameterization: the parameterization used 
	by Samorodnitsky and Taqqu in the book Stable Non-Gaussian 
	Random Processes. It is a slight modification of Zolotarev's 
	(A) parameterization.} 
	\cr
	"S*" or "S2" parameterization: a modification of the S0 
	parameterization which is defined so that (i) the scale gamma 
	agrees with the Gaussian scale (standard dev.) when alpha=2 
	and the Cauchy scale when alpha=1, (ii) the mode is exactly at 
	delta.} 
	\cr
	"S3" parameterization: an internal parameterization.
   	The scale is the same as the S2 parameterization, the shift 
	is -beta*g(alpha), where g(alpha) is defined in the paper 
	below.}
}

\notes{
	
	Note, that up to now for the stable distribution, only 
	parameterization 0 is implemented!
	
}


\examples{	
## rsymstb -
   xmpBasics("\nStart: Symmetric Stable Distribuion:  > ")
   par(mfcol = c(3, 2), cex = 0.5)
   set.seed(1953)
   r = rsymstb(n = 1000, alpha = 1.85)
   plot(r, type = "l", main = "symstb: alpha = 1.85")
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
   x = seq(-10, 10, 0.1)
   lines(x, dsymstb(x = x, alpha = 1.85))
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(x, psymstb(x, alpha = 1.85))
   # Compute quantiles:
   qsymstb(psymstb(q = seq(-10, 10, 1), alpha = 1.85), alpha = 1.85)
   
## stable - 
   set.seed(1953)
   r = rstable(n = 1000, alpha = 1.85, beta = 0.3)
   plot(r, type = "l", main = "stable: alpha = 1,85 beta = 0.3")
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
   x = seq(-10, 10, 0.1)
   lines(x, dstable(x = x, alpha = 1.85, beta = 0.3))
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(x, pstable(q = x, alpha = 1.85, beta = 0.3))
   # Compute quantiles:
   qstable(pstable(seq(-10, 10, 1), alpha = 1.85, beta = 0.3), 
     alpha = 1.85, beta = 0.3)
}


\author{

	McCulloch for the Fortran program, and \cr
	Diethelm Wuertz for this \R-port.

}


\references{
Chambers J.M., Mallows, C.L. and Stuck, B.W. (1976);
    \emph{A Method for Simulating Stable Random Variables},
    J. Amer. Statist. Assoc. 71, 340--344.

Nolan J.P. (1999);
    \emph{Stable Distributions},
    Preprint, University Washington DC, 30 pages.
    
Nolan J.P. (1999);
    \emph{Numerical Calculation of Stable Densities and Distribution
    Functions},
    Preprint, University Washington DC, 16 pages.

Samoridnitsky G., Taqqu M.S. (1994);
	\emph{Stable Non-Gaussian Random Processes, Stochastic Models
	with Infinite Variance},
	Chapman and Hall, New York, 632 pages.

Weron, A., Weron R. (1999);
	\emph{Computer Simulation of Levy alpha-Stable Variables and
	Processes},
	Preprint Technical Univeristy of Wroclaw, 13 pages.
}

\seealso{
  
	\code{\link{dnig}},
  	\code{\link{dhyp}}.
  	
}


\keyword{distribution}

