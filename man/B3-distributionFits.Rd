\name{distributionFits}

\alias{distributionFits}

\alias{tFit}
\alias{hypFit}
\alias{nigFit}

\alias{hypStats}

\title{Parameter Fit of a Distribution}


\description{

	A collection of maximum likelihood estimators to fit the 
	parameters of a distribution and to compute basic statistical
	properties. Included are estimators for the Student-t, the 
	hyperbolic and the normal inverse Gaussian distributions. 
	\cr

	The functions are:
	
	\tabular{rll}{
		1 	\tab 	\code{tFit}   	
			\tab 	MLE parameter fit for a Student t-distribution, \cr
		2 	\tab	\code{hypFit} 	
			\tab 	MLE parameter fit for a hyperbolic distribution, \cr
		3 	\tab	\code{hypStats}	
			\tab 	Computes Mean and Variance for a hyperbolic dist, \cr
		4 	\tab 	\code{nigFit} 	
			\tab 	MLE parameter fit for a normal inverse Gaussian distribution. }

}


\usage{
tFit(x, df, doplot = TRUE, 
	span = seq(from = -10, to = 10, by = 0.1), \dots)
hypFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
	span = seq(from = -10, to = 10, by = 0.1), \dots)
nigFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
	span = seq(from = -10, to = 10, by = 0.1), \dots)
	
hypStats(alpha = 1, beta = 0, delta = 1, mu = 0)
}


\arguments{
  
	\item{alpha, beta, delta, mu}{[hypFit][nigFit][hypStats] -
		shape parameter \code{alpha}; 
		skewness parameter \code{beta} with \code{0 < |beta| < alpha};
		scale parameter \code{delta} with \code{delta <= 0}; 
		location parameter \code{mu}.
		}
	\item{df}{[tFit] -
		the number of degrees of freedom for the Student distribution, 
		\code{df > 2}, maybe non-integer.
		}
	\item{doplot}{
		a logical. Should a plot be displayed?
		}
	\item{span}{
		x-coordinates for the plot, by default 201 values 
		ranging between -10 and +10.
		}
	\item{x}{
		a numeric vector.
		}
	\item{\dots}{
		parameters parsed to the function \code{density}.
		}

}


\value{
  
  	The functions \code{*Fit} return a list with the following components:
  
  	\item{estimate}{
		the point at which the maximum value of the log liklihood 
		function is obtained.
		}
  	\item{objective}{
		the value of the estimated maximum, i.e. the value of the
		log liklihood function.
		}
  	\item{message}{
		an integer indicating why the optimization process terminated.
		}
  	\item{code}{
		an integer indicating why the optimization process terminated.\cr
		1: relative gradient is close to zero, current iterate is probably 
			solution; \cr
		2: successive iterates within tolerance, current iterate is probably 
			solution; \cr
		3: last global step failed to locate a point lower than \code{estimate}. 
			Either \code{estimate} is an approximate local minimum of the 
			function or \code{steptol} is too small; \cr
 		4: iteration limit exceeded; \cr
 		5: maximum step size \code{stepmax} exceeded five consecutive times. 
 			Either the function is unbounded below, becomes asymptotic to a 
 			finite value from above in some direction or \code{stepmax} 
			is too small.
			}
  	\item{gradient}{
		the gradient at the estimated maximum.
		}
  	\item{steps}{
		number of function calls.
		}
	
  	The function \code{hypStats} returns a list with two elements: the 
  	mean, \code{mean}, and the variance, \code{var}.
  
}


\details{

	The function \code{\link{nlm}} is used to minimize the "negative" maximum 
	log-likelihood function. \code{nlm} carries out a minimization using 
	a Newton-type algorithm.
	
}


\examples{
## tFit -
   xmpBasics("\nStart: MLE Fit to Student's t Density > ")
   par(mfrow = c(2,2), cex = 0.7, err = -1)
   options(warn = -1)
   # Simulated random variates t(4):
   s = rt(n = 10000, df = 4) 
   # Note, this may take some time.
   # Starting vector:
   df.startvalue = 2*var(s)/(var(s)-1)
   tFit(s, df.startvalue, doplot = TRUE)
   
## hypFit -
   xmpBasics("\nNext: MLE Fit to Hyperbolic Density > ")
   # Simulated random variates HYP(1, 0.3, 1, -1):
   s = rhyp(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1) 
   # Note, this may take some time.
   # Starting vector (1, 0, 1, mean(s)):
   hypFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), 
     doplot = TRUE, width = 0.5)
   
## nigFit -
   xmpBasics("\nNext: MLE Fit to Normal Inverse Gaussian Density > ")
   # Simulated random variates HYP(1.5, 0.3, 0.5, -1.0):
   s = rnig(n = 10000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1.0) 
   # Note, this may take some time.
   # Starting vector (1, 0, 1, mean(s)):
   nigFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), doplot = TRUE)
}


\seealso{
  
	\code{\link{dt}},
  	\code{\link{dhyp}}, 
  	\code{\link{dnig}}, 
  	\code{\link{nlm}}.
  	
}


\author{

	Diethelm Wuertz for this \R-port.
	
}


\keyword{distribution}

