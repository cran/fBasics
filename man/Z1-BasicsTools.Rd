\name{fBasicsTools}


\alias{fBasicsTools}

\alias{xmpBasics}
\alias{xmpfBasics}

\alias{unirootNA}

\title{fBasics Tools}


\description{

	Tools used in the fBasics library.
	\cr
}


\usage{
xmpfBasics()
xmpBasics(prompt = "")

unirootNA(f, interval, lower = min(interval), upper = max(interval), 
	tol = .Machine$double.eps^0.25, \dots) 
}


\arguments{

  	\item{f}{
  		[unirootNA] - \cr
  		the function for which the root is sought.
  		}
  	\item{interval}{
  		[unirootNA] - \cr
  		a vector containing the end-points of the interval to 
  		be searched for the root.
  		}
  	\item{lower, upper}{
  		[unirootNA] - \cr
  		the lower and upper end points of the interval to be searched.
  		}
  	\item{prompt}{
		the string printed when prompting the user for input. 
		}	
	\item{tol}{
  		[unirootNA] - \cr
  		the desired accuracy or the convergence tolerance.
  		}
  	\item{\dots}{
  		[unirootNA] - \cr
  		additional arguments to f.
  		}
}


\value{
  
	xmpfBasics
	\cr
	Popups the example menu.
	\cr
	
	xmpBasics
	\cr
	Nothing, the default, or the the prompt if you have set
	\code{xmpBasics = readline} on the command prompt. 
	
}


\note{

	The example in the manual pages may be interactive and ask for
	input from the user. To achieve this you have to type on the
	command line: \code{xmpBasics = readline}

}


\author{

	Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## SOURCE("fBasics.A0-SPlusCompatibility")
## SOURCE("fBasics.Z1-BasicsTools")
}


\keyword{programming}

