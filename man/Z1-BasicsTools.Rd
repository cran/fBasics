\name{fBasicsTools}


\alias{fBasicsTools}

\alias{myFinCenter}
\alias{myUnits}
\alias{currentYear}

\alias{xmpBasics}
\alias{xmpfBasics}

\alias{modify}
\alias{modify.default}
\alias{atoms}
\alias{atoms.default}

\title{fBasics Tools}


\description{

	Tools used in the fBasics library.
	
}


\usage{
myFinCenter
currentYear

xmpfBasics()
xmpBasics(prompt = "")
}


\arguments{

  	\item{currentYear}{
  		the current year, formatted as \code{CCYY}.
  		}
  	\itam{myFinCenter}{
  		a character string denoting my financial center settings,
  		by default \code{"Zurich"}.
  		}
  	\itam{myUnits}{
  		a character string denoting current time units, one from
  		"secs", "mins", "hours", "days", or "weeks".
  		}
  	\item{prompt}{
		the string printed when prompting the user for input. 
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
\dontrun{
## xmpfBasics -
   # Popup the examples menu:
   xmpfBasics()
}
}


\keyword{programming}

