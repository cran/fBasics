\name{basicStatistics}

\alias{basicStatistics}

\alias{basicStats}

\alias{skewness}
\alias{skewness.default}
\alias{skewness.data.frame}
\alias{skewness.POSIXct}
\alias{skewness.POSIXlt}

\alias{kurtosis}
\alias{kurtosis.default}
\alias{kurtosis.data.frame}
\alias{kurtosis.POSIXct}
\alias{kurtosis.POSIXlt}

\alias{rowStats} 
\alias{rowAvgs}
\alias{rowVars}
\alias{rowStdevs}
\alias{rowSkewness}
\alias{rowKurtosis}
\alias{rowCumsums}

\alias{colStats} 
\alias{colAvgs}
\alias{colVars}
\alias{colStdevs}
\alias{colSkewness}
\alias{colKurtosis}
\alias{colCumsums}

\title{Basic Statistics Summary}


\description{

	A collection of functions which compute basic statistical properties. 
	Missing functions in R to calculate skewness and kurtosis are added, 
	a function which creates a summary statistics, and functions to calculate 
	column statistcs.
	\cr
	
	The functions are:
	
	\tabular{rll}{
		1	\tab 	\code{skewness} 		
			\tab 	Returns value of skewness, \cr
		2	\tab 	\code{kurtosis} 		
			\tab 	Returns value of kurtosis, \cr	
		3	\tab 	\code{basicStats} 	
			\tab 	Computes an overview of basic statistical values, \cr	
		4	\tab 	\code{rowStats}, \code{colStats} 		
			\tab 	Calculates row/column statistics, \cr
		5	\tab 	\code{rowAvgs}, \code{colAvgs} 		
			\tab 	Calculates row/column means, \cr
		6	\tab 	\code{rowVars}, \code{colVars} 		
			\tab 	Calculates row/column variances, \cr
		7	\tab 	\code{rowStdevs}, \code{colStdevs} 		
			\tab 	Calculates row/column standard deviations, \cr
		8	\tab 	\code{rowSkewness}, \code{colSkewness} 	
			\tab 	Calculates row/column skewness, \cr
		9	\tab 	\code{rowKurtosis}, \code{colKurtosis} 	
			\tab 	Calculates row/column kurtosis, \cr
	   10 	\tab 	\code{rowCumsums}, \code{colCumsums} 	
	   		\tab 	Calculates row/column cumulated Sums. }
}


\usage{
skewness(x, \dots)
\method{skewness}{default}(x, na.rm = FALSE, \dots)
\method{skewness}{data.frame}(x, \dots)
\method{skewness}{POSIXct}(x, \dots)
\method{skewness}{POSIXlt}(x, \dots)

kurtosis(x, \dots)
\method{kurtosis}{default}(x, na.rm = FALSE, \dots)
\method{kurtosis}{data.frame}(x, \dots)
\method{kurtosis}{POSIXct}(x, \dots)
\method{kurtosis}{POSIXlt}(x, \dots)

basicStats(x, ci = 0.95)

rowStats(x, FUN, na.rm = FALSE, \dots) 
rowAvgs(x, na.rm = FALSE, \dots)
rowVars(x, na.rm = FALSE, \dots)
rowStdevs(x, na.rm = FALSE, \dots)
rowSkewness(x, na.rm = FALSE, \dots)
rowKurtosis(x, na.rm = FALSE, \dots)
rowCumsums(x, na.rm = FALSE, \dots)

colStats(x, FUN, na.rm = FALSE, \dots) 
colAvgs(x, na.rm = FALSE, \dots)
colVars(x, na.rm = FALSE, \dots)
colStdevs(x, na.rm = FALSE, \dots)
colSkewness(x, na.rm = FALSE, \dots)
colKurtosis(x, na.rm = FALSE, \dots)
colCumsums(x, na.rm = FALSE, \dots)
}


\arguments{
  
	\item{ci}{
		confidence interval, a numeric value, by default 0.95, 
		i.e. 95 percent.
		}
	\item{FUN}{
	    the statistical function to be applied.
	    }
		\item{na.rm}{
		a logical. Should missing values be removed?
		}
	\item{x}{
		a numeric vector, or a matrix for column statistics.
		}
	\item{\dots}{
	    arguments to be passed.
	    }
	    
}


\value{
	
	\code{skewness}, \code{kurtosis}
	\cr
	returns the value of the statistics, a numeric value.
	\cr
	
	\code{basicsStats}
	\cr
	returns data frame with the following entries and row names:
	nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
	Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
	Stdev, Skewness, Kurtosis.
	\cr
	
	\code{rowStats}, \code{rowAvgs}, \code{rowVars}, \code{rowStdevs},\cr
	\code{rowSkewness}, \code{rowKurtosis}, \code{rowCumsum}
	\cr 
	computes sample statistics by column. Missing values can be
	handled.
	\cr
	
	\code{colStats}, \code{colAvgs}, \code{colVars}, \code{colStdevs},\cr
	\code{colSkewness}, \code{colKurtosis}, \code{colCumsum}
	\cr 
	computes sample statistics by column. Missing values can be
	handled. 
	
}


\note{

	\R's-base package contains a function \code{colMeans} with an
	additional argument \code{dim=1}. Therefore, the function used 
	here to compute column means (averages) is named \code{colAvgs}.
	
}


\author{

	Diethelm Wuertz for this \R-port.
	
}


\seealso{

	\code{\link{colMeans}},
	\code{\link{mean}},
	\code{\link{median}},
	\code{\link{var}}.
	
}


\examples{
## basicStats -
   xmpBasics("\nStart: Basic Statistics of log-Returns > ")
   # Data NYSE Composite Index:
   data(nyseres)
   basicStats(nyseres)  
     
## moments -
   xmpBasics("\nNext: Moments, Skewness and Kurtosis > ")
   # Mean, Variance:
   mean(nyseres)
   var(nyseres)
   # Skewness, Kurtosis:
   # Note, can handele data.frames:
   skewness(nyseres)
   kurtosis(nyseres)   
}


\keyword{univar}

