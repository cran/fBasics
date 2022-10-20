\name{BasicStatistics}
\alias{basicStats}


\title{Basic time series statistics}


\description{
    
    Computes basic financial time series statististics.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{basicStats} \tab Computes an overview of basic statistical values.}
    
}
    

\usage{ 
basicStats(x, ci = 0.95)
}


\arguments{
  
  \item{x}{

    an object of class \code{"timeSeries"} or any other object which can
    be transformed by the function \code{as.timeSeries} into an object
    of class \code{"timeSeries"}. The latter case, other than
    \code{"timeSeries"} objects, is more or less untested.

  }  
  \item{ci}{
    
    confidence interval, a numeric value, by default 0.95, i.e. 95\%.
    
  }
}

\details{

  Computes a number of sample statistics for each column of
  \code{x}. The statistics should be clear from the row names of the
  returned data frame.

  \code{"LCL"} and \code{"UCL"} stand for lower/upper confidence limits,
  computed under the null hypothesis of i.i.d.

  \code{"Kurtosis"} represents the \emph{excess kurtosis}, so its
  theoretical value for the normal distribution is zero, not 3.

  These statistics are often computed as a first step in the study of
  returns on financial assets. In that case any inference on these
  statistics (including the confidence intervals for the mean) should be
  considered exploratory, since returns are virtually never i.i.d.
  
}


\value{
       
  a data frame with one column for each column of \code{x} and the
  following row names:
  \code{"nobs"},
  \code{"NAs"},
  \code{"Minimum"},
  \code{"Maximum "},
  \code{"1. Quartile"},
  \code{"3. Quartile"},
  \code{"Mean"},
  \code{"Median"},
  \code{"Sum"},
  \code{"SE Mean"},
  \code{"LCL Mean"},
  \code{"UCL Mean"},
  \code{"Variance"},
  \code{"Stdev"},
  \code{"Skewness"},
  \code{"Kurtosis"}.
    
}


\examples{
\dontshow{set.seed(1234)}
## basicStats -
   # Simulated Monthly Return Data:
   tS = timeSeries(matrix(rnorm(12)), timeDate::timeCalendar())
   basicStats(tS)
}


\keyword{programming}

