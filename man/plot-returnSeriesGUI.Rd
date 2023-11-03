\name{ReturnSeriesGUI}
\alias{returnSeriesGUI}


\title{Return series plots}

\description{
    
  A graphical user interface to display finanical time series plots.
  \code{returnSeriesGUI} opens a GUI for return series plots.

}
    

\usage{ 
returnSeriesGUI(x)
}

\arguments{
  
    \item{x}{

      an object of class \code{"timeSeries"} or any other object which
      can be transformed by the function \code{as.timeSeries} into an
      object of class \code{timeSeries}. The latter case, other than
      \code{timeSeries} objects, is more or less untested.
	
    }  
}

\value{
       
  For \code{returnSeriesGUI} function, besides the graphical
  user interface no values are returned.
    
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port.
}

\keyword{hplot}
