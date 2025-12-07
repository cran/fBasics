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

    an object of class \code{"timeSeries"} or any other object which can
    be transformed by the function \code{as.timeSeries} into an object
    of class \code{"timeSeries"}.

  }  
}

\value{

  \code{NULL}, invisibly. \code{returnSeriesGUI} is used for the
  graphical user interface it provides.
    
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port
}

\seealso{
  \code{\link{seriesPlot}},
  \code{\link{returnPlot}},
  \code{\link{cumulatedPlot}},
  \code{\link{drawdownPlot}}

  \code{\link{qqnormPlot}},
  \code{\link{qqnigPlot}},
  \code{\link{qqghtPlot}},
  \code{\link{qqgldPlot}}

  \code{\link{histPlot}},
  \code{\link{densityPlot}},
  \code{\link{logDensityPlot}}

  \code{\link{boxPlot}},
  \code{\link{boxPercentilePlot}}

  \code{\link{acfPlot}},
  \code{\link{pacfPlot}},
  \code{\link{teffectPlot}},
  \code{\link{lacfPlot}}

  \code{\link{scalinglawPlot}}
}

\keyword{hplot}
