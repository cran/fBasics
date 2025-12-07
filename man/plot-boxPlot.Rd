\name{BoxPlot}
\alias{boxPlot}

\alias{boxPercentilePlot}


\title{Time series box plots}

\description{
    
  Produce a box plot or a box percentile plot.

}

\usage{ 
boxPlot(x, col = "steelblue", title = TRUE, \dots)
boxPercentilePlot(x, col = "steelblue", title = TRUE, \dots) 
}

\arguments{
  
  \item{x}{
    
    an object of class \code{"timeSeries"} or any other object which can
    be transformed by the function \code{as.timeSeries} into an object
    of class \code{"timeSeries"}.
    
  }  
  \item{col}{

    the color for the series. In the univariate case use just a color
    name like the default, \code{col = "steelblue"}, in the multivariate
    case we recommend to select the colors from a color palette,
    e.g. \code{col = heat.colors(ncol(x))}.

  }
  \item{title}{

    a logical flag, by default \code{TRUE}. Should a default title added
    to the plot?

  }
  \item{\dots}{
    optional arguments to be passed to \code{\link{boxplot}}.
  }
    
}
\details{

  \code{boxPlot} produces a side-by-side standard box plot,

  \code{boxPercentilePlot} produces a side-by-side box-percentile plot.

}

\value{
  \code{NULL}
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

  \code{\link{acfPlot}},
  \code{\link{pacfPlot}},
  \code{\link{teffectPlot}},
  \code{\link{lacfPlot}}

  \code{\link{scalinglawPlot}}

  \code{\link{returnSeriesGUI}}
}
\examples{
## data
data(LPP2005REC, package = "timeSeries")
LPP <- LPP2005REC[, 1:6]
plot(LPP, type = "l", col = "steelblue", main = "SP500")
abline(h = 0, col = "grey")
   
boxPlot(LPP) 
}

\keyword{hplot}
