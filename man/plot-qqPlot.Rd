\name{QuantileQuantilePlots}

\alias{qqnormPlot}
\alias{qqnigPlot}
\alias{qqghtPlot}
\alias{qqgldPlot}

\concept{quantile-quantile plot}
\concept{qq-plot}
\concept{qqplot}

\concept{normal distribution}
\concept{inverse Gaussian distribution}
\concept{generalized hyperbolic Student-t distribution}
\concept{generalized lambda distribution}

\title{Quantile-quantile plots}

\description{
    
    Produce quantile-quantile plots for the normal, inverse Gaussian,
    generalized hyperbolic Student-t and the generalized lambda
    distributions.
    
}

\usage{     
qqnormPlot(x, labels = TRUE, col = "steelblue", pch = 19,
    title = TRUE, mtext = TRUE, grid = FALSE, rug = TRUE, 
    scale = TRUE, \dots) 
qqnigPlot(x, labels = TRUE, col = "steelblue", pch = 19,
    title = TRUE, mtext = TRUE, grid = FALSE, rug = TRUE, 
    scale = TRUE, \dots) 
qqghtPlot(x, labels = TRUE, col = "steelblue", pch = 19,
    title = TRUE, mtext = TRUE, grid = FALSE, rug = TRUE, 
    scale = TRUE, \dots) 
qqgldPlot(x, labels = TRUE, col = "steelblue", pch = 19,
    title = TRUE, mtext = TRUE, grid = FALSE, rug = TRUE, 
    scale = TRUE, \dots)
}

\arguments{
  \item{x}{

    an object of class \code{"timeSeries"} or any other object which can
    be transformed by \code{as.timeSeries}.

  }  
  \item{labels}{

    a logical flag, should the plot be returned with default labels and
    decorated in an automated way? By default \code{TRUE}.

  }  
  \item{col}{

    the color for the series. In the univariate case use just a color
    name like the default, \code{col = "steelblue"}, in the multivariate
    case we recommend to select the colors from a color palette,
    e.g. \code{col = heat.colors(ncol(x))}.

  }
  \item{pch}{

    an integer value, by default 19. Which plot character should be used
    in the plot?

  }
  \item{title}{

    a logical flag, by default \code{TRUE}. Should a default title be
    added to the plot?

  }
  \item{mtext}{

    a logical flag, by default \code{TRUE}. Should a marginal text be
    printed on the third site of the graph?

  }
  \item{grid}{

    a logical flag, should a grid be added to the plot?  By default
    \code{TRUE}.

  } 
  \item{rug}{

    a logical flag, by default \code{TRUE}. Should a rug representation
    of the data be added to the plot?

  }
  \item{scale}{

    a logical flag, by default \code{TRUE}. Should the plot be for the
    scaled time series? Used by \code{qqnormPlot} only, ignored silently
    by the others.

  }
  \item{\dots}{
    optional arguments passed to \code{plot()}.
  }
    
}

\details{
    
  \code{qqnormPlot} produces a tailored Normal quantile-quantile plot.
  
  \code{qqnigPlot}  produces a tailored NIG quantile-quantile plot.
  
  \code{qqghtPlot}  produces a tailored GHT quantile-quantile plot.
  
  \code{qqgldPlot}  produces a tailored GLD quantile-quantile plot.
}

\value{
  a list containing some of the quantities computed for the plot,
  invisibly. Currently contains the following components:
  \item{x}{the quantiles of the reference distribution, used for the
    x-axis,}
  \item{y}{the (possibly scaled) ordered values of the time series, used
    for the y-axis.}

  The list has attribute \code{"control"} containing the parameters of
  the fitted distribution.
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port.
}

\seealso{
  \code{\link{seriesPlot}},
  \code{\link{returnPlot}},
  \code{\link{cumulatedPlot}},
  \code{\link{drawdownPlot}}

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

  \code{\link{returnSeriesGUI}}
}
\examples{
## data
data(LPP2005REC, package = "timeSeries")
SPI <- LPP2005REC[, "SPI"]
plot(SPI, type = "l", col = "steelblue", main = "SP500")
abline(h = 0, col = "grey")
   
qqnormPlot(SPI) 
}

\keyword{hplot}
