\name{QuantileQuantilePlots}

\alias{qqnormPlot}
\alias{qqnigPlot}
\alias{qqghtPlot}
\alias{qqgldPlot}


\title{Quantile-Quantile plots}

\description{
    
    Returns quantile-quantile plots for the normal,
    the normal inverse Gaussian, the generalized 
    hyperbolic Student-t and the generalized lambda
    distribution.
    
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
    be transformed by\code{as.timeSeries} into an object of class
    \code{"timeSeries"}. The latter case, other then \code{"timeSeries"}
    objects, is more or less untested.

  }  
  \item{labels}{

    a logical flag, should the plot be returned with default labels and
    decorated in an automated way? By default \code{TRUE}.

  }  
  \item{col}{

    the color for the series. In the univariate case use just a color
    name like the default, \code{col="steelblue"}, in the multivariate
    case we recommend to select the colors from a color palette,
    e.g. \code{col=heat.colors(ncol(x))}.

  }
  \item{pch}{

    an integer value, by default 19. Which plot character should be used
    in the plot?

  }
  \item{title}{

    a logical flag, by default \code{TRUE}. Should a default title added
    to the plot?

  }
  \item{mtext}{

    a logical flag, by default \code{TRUE}. Should a marginal text be
    printed on the third site of the graph?

  }
  \item{grid}{

    a logical flag, should a grid be added to the plot?  By default
    \code{TRUE}. To plot a horizontal lines only use \code{grid="h"} and
    for vertical lines use \code{grid="h"}, respectively.

  } 
  \item{rug}{

    a logical flag, by default \code{TRUE} . Should a rug representation
    of the data added to the plot?

  }
  \item{scale}{

    a logical flag, by default \code{TRUE}. Should the time series be
    scaled for the investigation?

  }
  \item{\dots}{
    optional arguments to be passed.
  }
    
}

\details{
  List of Functions:
    
  \tabular{ll}{
    \code{qqnormPlot} Produces a tailored Normal quantile-quantile plot, \cr
    \code{qqnigPlot}  Produces a tailored NIG quantile-quantile plot, \cr
    \code{qqghtPlot}  Produces a tailored GHT quantile-quantile plot, \cr
    \code{qqgldPlot}  Produces a tailored GLD quantile-quantile plot.}
}

\value{
  displays a quantile-quantile plot.
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port.
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
