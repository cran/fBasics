\name{HistogramPlot}
\alias{HistogramPlot}

\alias{histPlot}
\alias{densityPlot}
\alias{logDensityPlot}


\title{Histogram and density plots}

\description{

  Produce tailored histogram plots and kernel density/log-density
  estimate plots.
  
}

\usage{        
histPlot(x, labels = TRUE, col = "steelblue", fit = TRUE, 
    title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, \dots) 
densityPlot(x, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
    title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, \dots)    
logDensityPlot(x, labels = TRUE, col = "steelblue", robust = TRUE,  
    title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, \dots)    
}

\arguments{

  \item{x}{
    an object of class \code{"timeSeries"}.
  }
  \item{labels}{

    a logical flag, should the plot be returned with default labels
    and decorated in an automated way? By default \code{TRUE}.

  }   
  
  \item{col}{

    the color for the series. In the univariate case use just a color
    name like the default, \code{col="steelblue"}, in the multivariate
    case we recommend to select the colors from a color palette,
    e.g. \code{col=heat.colors(ncol(x))}.

  }
  \item{fit}{
    a logical flag, should a fit be added to the plot?
  }
  \item{hist}{

    a logical flag, by default \code{TRUE}. Should a histogram be laid
    under the plot?

  }
  \item{title}{

    a logical flag, by default \code{TRUE}. Should a default title be
    added to the plot?

  }
  \item{grid}{

    a logical flag, should a grid be added to the plot?  By default
    \code{TRUE}. To plot a horizontal lines only use \code{grid="h"} and
    for vertical lines use \code{grid="h"}, respectively.

  }
  \item{rug}{
    
    a logical flag, by default TRUE. Should a rug representation of the
    data be added to the plot?

  }
  \item{skip}{
    
    a logical flag, should zeros be skipped in the return Series?

  }
  \item{robust}{

    a logical flag, by default \code{TRUE}. Should a robust fit be added
    to the plot?

  }
  \item{\dots}{
    optional arguments to be passed on.
  }
    
}
\details{

  \code{histPlot} produces a tailored histogram plot.
  
  \code{densityPlot} produces a tailored kernel density estimate plot.
  
  \code{logDensityPlot} produces a tailored log kernel density estimate plot.
  
}
\value{
  \code{NULL}, invisibly. The functions are used for the side effect of
  producing a plot.
}

\examples{
## data
data(LPP2005REC, package = "timeSeries")
SPI <- LPP2005REC[, "SPI"]
plot(SPI, type = "l", col = "steelblue", main = "SP500")
abline(h = 0, col = "grey")
   
histPlot(SPI) 
   
densityPlot(SPI) 
}

\keyword{hplot}
