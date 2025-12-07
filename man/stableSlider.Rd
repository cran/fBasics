\name{StableSlider}
\alias{stableSlider}


\title{Slider GUI for Stable Distribution}

\description{
  The \code{stableSlider()} function provides interactive displays of
  density and probabilities of stable distributions.
}

\usage{
stableSlider(col= "steelblue", col.med = "gray30")
}

\arguments{
  \item{col}{colour for the density and distributions functions.}
  \item{col.med}{colour for the median.}
}

\value{
    The \code{stableSlider()} function displays densities and
    probabilities of the skew stable distribution, for educational
    purposes.
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port
}

\references{
  see those in \code{\link[stabledist]{dstable}}, in package \pkg{stabledist}.
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

  \code{\link{returnSeriesGUI}}
}

\examples{
if(dev.interactive())
   stableSlider()
}

\keyword{distribution}
