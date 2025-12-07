\name{interactivePlot}
\alias{interactivePlot}


\title{Interactive Plot Utility}

\description{
  Plots with emphasis on interactive plots.
}

\usage{
interactivePlot(x, choices = paste("Plot", 1:9), 
    plotFUN = paste("plot.", 1:9, sep = ""), which = "all", \dots)
}

\arguments{
  \item{x}{
    an object to be plotted. 
  }
  \item{choices}{

    a character vector of length at most 9, giving descriptive names of
    the plots for the menu presented to the user.
    
  }
  \item{plotFUN}{

    a vector of the same length as \code{choices}, containing functions
    and/or names of functions. \code{plotFUN[[i]]} is called to produce
    the plot corresponding to \code{choice[i]}.

  } 
  \item{which}{
    
    Which graph(s) should be displayed?  One of the character strings
    \code{"ask"} (ask the user) or \code{"all"} (produce all plots), or
    a logical vector in which the positions of the \code{TRUE} values
    designate the plots to produce.
    
  }
  \item{\dots}{

    additional arguments passed to the FUN or plot function.
    (2023-10-21 GNB: currently the \code{"..."} arguments are not
    really passed on to the plotting functions.)
    
  }
}

\details{

  If \code{which} is the character string \code{"ask"}, then the user
  is presented with a menu to interactively choose which plot(s) to
  show.  Argument \code{choices} is used for the choices in the menu, so
  they should be informative.

  If \code{which} is equal to \code{"all"} all plots are drawn. If
  \code{which} is a logical vector, the indicated plots are displayed.

  Note that if more plots are to be shown in one window, the arrangement
  should be made in advance (and cleaned up afterwards), see the
  examples.

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
## Test Plot Function
testPlot <- function(x, which = "all", ...) {   
    ## Plot Function and Addons
    plot.1 <<- function(x, ...) plot(x, ...)      
    plot.2 <<- function(x, ...) acf(x, ...)
    plot.3 <<- function(x, ...) hist(x, ...)      
    plot.4 <<- function(x, ...) qqnorm(x, ...)
    ## Plot
    interactivePlot(
        x,
        choices = c("Series Plot", "ACF", "Histogram", "QQ Plot"),
        plotFUN = c("plot.1", "plot.2", "plot.3", "plot.4"),
        which = which, ...)       
    ## Return Value
    invisible()
}

## Plot
## prepare the window and store its previous state
op <- par(mfrow = c(2, 2), cex = 0.7)
## produce the plot
testPlot(rnorm(500))            
## restore the previous state
par(op)

## Try:
## par(mfrow = c(1,1))
## testPlot(rnorm(500), which = "ask")

## similar to above but using functions for plotFUN
testPlot_2 <- function(x, which = "all", ...) {   
    interactivePlot(
        x,
        choices = c("Series Plot", "ACF", "Histogram", "QQ Plot"),
        plotFUN = c(plot.1 = function(x, ...) plot(x, ...),
                    plot.2 = function(x, ...) acf(x, ...),
                    plot.3 = function(x, ...) hist(x, ...),
                    plot.4 = function(x, ...) qqnorm(x, ...) ),
        which = which, ...)       
   
    ## Return Value:
    invisible()
}
## produce the plot
op <- par(mfrow = c(2, 2), cex = 0.7)
testPlot_2(rnorm(500))            
par(op)
}

\keyword{hplot}
