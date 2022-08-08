\name{decor}

\alias{decor}
\alias{hgrid}
\alias{vgrid}
\alias{boxL}
\alias{box_}
\alias{copyright}

\title{Functions for decorating plots}

\description{

    Functions for decorating plots.
    
}

\usage{
decor()

hgrid(ny = NULL, ...) 
vgrid(nx = NULL, ...) 

boxL(col = "white") 
box_(col = c("white", "black")) 

copyright()
}

\arguments{

  \item{col}{
    the color of the background, \code{"black"} and foreground
    \code{"white"} lines of the box.
  }
  \item{nx, ny}{
    number of cells of the grid in \code{x} or \code{y} direction. When \code{NULL}, 
    as per default, the grid aligns with the tick marks on the 
    corresponding default axis (i.e., tick marks as computed by axTicks). 
  } 
  \item{\dots}{
    additional arguments passed to the \code{grid()} function.
  }
  
}

\details{
  The plot decorating functions are:
  
  \describe{    
    \item{decor}{
      simple decoration function, equivalent to \code{hgrid()} followed
      by \code{boxL()},
    }
    \item{hgrid}{creates horizontal grid lines,}
    \item{vgrid}{creates vertical grid lines,}
    \item{boxL}{creates an L-shaped box,}
    \item{box_}{creates a bottom line box,}
    \item{copyright}{adds Rmetrics copyright to a plot.}
  }
}

\examples{
## Test Plot Function:
   plot(x = rnorm(100), type = "l", col = "red", 
     xlab = "", ylab = "Variates", las = 1)
   title("Normal Deviates", adj = 0)
   hgrid()
   boxL()
   copyright()
}

\keyword{hplot}
