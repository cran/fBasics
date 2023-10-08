\name{gridVector}
\alias{gridVector}


\title{Grid vector coordinates}

\description{
  Creates rectangular grid coordinates from two vectors.
}

\usage{
gridVector(x, y = NULL)
}

\arguments{

  \item{x,y}{
    numeric vectors
  }
}

\details{

  The grid is obtained by pairing each element of \code{y} with all
  elements of \code{x}. The \code{X} and \code{Y} coordinates of the
  points are stored in separate vectors. This is convenient, for
  example, for plotting. It can be useful also for brute force
  optimisation or simulation.
      
  If \code{y} is \code{NULL}, the default, then \code{y = x}.

}

\value{

  a list with two components, \code{X} and \code{Y}, giving the
  coordinates which span the bivariate grid.

}

\seealso{
  \code{\link{expand.grid}}
}

\examples{
## a small grid vector with row and col transformations
gridVector(0:2)
data.frame(gridVector(0:2))
do.call("rbind", gridVector(0:2))

gridVector(0:2, 0:3)

## grid over a unit square
gridVector((0:10)/10) # equivalently: gridVector((0:10)/10, (0:10)/10)
}

\keyword{hplot}
