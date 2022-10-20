\name{gridVector}
\alias{gridVector}


\title{Grid vector coordinates}


\description{

  Creates rectangular grid coordinates from two vectors.
    
}


\usage{
gridVector(x, y = NULL)}


\arguments{

    \item{x, y}{
        two numeric vectors of length m and n which span the 
        rectangular grid of size m times n. If \code{y} takes
        the default value, \code{NULL}, then \code{y=x}.
        }
        
}


\value{

  a list with two entries named \code{$X} and \code{$Y},
  giving the coordinates which span the bivariate grid.

}


\seealso{

    \code{\link{expand.grid}}.

}


\examples{
## gridVector -
   gridVector((0:10)/10) 
   gridVector((0:10)/10, (0:10)/10)
}


\keyword{hplot}

