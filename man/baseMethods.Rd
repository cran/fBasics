\name{baseMethods}
\alias{baseMethods}

\alias{termPlot}
\alias{termPlot.default}

\alias{stdev}
\alias{stdev.default}


\title{Generic functions extensions}

\description{

  Basic extensions which add and/or modify additional functionality
  which is not available in R's basic packages.
    
}

\usage{
\method{stdev}{default}(x, na.rm = FALSE)

\method{termPlot}{default}(model, \dots)
}


\arguments{

  \item{model}{
    a fitted model object.
  }
  \item{x}{
    an object for which to compute the standard deviation.
  }

  \item{na.rm}{
    a logical value - should the NA values be removed.
  }  
  \item{\dots}{
    arguments to be passed.
  }

}

\details{

  \code{stdev} and \code{termPlot} are generic functions with default
  methods \code{stats::sd} and \code{stats::termplot}, respectively.

}

\seealso{
  \code{\link[stats]{sd}},
  \code{\link[stats]{termplot}}
}

\keyword{programming}
\keyword{stats}
