\name{baseMethods}
\alias{baseMethods}

\alias{volatility}
\alias{volatility.default}

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
   
\method{volatility}{default}(object, \dots)
}


\arguments{

  \item{model}{
    a fitted model object.
  }
  \item{object}{
    an object from which to extract the volatility.
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

  \code{volatility} is a generic function, whose default method centers
  and squares the values in \code{object}. Other packages can (and do)
  define methods for it.

}

\seealso{
  \code{sd},
  \code{termplot}
}

\keyword{programming}
