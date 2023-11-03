\name{fHTEST-class}
\Rdversion{1.1}
\docType{class}

\alias{fHTEST-class}
\alias{fHTEST}
\alias{show,fHTEST-method}


\title{Class \code{"fHTEST"}}

\description{

  An S4 class representing the outcome of a statistical test.
  
}
\section{Objects from the Class}{
% Objects can be created by calls of the form \code{new("fHTEST", ...)}.

Objects are created by some statistical test functions.

%%  ~~ describe objects here ~~ 

}
\section{Slots}{
  \describe{
    \item{\code{call}:}{
      the function call.
    }
    \item{\code{data}:}{
      the data as specified by the input argument(s).
    }
    \item{\code{test}:}{

      a list whose elements contain the results from the statistical
      test. The information provided is similar to a list object of
      class \code{"htest"}.

    }
    \item{\code{title}:}{

      a character string with the name of the test. This can be
      overwritten specifying a user defined input argument.

    }
    \item{\code{description}:}{
      
      a character string with an optional user defined description.  By
      default just the current date when the test was applied will be
      returned.
      
    }
  }

  Slot \code{@test} is an object of class \code{"list"} containing at
  least the following elements:

  \describe{
    \item{statistic}{
      the value(s) of the test statistic.
    }
    \item{p.value}{
      the p-value(s) of the test.
    }
    \item{parameters}{
      a numeric value or vector of parameters.
    }
    \item{estimate}{
      a numeric value or vector of sample estimates.
    }
    \item{conf.int}{
      a numeric two row vector or matrix of 95\% confidence levels.
    }
    \item{method}{
      a character string indicating what type of test was performed.
    }
    \item{data.name}{
      a character string giving the name(s) of the data.
    }
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "fHTEST")}: ... }
  }
}

\seealso{
  for functions returning objects from class \code{"fHTEST"}, see
  \code{\link{scaleTest}},
  \code{\link{correlationTest}},
  \code{\link{ks2Test}},
  \code{\link{locationTest}},
  \code{\link{NormalityTests}},
  \code{\link{varianceTest}}
}

\examples{
showClass("fHTEST")
}

\keyword{classes}
\keyword{htest}
