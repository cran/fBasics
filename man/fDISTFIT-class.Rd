\name{fDISTFIT-class}
\Rdversion{1.1}
\docType{class}
\alias{fDISTFIT-class}

\alias{show,fDISTFIT-method}


\title{Class \code{"fDISTFIT"}}
\description{

  S4 class representing fitted distributions.
  
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("fDISTFIT", ...)}
  but are typically created by functions fitting distributions.
  
  
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"call"} ~~ }
    \item{\code{model}:}{Object of class \code{"character"} ~~ }
    \item{\code{data}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{fit}:}{Object of class \code{"list"} ~~ }
    \item{\code{title}:}{Object of class \code{"character"} ~~ }
    \item{\code{description}:}{Object of class \code{"character"} ~~ }
  }

  Slot \code{fit} is a list with components:

  \describe{

    \item{estimate}{
      the point at which the maximum value of the log liklihood 
      function is obtained.
    }
    \item{minimum}{
      the value of the estimated maximum, i.e. the value of the
      log liklihood function.
    }
    \item{code}{
      an integer indicating why the optimization process terminated.
    }
    \item{gradient}{
      the gradient at the estimated maximum.
    }

  }

  The above description of slot \code{fit} is taken from the
  documentation for \code{\link{tFit}}, \code{\link{nFit}} and
  \code{\link{stableFit}}. \strong{TODO:} needs checking and also is it the same for
  other distribution fitting functions.
  
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "fDISTFIT")}: ... }
  }
}

%\references{
%%%  ~~put references to the literature/web site here~~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
showClass("fDISTFIT")
}
\keyword{classes}
