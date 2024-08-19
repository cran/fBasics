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

  Slot \code{fit} is a list. Its components depend on the fitting
  functions. Here is the meaning of some common ones:

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
      an integer indicating why the optimization process terminated.\cr
      1: relative gradient is close to zero, current iterate is probably 
          solution; \cr
      2: successive iterates within tolerance, current iterate is probably 
          solution; \cr
      3: last global step failed to locate a point lower than \code{estimate}. 
          Either \code{estimate} is an approximate local minimum of the 
          function or \code{steptol} is too small; \cr
      4: iteration limit exceeded; \cr
      5: maximum step size \code{stepmax} exceeded five consecutive times. 
          Either the function is unbounded below, becomes asymptotic to a 
          finite value from above in some direction or \code{stepmax} 
          is too small.
    }
    \item{gradient}{
      the gradient at the estimated maximum.
    }
  }

}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "fDISTFIT")}: ... }
  }
}

\examples{
showClass("fDISTFIT")
}

\keyword{classes}
