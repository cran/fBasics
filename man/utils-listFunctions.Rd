\name{listFunctions}
\alias{listFunctions}

\alias{countFunctions}
\alias{listIndex}


\title{List exported functions in a package}

\description{

  Utilities to list and count exported functions in a package, 
  list the contents of the description file of a package, and 
    
  Prints the content of an index file for a package (a list of the
  objects exported by a package).
}


\usage{
listFunctions(package, character.only = FALSE)
countFunctions(package, character.only = FALSE)

listIndex(package, character.only = FALSE)
}


\arguments{

  \item{package}{

    a literal character string or a character string denoting the name
    of a package.

  }
  \item{character.only}{

    a logical indicating whether 'package' can be assumed to be
    a character string.

  }
}

\value{

  for \code{listFunctions}, a character vector containing the names of
  the exported functions in a package,

  for \code{countFunctions}, a named numeric value giving the number of
  the exported functions in a package.

  \code{listIndex} doesn't return a useful value. It is used for the
  side effect of printing the description or index.

}

\note{
  
  Be aware that \code{listFunctions} and \code{countFunctions} attach
  the package to the search path. 

}

\seealso{
  \code{\link{packageDescription}}
}


\examples{
## listFunctions -
   listFunctions("fBasics")
   
## countFunctions -
   countFunctions("fBasics")

}


\keyword{package}
