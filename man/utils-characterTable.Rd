\name{characterTable}
\alias{characterTable}


\title{Table of characters}

\description{
  Displays a table of numerical equivalents to Latin characters.
}

\usage{ 
characterTable(font = 1, cex = 0.7)
}

\arguments{
  
  \item{cex}{

    a numeric value, determines the character size, the default 
    size is 0.7.

  }  
  \item{font}{

    an integer value, the number of the \code{font}, by default font
    number 1.

  }  
}

\value{
    
  displays a table with the characters of the requested font.  The
  character on line \code{"xy"} and column \code{"z"} of the table has
  code \code{"\xyz"}, e.g \code{cat("\\126")} prints: V for font number
  1. These codes can be used as any other characters.
    
}

\note{
  
  What happens with non-ASCII characters in plots is system dependent
  and depends on the graphics device, as well. Use of such characters is
  not recommended for portable code.
  
}

\seealso{
  \code{\link{colorTable}},
  \code{\link{symbolTable}}

  \code{\link[graphics]{points}} for use of characters in plotting
}

\examples{
## Character Table for Font 1:
# characterTable(font = 1)
}

\keyword{programming}
