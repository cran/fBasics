\name{symbolTable}
\alias{symbolTable}


\title{Table of symbols}

\description{
  Displays a table of plot characters and symbols.
}

\usage{ 
symbolTable(font = par('font'), cex = 0.7)
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

  displays a table with the plot characters and symbols numbered from 
  0 to 255 and returns invisibly the name of the font.
       
}

\note{
  
  Symbols with codes on the range 128-255 are not legitimate in some
  locales, most notably UTF-8. Moreover, what happens with non-ASCII
  characters in plots is system dependent and depends on the graphics
  device, as well. Use of such characters is not recommended for
  portable code.

  From version 4031.95 of package \code{fBasics}, the characters are
  always defined as Latin1. In particular, in UTF8 locales the system
  converts them internally to UTF8. Still some symbols are not usable
  and non-ASCII symbols are not recommended, as pointed out above. For
  details, see the help page of \code{points()}, in particular the
  discussion of its argument \code{pch}.
  
}

\seealso{
  \code{\link{characterTable}},
  \code{\link{colorTable}}

  \code{\link[grDevices]{pdf}} for discussion of encodings for the pdf
  device
  
}

\examples{  
# symbolTable()
}

\keyword{programming}
