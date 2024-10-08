\name{Ids}
\alias{Ids}

\alias{colIds}
\alias{rowIds}
\alias{colIds<-}
\alias{rowIds<-}


\title{Set and retrieve column/row names}

\description{
    
  Sets and retrieves column and row names. The functions
  are for compatibility with SPlus. 
  
}

\usage{
colIds(x, \dots)
rowIds(x, \dots)
}

\arguments{
  \item{x}{
    a numeric matrix.
  }
  \item{\dots}{
    passed on to \code{colnames} or \code{rownames}.
  }
}

\details{
    
  Usually in R the functions \code{colnames} and \code{rownames} are 
  used to retrieve and set the names of matrices. The functions 
  \code{rowIds} and \code{colIds}, are S-Plus like synonyms.
    
}

\examples{
## Create Pascal Matrix:
P <- pascal(3)
P
 
rownames(P) <- letters[1:3]
P   
   
colIds(P) <- as.character(1:3)
P                            
}

\keyword{math}
