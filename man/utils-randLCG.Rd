\name{lcg}
\alias{lcg}

\alias{set.lcgseed}
\alias{get.lcgseed}
\alias{runif.lcg}
\alias{rnorm.lcg}
\alias{rt.lcg}


\title{Generator for Portable random innovations}

\description{

    Functions to generate portable random innovations. 
    The functions run under R and S-Plus and generate 
    the same sequence of random numbers. Supported 
    are uniform, normal and Student-t distributed 
    random numbers.
    \cr
        
    The functions are:
    
    \tabular{ll}{
    \code{set.lcgseed} \tab Set initial random seed, \cr
    \code{get.lcgseed} \tab Get the current valus of the random seed, \cr
    \code{runif.lcg} \tab Uniform linear congruational generator, \cr
    \code{rnorm.lcg} \tab Normal linear congruational generator, \cr
    \code{rt.lcg} \tab Student-t linear congruential generator. }
    
}


\usage{
set.lcgseed(seed = 4711)
get.lcgseed()

runif.lcg(n, min = 0, max = 1)
rnorm.lcg(n, mean = 0, sd = 1)
rt.lcg(n, df)   
}


\arguments{
  
  \item{seed}{
    an integer value, the random number seed.
  }
  \item{n}{
    an integer, the number of random innovations to be generated.
  }
  \item{df}{
    degrees of freedom, a positive number, may be non-integer.
  }
  \item{mean, sd}{
    mean and standard deviation of the normally distributed 
    innovations.
  }
  \item{min, max}{
    lower and upper limits of the uniformly distributed 
    innovations.
  }
            
}


\details{
    
    A simple portable random number generator for use in R and 
    SPlus. We recommend to use this generator only for comparisons 
    of calculations in R and Splus.

    The generator is a linear congruential generator with parameters
    \code{LCG(a=13445, c=0, m=2^31-1, X=0)}. It is a simple random 
    number generator which passes the bitwise randomness test.

}


\value{

    A vector of generated random innovations. The value of the
    current seed is stored in the variable \code{lcg.seed}.
    
}


\references{
Altman, N.S. (1988);
    \emph{Bitwise Behavior of Random Number Generators},
     SIAM J. Sci. Stat. Comput., 9(5), September, 941--949.
}


\examples{
set.lcgseed(seed = 65890)
 
## runif.lcg, rnorm.lcg, rt.lcg
cbind(runif.lcg(10), rnorm.lcg(10), rt.lcg(10, df = 4))

get.lcgseed()  
   
## Note, to overwrite rnorm, use
   # rnorm = rnorm.lcg
   # Going back to rnorm
   # rm(rnorm)
}

\keyword{distribution}
