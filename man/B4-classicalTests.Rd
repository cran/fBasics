\name{ClassicalTests}

\alias{ClassicalTests}

\alias{ansariTest}
\alias{bartlettTest}
\alias{corTest}
\alias{flignerTest}
\alias{ksTest}
\alias{moodTest}
\alias{runsTest}
\alias{shapiroTest}
\alias{varTest}

% from lmtest:

\alias{adTest}             
\alias{cvmTest}     
\alias{lillieTest} 
\alias{pearsonTest}    
\alias{sfTest}              

\alias{dagoTest}   
\alias{normalTest}           


\alias{gofnorm}

\title{Classical Statistical Tests}


\description{
    
    A collection and description of functions for 
    conventional statistical tests. Most tests are 
    selected from \R's 'cTest' and 'nortest' packages. 
    The tests may be of interest for many financial and 
    economic applications. 
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{ansariTest} \tab  Ansari--Bradley's test for differences in scale, \cr
    \code{bartlettTest} \tab Bartlett's test for differences in variances, \cr
    \code{corTest} \tab A test for association between paired samples, \cr
    \code{flignerTest} \tab Fligner--Killeen's test for differences in variances, \cr
    \code{ksTest} \tab One or two sample Kolmogorov-Smirnov tests, \cr
    \code{moodTest} \tab Mood's test for differences in scale parameters, \cr
    \code{shapiroTest} \tab Shapiro-Wilk's test for normality, \cr
    \code{varTest} \tab F test for differences in variances, \cr  
    \code{dagoTest} \tab D'Agostino normality test, \cr
    \code{normalTest} \tab S-Plus like Test for Shapiro-Wilk and Jarque-Bera tests, \cr
    \code{runsTest} \tab Runs test for detecting non-randomness, \cr
    \code{gofnorm} \tab Prints a report on 13 different tests of normality. }
            
    The following tests are available from the \code{"nortest"} package: 
    
    \tabular{ll}{
    \code{adTest} \tab Anderson--Darling normality test, \cr
    \code{cvmTest} \tab Cramer--von Mises normality test, \cr
    \code{lillieTest} \tab Lilliefors (Kolmogorov-Smirnov) normality test, \cr
    \code{pearsonTest} \tab Pearson chi-square normality test, \cr
    \code{sfTest} \tab Shapiro-Francia normality test. }       
     
}


\usage{
ansariTest(x, y, \dots)
bartlettTest(x, g, \dots)
corTest(x, y, alternative = <<see below>>, method = <<see below>>, \dots)
flignerTest(x, g, \dots)
ksTest(x, y, alternative = <<see below>>, \dots)
moodTest(x, y, alternative = <<see below>>, \dots)
shapiroTest(x)
varTest(x, y, alternative = <<see below>>, \dots)

adTest(x)            
cvmTest(x)      
lillieTest(x) 
pearsonTest(x, n.classes = <<see below>>, adjust = TRUE)    
sfTest(x)   

dagoTest(x, method = c("omnibus", "skewness", "kurtosis"))
normalTest(x, method = <<see below>>) 

runsTest(x)

gofnorm(x, doprint = TRUE)
}


\arguments{
  
    \item{adjust} {
        [pearsonTest] - \cr
        a logical flag, if \code{TRUE}, the default, the p-value 
        is computed from a chi-square distribution with 
        \code{n.classes-3} degrees of freedom, otherwise from a 
        chi-square distribution with n.classes-1 degrees of freedom. 
        }
    \item{alternative}{
        indicates the alternative hypothesis; must be one of the 
        entries specified by the input vector \code{c("two.sided", 
        "less", "greater")}.
        }
    \item{doprint}{
        if TRUE, an exhaustive report is printed.
        }
    \item{g}{
        a vector or factor object giving the group for the
        corresponding elements of \code{x}. Ignored if \code{x} 
        is a list.
        }
    \item{method}{
        [corTest] - \cr
        indicates the three different methods for the correlation
        test; must be one of the entries specified by the input 
        vector \code{c("pearson", "kendall", "spearman")}.
        [dagoTest] - \cr
        selects the kind of test to be performed, either the
        \code{"omnibus"} (by default), the \code{"skewness"} or 
        the \code{"kurtosis"} test. A character string.
        [normalTest] - \cr
        indicates the two different methods for the normality
        test, Shapiro-Wilk or Jarque-Bera Test. Must be one of 
        the entries specified by the input vector \code{c("sw", "jb")}.
        }    
    \item{n.classes}{
        [pearsonTest] - \cr
        the number of classes. The default is due to Moore (1986):
        \code{n.classes = ceiling(2 * (n^(2/5)))}.
        }
    \item{x, y}{
        a numeric vector of data values.
        }
    \item{\dots}{
        arguments passed to the underlying function from R's
        \code{ctest} package. Consult the appropriate manual pages.
        }
       
}


\value{

    The tests return a list with class \code{"htest"} containing 
    the following components:

    \item{statistic}{
        the value of the test statistic.
        }
    \item{p.value}{
        the p-value of the test.
        }
    \item{alternative}{
        a character string describing the alternative
        hypothesis.
        }
    \item{method}{
        a character string indicating what type of test was performed.
        }
    \item{data.name}{
        a character string giving the name(s) of the data.
        }
        
}


\details{

    \bold{Report from gofnorm:}
    \cr\cr
    The function reports the following goodness-of-fit tests
    for normality:

    \tabular{rl}{
     1  \tab   Omnibus Moments Test for Normality \cr
     2  \tab   Geary's Test of Normality \cr
     3  \tab   Studentized Range for Testing Normality \cr
     4  \tab   D'Agostino's D-Statistic Test of Normality \cr
     5  \tab   Kuiper V-Statistic Modified to Test Normality \cr
     6  \tab   Watson U-Squared-Statistic Modified to Test Normality \cr
     7  \tab   Durbin's Exact Test (Normal Distribution \cr
     8  \tab   Anderson-Darling Statistic Modified to Test Normality \cr
     9  \tab   Cramer-Von Mises W-Squared-Statistic to Test Normality \cr
    10  \tab   Kolmogorov-Smirnov D-Statistic to Test Normality \cr
    11  \tab   Kolmogorov-Smirnov D-Statistic (Lilliefors Critical Values) \cr
    12  \tab   Chi-Square Test of Normality (Equal Probability Classes) \cr
    13  \tab   Shapiro-Francia W-Test of Normality for Large Samples }
    
    The functions are implemented from the GRASS GIS software package
    an Open Source project avalaible under the GNU GPL license.

}


\references{
Anderson T.W., Darling D.A. (1954); 
    \emph{A Test of Goodness of Fit},
    JASA 49:765--69.
    
Conover, W. J. (1971);
    \emph{Practical nonparametric statistics},
    New York: John Wiley & Sons.

D'Agostino R.B., Pearson E.S. (1973); 
    \emph{Tests for Departure from Normality},
    Biometrika 60, 613--22.

D'Agostino R.B., Rosman B. (1974); 
    \emph{The Power of Geary's Test of Normality},
    Biometrika 61, 181--84.

Durbin J. (1961); 
    \emph{Some Methods of Constructing Exact Tests},
    Biometrika 48, 41--55. 

Durbin,J. (1973);
    \emph{Distribution Theory Based on the Sample Distribution Function},
    SIAM, Philadelphia.

Geary R.C. (1947); 
    \emph{Testing for Normality}; 
    Biometrika 36, 68--97.

Kotz S. (1973); 
    \emph{Normality versus Lognormality with Applications},
    Communications in Statistics 1, 113--32.

Lehmann E.L. (1986); 
    \emph{Testing Statistical Hypotheses}, 
    John Wiley and Sons, New York.

Linnet K. (1988); 
    \emph{Testing Normality of Transformed Data},
    Applied Statistics 32, 180--186. 
    
Moore, D.S. (1986);
    \emph{Tests of the chi-squared type}, 
    In: D'Agostino, R.B. and Stephens, M.A., eds., 
    Goodness-of-Fit Techniques, Marcel Dekker, New York. 

Shapiro S.S., Francia R.S. (1972); 
    \emph{An Approximate Analysis of Variance Test for Normality},
    JASA 67, 215--216.

Shapiro S.S., Wilk M.B., Chen V. (1968); 
    \emph{A Comparative Study of Various Tests for Normality},
    JASA 63, 1343--72.

Thode H.C. (2002);
    \emph{Testing for Normality}, 
    Marcel Dekker, New York. 

Weiss, M.S. (1978); 
    \emph{Modification of the Kolmogorov-Smirnov 
        Statistic for Use with Correlated Data}, 
    JASA 73, 872--75.
}


\examples{
## ansariTest - moodTest - varTest -
   xmpBasics("\nStart: Ansari/Mood/Var Test > ")
   # Differences in scale / variances:
   data(nyseres)
   x = nyseres[4001:5000, 1]
   y = nyseres[6001:7000, 1]
   p = c(
     ansariTest(x, y)$p.value,
     moodTest(x, y)$p.value,
     varTest(x, y)$p.value)
   test = c("ansari", "mood", "var") 
   data.frame(test, p)
   
## ansariTest - moodTest - varTest -
   xmpBasics("\nNext: Ansari/Mood/Var Test > ")
   # Differences in scale / variances:
   x = rnorm(1000)
   y = rnorm(1000)
   p = c(
     ansariTest(x, y)$p.value,
     moodTest(x, y)$p.value,
     varTest(x, y)$p.value)
   test = c("ansari", "mood", "var") 
   data.frame(test, p)
   
## gofnorm -
   xmpBasics("\nNext: Goodness-of-Fit Test for Normality > ")  
   x = nyseres[1:1000, 1]
   # Standardize the data:
   x = (x-mean(x))/sqrt(var(x))
   # Test:
   r = gofnorm(x, doprint = TRUE)
   
## ksTest -
   # unique: remove ties:
   xmpBasics("\nNext: Kolmogorov-Smirnov Goodness-of-Fit Test > ")
   nyseres = unique(nyseres[, 1])
   ksTest(nyseres, "pnorm")
   # Do x and y come from the same distribution?
   x = nyseres[1:(length(nyseres)/2)]
   y = nyseres[(length(nyseres)/2+1):length(nyseres)]
   ksTest(x, y)
   # Does x come from an alpha stable distribution?
   alpha = scalinglawPlot(nyseres, doplot = FALSE)$exponent
   ksTest(x = nyseres, y = "psymstb", alpha, alternative = "gr")
   
## runsTest -
   xmpBasics("\nNext: Runs Test > ")
   runsTest(nyseres)
   runsTest(rnorm(length(nyseres)))
}


\author{

    R-core team for the tests from \R's ctest package,\cr
    Adrian Trapletti for the runs test from \R's tseries package,\cr
    Juergen Gross for the normal tests from \R's nortest package,\cr
    James Filliben for the Fortran program producing the runs report,\cr
    Paul Johnson for the Fortran program producing the gofnorm report, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{htest}

