
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             NORMAL TESTS:
#  shapiroTest           Shapiro-Wilk's test for normality
#  adTest                Anderson-Darling normality test
#  cvmTest               Cramer-von Mises normality test
#  lillieTest            Lilliefors (Kolmogorov-Smirnov) normality test 
#  pearsonTest           Pearson chi-square normality test 
#  sfTest                Shapiro-Francia normality test     
#  dagoTest              D'Agostino normality test
#  normalTest            S-Plus like function 
# FUNCTION:             DESCRIPTION:
#  bartlettTest          Bartlett's test for differences in variances
#  flignerTest           Fligner-Killeen's test for differences in variances
#  varTest               F test for differences in variances
#  ansariTest            Ansari-Bradley's test for differences in scale
#  moodTest              Mood's test for differences in scale
# FUNCTION:
#  corTest               A test for association between paired samples
#  ksTest                One or two sample Kolmogorov-Smirnov tests    
#                        ... for SPlus we need the tests from R's 'stats'  
# FUNCTION:             DESCRIPTION:
#  runsTest              Runs test for detecting non-randomness [tseries]
#  gofnorm               Reports on several tests of normality
################################################################################



shapiroTest = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs the Shapiro-Wilk test for normality. 
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   A function linked to "stats"
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # A Copy from stats for SPlus compatibility
    DNAME = deparse(substitute(x))
    x = sort(x[complete.cases(x)])
    n = length(x)
    if (n < 3 || n > 5000) 
        stop("sample size must be between 3 and 5000")
    rng = x[n] - x[1]
    if (rng == 0) 
        stop(paste("all", sQuote("x[]"), "are identical"))
    if (rng < 1e-10) 
        x = x/rng
    n2 = n%/%2
    sw = .C("swilk", init = FALSE, as.single(x), n, n1 = as.integer(n), 
        as.integer(n2), a = single(n2), w = double(1), pw = double(1), 
        ifault = integer(1), PACKAGE = "stats")
    if (sw$ifault && sw$ifault != 7) 
        stop(paste("ifault=", sw$ifault, ". This should not happen"))
    RVAL = list(statistic = c(W = sw$w), p.value = sw$pw, 
        method = "Shapiro-Wilk normality test", data.name = DNAME)
    class(RVAL) = "htest"
 
    # Return Value:
    RVAL
}


# ------------------------------------------------------------------------------


adTest =
function (x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Anderson-Darling normality test
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Source:
    #   Package: nortest
    #   Title: Tests for Normality
    #   Version: 1.0
    #   Author: Juergen Gross
    #   Description: 5 omnibus tests for the composite hypothesis of normality
    #   Maintainer: Juergen Gross <gross@statistik.uni-dortmund.de>
    #   License: GPL version 2 or newer
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    DNAME = deparse(substitute(x))
    x = sort(x[complete.cases(x)])
    n = length(x)
    if (n < 8) stop("sample size must be greater than 7")
    p = pnorm((x - mean(x))/sd(x))
    h = (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
    A = -n - mean(h)
    AA = (1 + 0.75/n + 2.25/n^2) * A
    if (AA < 0.2) {
        pval = 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
    } else if (AA < 0.34) {
        pval = 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2) 
    } else if (AA < 0.6) {
        pval = exp(0.9177 - 4.279 * AA - 1.38 * AA^2) 
    } else {
        pval = exp(1.2937 - 5.709 * AA + 0.0186 * AA^2) }
    RVAL = list(
        statistic = c(A = A), 
        p.value = pval, 
        method = "Anderson-Darling normality test", 
        data.name = DNAME)
    class(RVAL) = "htest"
    
    # Return Value:
    return(RVAL)
}


# ------------------------------------------------------------------------------


cvmTest = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cramer-von Mises normality test
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Notes:
    #   A copy from contributed R-package 'nortest'
    #   Source:
    #       Package: nortest
    #       Title: Tests for Normality
    #       Version: 1.0
    #       Author: Juergen Gross
    #       Description: 5 omnibus tests for the composite hypothesis of normality
    #       Maintainer: Juergen Gross <gross@statistik.uni-dortmund.de>
    #       License: GPL version 2 or newer
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    DNAME = deparse(substitute(x))
    x = sort(x[complete.cases(x)])
    n = length(x)
    if (n < 8) stop("sample size must be greater than 7")
    p = pnorm((x - mean(x))/sd(x))
    W = (1/(12 * n) + sum((p - (2 * seq(1:n) - 1)/(2 * n))^2))
    WW = (1 + 0.5/n) * W
    if (WW < 0.0275) {
        pval = 1 - exp(-13.953 + 775.5 * WW - 12542.61 * WW^2) }
    else if (WW < 0.051) {
        pval = 1 - exp(-5.903 + 179.546 * WW - 1515.29 * WW^2) }
    else if (WW < 0.092) {
        pval = exp(0.886 - 31.62 * WW + 10.897 * WW^2) }
    else {
        pval = exp(1.111 - 34.242 * WW + 12.832 * WW^2)}
    RVAL = list(
        statistic = c(W = W), 
        p.value = pval, 
        method = "Cramer-von Mises normality test", 
        data.name = DNAME)
    class(RVAL) = "htest"
    
    # Return Value:
    return(RVAL)
}


# ------------------------------------------------------------------------------


lillieTest = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Lilliefors (Kolmogorov-Smirnov) normality test  
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Notes:
    #   A copy from contributed R-package 'nortest'
    #   Source:
    #       Package: nortest
    #       Title: Tests for Normality
    #       Version: 1.0
    #       Author: Juergen Gross
    #       Description: 5 omnibus tests for the composite hypothesis of normality
    #       Maintainer: Juergen Gross <gross@statistik.uni-dortmund.de>
    #       License: GPL version 2 or newer
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    DNAME = deparse(substitute(x))
    x = sort(x[complete.cases(x)])
    n = length(x)
    if (n < 5) stop("sample size must be greater than 4")
    p = pnorm((x - mean(x))/sd(x))
    Dplus = max(seq(1:n)/n - p)
    Dminus = max(p - (seq(1:n) - 1)/n)
    K = max(Dplus, Dminus)
    if (n <= 100) {
        Kd = K
        nd = n }
    else {
        Kd = K * ((n/100)^0.49)
        nd = 100 }
    pvalue = exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * 
        Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) + 
        1.67997/nd)
    if (pvalue > 0.1) {
        KK = (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
        if (KK <= 0.302) {
            pvalue = 1 }
        else if (KK <= 0.5) {
            pvalue = 2.76773 - 19.828315 * KK + 80.709644 * 
                KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4 }
        else if (KK <= 0.9) {
            pvalue = -4.901232 + 40.662806 * KK - 97.490286 * 
                KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4 }
        else if (KK <= 1.31) {
            pvalue = 6.198765 - 19.558097 * KK + 23.186922 * 
                KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4 }
        else {
            pvalue = 0 } }
    RVAL = list(
        statistic = c(D = K), 
        p.value = pvalue, 
        method = "Lilliefors (Kolmogorov-Smirnov) normality test", 
        data.name = DNAME)
    class(RVAL) = "htest"
    
    # Return Value:
    return(RVAL)
}


# ------------------------------------------------------------------------------


pearsonTest = 
function (x, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Pearson chi-square normality test   
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   n.classes - the number of classes. The default is due 
    #       to Moore (1986). 
    #   adjust - a logical flag,  if TRUE (default), the p-value 
    #       is computed from a chi-square distribution with 
    #       n.classes-3 degrees of freedom, otherwise from a 
    #       chi-square distribution with n.classes-1 degrees of 
    #       freedom. 

    # Notes:
    #   A copy from contributed R-package 'nortest'
    #   Source:
    #       Package: nortest
    #       Title: Tests for Normality
    #       Version: 1.0
    #       Author: Juergen Gross
    #       Description: 5 omnibus tests for the composite hypothesis of normality
    #       Maintainer: Juergen Gross <gross@statistik.uni-dortmund.de>
    #       License: GPL version 2 or newer
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    DNAME = deparse(substitute(x))
    x = x[complete.cases(x)]
    n = length(x)
    if (adjust == TRUE) {
        dfd = 2 
    } else {
        dfd = 0 
    }
    num = floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
    count = tabulate(num, n.classes)
    prob = rep(1/n.classes, n.classes)
    xpec = n * prob
    h = ((count - xpec)^2)/xpec
    P = sum(h)
    pvalue = pchisq(P, n.classes - dfd - 1, lower.tail = FALSE)
    RVAL = list(
        statistic = c(P = P), 
        p.value = pvalue, 
        method = "Pearson chi-square normality test", 
        data.name = DNAME, 
        n.classes = n.classes, 
        df = n.classes - 1 - dfd)
    class(RVAL) = "htest"
    
    # Return Value:
    return(RVAL)
}


# ------------------------------------------------------------------------------


sfTest = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Shapiro-Francia normality test  
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Notes:
    #   A copy from contributed R-package 'nortest'
    #   Source:
    #       Package: nortest
    #       Title: Tests for Normality
    #       Version: 1.0
    #       Author: Juergen Gross
    #       Description: 5 omnibus tests for the composite hypothesis of normality
    #       Maintainer: Juergen Gross <gross@statistik.uni-dortmund.de>
    #       License: GPL version 2 or newer
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    DNAME = deparse(substitute(x))
    x = sort(x[complete.cases(x)])
    n = length(x)
    if ((n < 5 || n > 5000)) 
        stop("sample size must be between 5 and 5000")
    y = qnorm(ppoints(n, a = 3/8))
    W = cor(x, y)^2
    u = log(n)
    v = log(u)
    mu = -1.2725 + 1.0521 * (v - u)
    sig = 1.0308 - 0.26758 * (v + 2/u)
    z = (log(1 - W) - mu)/sig
    pval = pnorm(z, lower.tail = FALSE)
    RVAL = list(
        statistic = c(W = W), 
        p.value = pval, 
        method = "Shapiro-Francia normality test", 
        data.name = DNAME)
    class(RVAL) = "htest"
    
    # Return Value:
    return(RVAL)
}


# ******************************************************************************


dagoTest =
function(x, method = c("omnibus", "skewness", "kurtosis"))  
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   D'Agostino Test
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   method - the kind of test to be performed, either the
    #       "omnibus" (by default), the "skewness" or the "kurtosis"
    #       test. A character string.
    
    # Source:
    #   This function was inspired by ...
    #   http://adela.karlin.mff.cuni.cz/~klaster/vyuka/
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Settings:
    method = method[1]
    
    # Internal Function:
    skewness.test = function(x) {
       DNAME = deparse(substitute(x))
       x = x[complete.cases(x)]
       n = length(x)
       if (n < 8) stop("Sample size must be at least 8")
       meanX = mean(x)
       s =  sqrt(mean((x-meanX)**2))
       a3 = mean((x-meanX)**3)/s**3
       SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
       U3 = a3/SD3
       b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
       W2 = sqrt(2*(b-1))-1
       delta = 1/sqrt(log(sqrt(W2)))
       a = sqrt(2/(W2-1))
       Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
       pZ3 = 2*(1-pnorm(abs(Z3),0,1))
       names(Z3) = "Z3"
       RVAL = list(
          statistic = Z3,
          p.value = pZ3,
          method = "D'Agostino skewness normality test",
          data.name = DNAME)
       class(RVAL) = "htest"
       return(RVAL)}
        
    # Internal Function:
    kurtosis.test = function(x) {
       DNAME = deparse(substitute(x))
       x = x[complete.cases(x)]
       n = length(x)
       if (n < 20) stop("Sample size must be at least 20")
       meanX = mean(x)
       s =  sqrt(mean((x-meanX)**2))
       a4 = mean((x-meanX)**4)/s**4
       SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
       U4 = (a4-3+6/(n+1))/SD4
       B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))  
       A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
       jm = sqrt(2/(9*A))
       pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
       Z4 = (1-2/(9*A)-pos)/jm
       pZ4 = 2*(1-pnorm(abs(Z4),0,1))
       names(Z4) = "Z4"
       RVAL = list(
         statistic = Z4,
         p.value = pZ4,
         method = "D'Agostino kurtosis normality test",   
         data.name = DNAME)
       class(RVAL) = "htest"
       return(RVAL)}
        
    # Internal Function:
    omnibus.test = function(x) {
       DNAME = deparse(substitute(x))
       x = x[complete.cases(x)]
       n = length(x)
       if (n < 20) stop("sample size must be at least 20")
       meanX = mean(x)
       s =  sqrt(mean((x-meanX)**2))
       a3 = mean((x-meanX)**3)/s**3
       a4 = mean((x-meanX)**4)/s**4
       SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
       SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
       U3 = a3/SD3
       U4 = (a4-3+6/(n+1))/SD4
       b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
       W2 = sqrt(2*(b-1))-1
       delta = 1/sqrt(log(sqrt(W2)))
       a = sqrt(2/(W2-1))
       Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
       B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))  
       A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
       jm = sqrt(2/(9*A))
       pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
       Z4 = (1-2/(9*A)-pos)/jm
       omni = Z3**2+Z4**2
       pomni = 1-pchisq(omni,2)
       df = c(2)
       names(omni) = "Chi2"
       names(df) = "df"
       RVAL = list(
         statistic = omni,
         method = "D'Agostino omnibus normality test",
         parameter = df,
         p.value = pomni,
         data.name = DNAME)
       class(RVAL) = "htest"
       return(RVAL)}
       
    # Result:
    ans = NA
    if (method == "omnibus") ans = omnibus(x, method)
    if (method == "skewness") ans = skewness(x, method)
    if (method == "kurtosis") ans = kurtosis(x, method)
       
    # Return Value:
    ans }

    
# ------------------------------------------------------------------------------


normalTest =
function(x, method = c("sw", "jb")) 
{    # A function implemented by Diethelm Wuertz

    # Description:
    #   Shapiro-Wilk and Jarque-Bera Test
    
    # Notes:
    #   This function is for S-Plus compatibility
    
    # Transform:
    x = as.vector(x)
       
    # FUNCTION:
    
    # Test:
    if (method[1] == "sw") {
        ans = shapiroTest(x) 
    }
    # jbTest can be found in time series tests (tseriesTests)
    # A Copy for SPlus compatibility:
    if (method[1] == "jb") {
        if (NCOL(x) > 1) 
            stop("x is not a vector or univariate time series")
        if (any(is.na(x))) 
            stop("NAs in x")
        DNAME = deparse(substitute(x))
        n = length(x)
        m1 = sum(x)/n
        m2 = sum((x - m1)^2)/n
        m3 = sum((x - m1)^3)/n
        m4 = sum((x - m1)^4)/n
        b1 = (m3/m2^(3/2))^2
        b2 = (m4/m2^2)
        STATISTIC = n * b1/6 + n * (b2 - 3)^2/24
        names(STATISTIC) = "X-squared"
        PARAMETER = 2
        names(PARAMETER) = "df"
        PVAL = 1 - pchisq(STATISTIC, df = 2)
        METHOD = "Jarque Bera Test"
        ans = structure(list(statistic = STATISTIC, parameter = PARAMETER, 
            p.value = PVAL, method = METHOD, data.name = DNAME), 
            class = "htest")
    }
        
    # Return Value:
    ans
}

# ******************************************************************************


ansariTest = 
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Ansari-Bradley's test for differences in scale
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   A function copied from "stats" for SPlus compatibility
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Return Value:
    ansari.test(x = x, y = y, ...)
}


# ------------------------------------------------------------------------------


bartlettTest = 
function(x, g, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Bartlett's test for differences in variances
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   # A function linked to "stats"

    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Return Value:
    bartlett.test(x = x, g = g, ...)
}


# ------------------------------------------------------------------------------


corTest = 
function(x, y, alternative = c("two.sided", "less", "greater"),
method = c("pearson", "kendall", "spearman"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A test for association between paired samples
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   # A function linked to "stats"
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    alternative = alternative[1]
    method = method[1]
    ans = cor.test(x = x, y = y, alternative = alternative, 
        method = method, ...)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
    
    
flignerTest = 
function(x, g, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fligner-Killeen's test for differences in variances
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   A function linked to "stats"
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    ans = fligner.test(x = x, g = g, ...)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


ksTest = 
function(x, y, alternative = c("two.sided", "less", "greater"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   One or two sample Kolmogorov-Smirnov tests
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   A function linked to "stats"
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    alternative = alternative[1]
    ans = ks.test(x = x, y = y, alternative = alternative, ...)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


moodTest = 
function(x, y, alternative = c("two.sided", "less", "greater"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    alternative = alternative[1]
    ans = mood.test(x = x, y = y, alternative = alternative, ...)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


varTest = 
function(x, y, alternative = c("two.sided", "less", "greater"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   A function linked to "stats"
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Test:
    alternative = alternative[1]
    ans = var.test(x = x, y = y, alternative = alternative, ...)
    
    # Return Value:
    ans
}


# ******************************************************************************


runsTest = 
function (x)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs a runs test
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Notes:
    #   Implementing Trapletti's tseries R-Package

    # Note:
    #   We consider the signs of x in the series, the zeros will be 
    #   discarded. In addition we have to factor the data for runs.test().
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # runs.test() copied from A. Traplettis tseries package
    runs.test = function (x, alternative = c("two.sided", "less", "greater")) {
        if (!is.factor(x)) stop("x is not a factor")
        if (any(is.na(x))) stop("NAs in x")
        if (length(levels(x)) != 2) stop("x does not contain dichotomous data")
        alternative = match.arg(alternative)
        DNAME = deparse(substitute(x))
        n = length(x)
        R = 1 + sum(as.numeric(x[-1] != x[-n]))
        n1 = sum(levels(x)[1] == x)
        n2 = sum(levels(x)[2] == x)
        m = 1 + 2 * n1 * n2/(n1 + n2)
        s = sqrt(2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)/((n1 + n2)^2 * 
            (n1 + n2 - 1)))
        STATISTIC = (R - m)/s
        METHOD = "Runs Test"
        if (alternative == "two.sided") 
            PVAL = 2 * pnorm(-abs(STATISTIC))
        else if (alternative == "less") 
            PVAL = pnorm(STATISTIC)
        else if (alternative == "greater") 
            PVAL = pnorm(STATISTIC, lower.tail = FALSE)
        else stop("irregular alternative")
        names(STATISTIC) = "Standard Normal"
        structure(list(
            statistic = STATISTIC, 
            alternative = alternative, 
            p.value = PVAL, 
            method = METHOD, 
            data.name = DNAME), 
            class = "htest") }
            
    # Result:
    x = sign(x)
    x = x[x != 0]
    x = factor(x)
    ans = runs.test(x = x) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


gofnorm = 
function(x, doprint = TRUE) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Distribution: GoF-Tests
    #    1     Omnibus Moments Test for Normality
    #    2     Geary's Test of Normality
    #    3     Studentized Range for Testing Normality
    #    4     D'Agostino's D-Statistic Test of Normality
    #    5     Kuiper V-Statistic Modified to Test Normality
    #    6     Watson U^2-Statistic Modified to Test Normality
    #    7     Durbin's Exact Test (Normal Distribution)
    #    8     Anderson-Darling Statistic Modified to Test Normality
    #    9     Cramer-Von Mises W^2-Statistic to Test Normality
    #   10     Kolmogorov-Smirnov D-Statistic to Test Normality 
    #   11     Kolmogorov-Smirnov D-Statistic (Lilliefors Critical Values)
    #   12     Chi-Square Test of Normality (Equal Probability Classes)
    #   13     Shapiro-Francia W-Test of Normality for Large Samples
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   Function Calls:
    #   Fortran:
    #   SUBROUTINE GOFS(x,n,y1,y2,z1,z2,z3,z4,z5,z6,z7)
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Settings:
    lp1 = length(x)+1
        result = .Fortran("gofs",
            as.double(x),
            as.integer(length(x)),
            as.double(rep(0,times=13)),
            as.double(rep(0,times=13)),
            as.double(rep(0,times=lp1)),
            as.double(rep(0,times=lp1)),
            as.double(rep(0,times=lp1)),
            as.double(rep(0,times=lp1)),
            as.double(rep(0,times=lp1)),
            as.double(rep(0,times=lp1)),
            as.double(rep(0,times=lp1)),
            PACKAGE = "fBasics")
        statistics1 = result[[3]]
        statistics2 = rep(NA,times=13)
        statistics2[1] = result[[4]][1]
        statistics2[2] = result[[4]][2]
        statistics2[12] = result[[4]][12] 
            
    # Printing:
    if (doprint) { 
        paste (cat ('\n Omnibus Moments Test               '),
                    cat(c(statistics1[1],statistics2[1])))
        paste (cat ('\n Geary Test                         '),
                    cat(c(statistics1[2],statistics2[2])))
        paste (cat ('\n Studentized Range Test             '),
                    cat(statistics1[3]))
        paste (cat ('\n D\'Agostino D-Statistic Test        '),
                    cat(statistics1[4]))
        paste (cat ('\n Kuiper V-Statistic, Modified       '),
                    cat(statistics1[5]))
        paste (cat ('\n Watson U^2-Statistic, Modified     '),
                    cat(statistics1[6]))
        paste (cat ('\n Durbin Exact Test                  '),
                    cat(statistics1[7]))
        paste (cat ('\n Anderson-Darling Statistic         '),
                    cat(statistics1[8]))
        paste (cat ('\n Cramer-Von Mises W^2-Statistic     '),
                    cat(statistics1[9]))
        paste (cat ('\n Kolmogorov-Smirnov D-Statistic     '),
                    cat(statistics1[10]))
        paste (cat ('\n KS, Lilliefors Critical Values     '),
                    cat(statistics1[11]))
        paste (cat ('\n Chi-Square, Equal Prob. Classes    '),
                    cat(c(statistics1[12],statistics2[12])))
        paste (cat ('\n Shapiro-Francia W-Test             '),
                    cat(statistics1[13]))
        cat("\n\n")}    
    
    # Return Value:
    list(s1 = statistics1, s2 = statistics2)
}


# ##############################################################################
