
#
# Example: 
#   Explore the symmetric and skew stable distributions
#   
# Description: 
#   Part I: Explore Symmetric Stable Distributions. The example creates 
#     random deviates from a symmetric stable df close to Cauchy 
#     distribution, alpha=1.001, and close to a Gaussian df, alpha=1.999. 
#     It plots a histogram and compares the empirical dfs for two alphas, 
#     alpha=0.5 and 1.5, with those obtained from the Cauchy and Gaussian 
#     dfs. In addition it plots the cumulated df for the alpha values 
#     1.001 and 1.999. The function implements J.H. McCulloch's Fortran 
#     program for symmetric distributions.
#     Note, that McCullochs approach has a density precision of 0.000066
#     and a distribution precision of 0.000022 for alpha in the range 
#     [0.84, 2.00]. We have added only first order tail approximation to 
#     calculate the tail density and probability. This has still to be 
#     improved! 
#   Part II: Explore Symmetric Stable Distributions. The example
#     investigate the limit behaviour of the "symstb" function.
#   Part III: #	Investigate the Tails of the Symmetric Stable Distribution
#     The example investigates the tail behaviour of the "symstb" function.
#   Part IV: Plot alpha stable density function for alpha values 0.5, 
#     0.75, 1.01, 1.25, 1.5 and beta=0.5. Note, that the function 
#     doesn't yet apply for some special x if they are integer multiples 
#     of alpha and/or beta. These cases have to be considered separately 
#     and to be implemented!
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


################################################################################
## Part I: Symmetric Stable Distribution

   # Settings:
   par(mfcol = c(3, 2), err = -1)

   # RSYMSTB(1.01) - Symmetric Stable Distribution - Close Cauchy: 
   x = seq(-7.5, 7.5, length = 256)
   r = rsymstb(4096, alpha = 1.01)
   plot(r, type = "l", main = "RSYMSTB(1.01) Series")
   d = dsymstb(x, alpha = 1.01)
   d.cauchy = dcauchy(x)
   plot(x, log(d), type="l", main="DSYMSTB(1.01) - Close Cauchy")
   density = density(r, from = -10, to = 10, n = 256)
   points(density$x, log(density$y), col = 4)
   lines(x, log(d.cauchy), col = 6)
   p = psymstb(x, alpha = 1.01)
   p.cauchy = pcauchy(x)
   plot(x, p, type="l", main="PSYMSTB(1.01) - Close Cauchy")
   points(sort(r), (1:length(r))/length(r), col = 4)
   lines(x, p.cauchy, col = 6)
    
   # RSYMSTB(1.99) - Symmetric Stable Distribution - Close Cauchy: 
   x = seq(-5, 5, length = 256)
   r = rsymstb(4096, alpha = 1.99)
   plot(r, type = "l", main = "RSYMSTB(1.99) Series")
   d = dsymstb(x, alpha = 1.99)
   d.norm = dnorm(x, sd = sqrt(2))
   plot(x, log(d), type = "l", main = "DSYMSTB(1.99) - Close Normal")
   density = density(r, from = -5, to = 5, n = 256)
   points(density$x, log(density$y), col = 4)
   lines(x, log(d.norm), col = 6)
   p = psymstb(x, alpha = 1.99)
   p.norm = pnorm(x, sd = sqrt(2))
   plot(x, p, type="l", main="PSYMSTB(1.99) - Close Normal")
   points(sort(r), (1:length(r))/length(r), col = 4)
   lines(x, p.norm, col = 6)
   

################################################################################   
## PART II: Limit Behaviour of the Symmetric Stable Distribution

   # Settings:
   par(mfrow = c(3, 2), err = -1)
    
   # Histogram of Symmetric Stable Deviates with alpha=1.001
   # Comparison with Cauchy Density
   # Figure 1 - Histogram Close to Cauchy / alpha=1.001
   x = rsymstb(n = 5000, alpha = 1.001)
   hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
     ylim = c(0.0,0.5), main = "Close Cauchy")
   x = seq(from = -10, to = 10, by = 0.10)
   lines(x, dcauchy(x))

   # Histogram of Symmetric Stable Deviates with alpha=1.999
   # Comparison with Gaussian Density, Note: sd=sqrt(2)!
   # Figure 2 - Histogram Close to Gaussian / alpha=1.999
   x = rsymstb(n = 5000, alpha = 1.999)
   hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
     ylim=c(0.0,0.5), main = "Close Gaussian")
   x = seq(from = -10, to = 10, by = 0.10)
   lines(x, dnorm(x, sd = sqrt(2)))

   # Histogram of Symmetric Stable Deviates with alpha=0.500
   # Comparison with Stable Density "dstable"
   # Figure 3 - Symmetric Stable Density / alpha=0.500
   x = rsymstb(n = 5000, alpha = 0.500)
   hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
     ylim = c(0.0, 0.6), main = "alpha=0.500")
   x = seq(from = -10, to = 10, by = 0.05)
   lines(x, dsymstb(x, alpha = 0.500))

   # Histogram of Symmetric Stable Deviates with alpha=1.500
   # Comparison with Stable Density "dsymstb"
   # Figure 4 - Symmetric Stable Density / alpha=1.500
   x = rsymstb(n = 5000, alpha = 1.500)
   hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
     ylim = c(0.0, 0.6), main = "alpha=1.500")
   x = seq(from=-10, to=10, by=0.25)
   lines(x, dsymstb(x, alpha=1.500))

   # CDF of Symmetric Stable Deviates with alpha=1.001
   # Comparison with Cauchy Probability Function
   # Figure 5 - CDF Close to Cauchy / alpha=1.001
   n = 500
   x = sort(rsymstb(n, alpha = 1.001))
   y = (1:n)/n
   plot(x, y, xlim = c(-120, 120), main = "Close Cauchy")
   x = seq(from = -100, to = 100, by = 1)
   lines(x, pcauchy(x), col = 5)
   lines(x, psymstb(x, alpha = 1.001), col = 4)

   # CDF of Symmetric Stable Deviates with alpha=1.999
   # Comparison with Gaussian Probability Function, Note: sd=sqrt(2)!
   # Figure 6 - CDF Close to Gaussian / alpha=1.999
   n = 500
   x = sort(rsymstb(n, alpha = 1.999))
   y = (1:n)/n
   plot(x, y, xlim = c(-10, 10), main = "Close Gaussian")
   x = seq(from = -10, to = 10, by = 0.25)
   lines(x, pnorm(x, sd = sqrt(2)), col = 4)
   lines(x, psymstb(x, alpha=1.999), col = 4)
 

################################################################################   
## PART III: 

   # Settings:
   par(mfrow = c(2, 2))
    
   # Show the precision of the DF in the Tail:
   # There remains somw further work to do ... 
   x = seq(-20, 0, 1)
   alpha = 1.9 # worst in the limit alpha -> 2.0
   plot(x, log(dsymstb(x, alpha)), type = "b", main = "dsymstb")
   plot(x, log(psymstb(x, alpha)), type = "b", main = "psymstb")
   x = seq(-12, -3, 0.25)
   plot(x, log(dsymstb(x, alpha)), type = "b", main = "dsymstb")
   plot(x, log(psymstb(x, alpha)), type = "b", main = "psymstb")


################################################################################
## PART IV: Skew Stable Distribution

   # Settings:
   par(mfcol = c(3, 3))

   # RSTABLE(1.1, 0.5) - Stable Distribution:    
   x = seq(from = -50, to = 50, length = 100) 
   r = rstable(4096, alpha = 0.4, beta = 0.5)
   plot(r, type = "l", main = "RSTABLE (0.4,0.5)")    
   d = dstable(x, alpha = 0.4, beta = 0.5)
   plot(x, log(d), type = "l", main = "DSTABLE (0.4,0.5)")
   density = density(r[abs(r)<10], n = 100)
   points(density$x, log(density$y), col = 4)    
   p = pstable(x, alpha=0.4, beta = 0.5)
   plot(x, p, type = "l", ylim = c(0, 1), main = "PSTABLE (0.4,0.5)")
   points(sort(r), (1:length(r))/length(r), col = 4)

   # RSTABLE(1.1, 0.5) - Stable Distribution:   
   x = seq(from = -10, to = 10, length = 100) 
   r = rstable(4096, alpha = 1.1, beta = 0.5, gamma = 1.5, delta = -1.0)
   plot(r, type = "l", main = "RSTABLE (1.1,0.5,1.5,-1.0)")    
   d = dstable(x, alpha = 1.1, beta = 0.5, gamma = 1.5, delta = -1.0)
   plot(x, log(d), type = "l", main = "DSTABLE (1.1,0.5,1.5,-1.0)")
   density = density(r[abs(r)<10], n = 100)
   points(density$x, log(density$y), col = 4)    
   p = pstable(x, alpha = 1.1, beta = 0.5, gamma = 1.5, delta = -1.0)
   plot(x, p, type = "l", ylim = c(0, 1), main = "PSTABLE (1.1,0.5,1.5,-1.0)")
   points(sort(r), (1:length(r))/length(r), col = 4)

   # RSTABLE(1.9, 0.5) - Stable Distribution: 
   x = seq(from = -5, to = 5, length = 100)   
   r = rstable(4096, alpha = 1.9, beta = 0.5)
   plot(r, type = "l", main = "RSTABLE (1.9,0.5)")    
   d = dstable(x, alpha = 1.9, beta = 0.5)
   plot(x, log(d), type = "l", main = "DSTABLE (1.9,0.5)")
   density = density(r, n = 200)
   points(density$x, log(density$y), col = 4)    
   p = pstable(x, alpha = 1.9, beta = 0.5)
   plot(x, p, type = "l", main = "PSTABLE (1.9,0.5)")
   points(sort(r), (1:length(r))/length(r), col = 4)

