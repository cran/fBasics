
#
# Example: 
#   Compute Basic Statistics of Time Series Data, write a bootstrapped
#   mean function, explore aggregation effection with the central
#   limit theory, and plot and fit distributions on logarithmic scales.   
#   
# Description: 
#   Part I: Calculate skewness, kurtosis and further basic statistical 
#     values of the returns of the NYSE Composite Index using the functions 
#     "skewness", "kurtosis", and "basicStats".
#   Part II: Write a function to compute the bootstrapped mean value.
#     Use the bootstrap function from the "Hmisc" package.
#   Part III: Explore Aggregation Effects with the Central Limit Theorem
#	  This example creates a Quantile-Quantile Plot for the NYSE Composite 
#     Index residuals. Calculate mean, variance, skewness and kurtosis. 
#     It also tests the program for a set of iid residuals of equal size 
#     derived from a Gaussian distribution, compared the data with those 
#     obtained from an aggregated time series as a result of the Central 
#     Limit Theorem.
#   Part IV: Plot and fit distributions on logarithmic scales.
#	  This example plots the NYSE residuals on a lin-log and on a log-log 
#  	  scale. It compares the results with iid Gaussian residuals with 
#  	  the same mean and variance and equal length.
#	Part V: Plot and fit distributions on double logarithmic scales.
#     This example plots the PDF of the NYSE residuals on a double 
#  	  log-scale. It fits the tail and calculates the intercept and slope. 
#  	  In addition itompares the result to simulated residuals obtained 
#	  from an alpha-stable process.
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


################################################################################
## Part I: Calculate Basic Statistics:

   # Settings:
   data(nyseres)
    
   # Skewness / Kurtosis:
   print(skewness(nyseres))
   print(kurtosis(nyseres))

   # Basic Statistics:
   print(basicStats(nyseres))
   
   
################################################################################
## Part II: Bootstrapped Mean:

   bootMean =
   function(x, B = 1000, ci = 0.95, na.rm = TRUE, reps = FALSE)
   {    # A function implemented by Diethelm Wuertz
		# Description:
        #   A very fast implementation of the basic nonparametric 
        #   bootstrap for obtaining confidence limits for the population 
        #   mean without assuming normality.       
        # Arguments:
        #   B - number of bootstrap resamples, by default 1000.
        #   ci - specifies the confidence level (0-1) for interval 
        #       estimation of the population mean. 
        #   na.rm - a logical flag, should NAs be removed?
        #   reps - set to TRUE to have bootMean return the vector 
        #       of bootstrapped means as the reps attribute of 
        #       the returned object .
        # Notes:
        #   The function calls "smean.cl.boot" from the "Hisc" package
        #   Requirements: require(Hmisc)       
        # FUNCTION:       
        # Requirements:
        sink("@sink@") # Don't print loading ...
        library(Design, warn.conflicts = FALSE)
        library(Hmisc, warn.conflicts = FALSE)
        sink()
        unlink("@sink@")        
        # Return Value:
        smean.cl.boot(x = x, conf.int = ci, B = B, na.rm = na.rm, reps = reps)}
    
    # Bootstrapped Mean:
    print(bootMean(nyseres))


################################################################################
## Part III: Explore aggregation effection with CLT


   # Settings:
   options(warn = -1)
   par(mfrow = c(3, 3), cex = 0.6, err=-1)
   data(nyseres)

   # CLT Plot:     
   set.seed(11)
   for (k in 1:2) {
     if (k == 4) par(ask = TRUE) 
     le = 8390 # length of nyseres
     if (k == 1) {r = rt(le, df = 6); title = "Student-t df=6"}
     if (k == 2) {r = nyseres[,1]; title = "NYSE Residuals"}
     if (k == 3) {r = rexp(le); title = "Exponential"}
     # How look the plots when the 2nd moment doesn't exist ?
     if (k == 4) {r = rstable(le, alpha = 1.75, beta = 0)
       title = "Stable alpha=1.75"}
     if (k == 5) {r = runif(le); title = "Uniform"}
     if (k == 6) {r = rnorm(le); title = "Normal"}
     # Normalize:
     r = (r - mean(r))/sqrt(var(r))
     cat("\n", title, "\n")
     cat(" Length of r: ", le, "\n")
     n = 2
       x = apply(matrix(r, ncol = n), MARGIN = 1, FUN = sum)/sqrt(n)
       logpdfPlot(x[abs(x) < 5], n = 64, main=title, xlim=c(-5,5), 
         ylim = c(-7, 0), xlab = "x", ylab = "Density")
       cat(" Skewness: ",skewness(x), "    Kurtosis: ",kurtosis(x), "\n")
     n = 10
       x = apply(matrix(r, ncol = n), MARGIN = 1, FUN = sum)/sqrt(n)
       logpdfPlot(x[abs(x) < 5], n = 64, xlim=c(-5, 5), ylim=c(-7,0),
         xlab="x",  ylab="Density")
       cat(" Skewness: ",skewness(x), "    Kurtosis: ",kurtosis(x), "\n")
     n = 2
       x = apply(matrix(r, ncol = n), MARGIN = 1, FUN = sum)/sqrt(n)
       qqgaussPlot(x, col = 1, ylab = "Empirical Data")
     n = 10
       x = apply(matrix(r, ncol = n), MARGIN = 1, FUN = sum)/sqrt(n)
       points(qqnorm(x[abs(x) < 5], plot = FALSE), col = 6, pch = 3)
     if (k == 4) par(ask = FALSE)}
    

################################################################################
## Part IV:

   # Settings:
   cat("\nPlot Distributions on Logarithmic Scales")
   cat("\nData: NYSE Composite Index log Returns \n") 
   par(mfrow = c(2, 2), err = -1)
   data(nyseres)
    
   # Plot the log-PDF:
   cat("\n Figure 1 - log PDF NYSE Plot")
   x = nyseres[,1]
   result = logpdfPlot(x, n = 501, 
     xlab = "log Return", ylab = "log Empirical PDF",
     xlim = c(-0.2, 0.2), main = "log PDF NYSE Plot")

   # Simulate a Gaussian with same mean and variance:
   cat("\n Figure 2 - log PDF Gaussian Plot")
   result = logpdfPlot(sqrt(var(x))*rnorm(length(x))+mean(x), n = 251, 
     xlab = "log Return", ylab = "log Gaussian PDF",
     xlim = c(-0.2, 0.2), main = "log PDF Gaussian Plot")
        
   # Plot the log-log-PDF:
   cat("\n Figure 3 - log-log PDF NYSE Plot")
   result = logpdfPlot(x, n=501, type="log-log",
     xlab = "log Return", ylab = "log Empirical PDF",
     xlim = c(-7, -1), main = "log-log PDF NYSE Plot")

   # Simulate a Gaussian with same mean and variance:
   cat("\n Figure 4 - log-log PDF Gaussian Plot \n")
   result = logpdfPlot(
     sqrt(var(x))*rnorm(length(x))+mean(x), n = 251, type = "log-log", 
     xlab = "log Return", ylab = "log Gaussian PDF",
     xlim = c(-7, -1), main = "log-log PDF Gaussian Plot")
        

################################################################################
## Part V:    

   # Settings:
   cat("\nFit the Tails in a log-log PDF Plot")
   cat("\nData: NYSE Composite Index log Returns \n") 
   par(mfrow = c(1, 1))
   data(nyseres)
    
   # Plot the log-log-PDF:
   x = nyseres[,1]
   result = logpdfPlot(x, n = 501, type = "log-log",
     xlab="log |Return|", ylab = "log PDF",
     xlim=c(-7, -1), ylim = c(-6, 5),
     main="log-log PDF NYSE Plot")
            
   # Combined Tail Fit:
   y1 = log(result$counts)
   x1 = log(result$breaks)
   y1 = y1[x1 > (-4.0) & x1 < (-3.0)]
   x1 = x1[x1 > (-4.0) & x1 < (-3.0)]
   l = lsfit(x1, y1)
   abline(l$coefficients)

   # Compare with alpha-Stable Distribution:
   print("Compare with alpha-Stable Distribution")
   alpha = 1.7
   sd = sqrt(var(x)/2)
   x3 = result$breaks
   y3 = dstable(x3/sd, alpha, beta = 0)/sd
   points(log(x3), log(y3), pch = 8, col = 4)
   lines(log(x3), log(y3), col = 4)

   # Print Intercept/Slope from Tail Fit:
   print("Fittet Parameters Intercept and Slope")
   print(l$coefficients)


################################################################################

    