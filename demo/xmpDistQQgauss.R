
#
# Example: 
#	Create a Quantile-Quantile Plot and Calculate Basic Statistics
#
# Description: 
#	This example creates a Quantile-Quantile Plot for the NYSE 
#  	residuals. It also calculated mean, variance, skewness and 
#  	kurtosis. 
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------
  

# Settings:

  	# Create a Quantile-Quantile Plot
	# Data: NYSE Composite Index log Returns
	par(mfrow = c(2, 1))
	data(nyseres)

# NYSE QQ Plot:

  	# Figure 1 - Empirical Data
	x <- nyseres[,1]
	result <- qqgaussPlot(x, main="QQ Plot")

# Simulate a Gaussian - QQ Plot:

  	# Figure 2 - Simulated Gaussian Data
	x.simulated <- sqrt(var(x))*rnorm(length(x)) + mean(x)
	result <- qqgaussPlot(x.simulated, main="QQ Plot")

# Also Calculate mean, variance, skewness and kurtosis:

  	mean(x)
  	var(x)
  	skewness(x)
	kurtosis(x)

	