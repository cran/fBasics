
#
# Example: 
#	Investigate dependencies in the NYSE Composite Index
#
# Description:
#	Investigate autocorrelations and partial autocorrelations, the 
#	Taylor effect and the long memory behaviour of the NYSE Composite 
#	Index. In detail we will cover the following topics:
#
#		1 ACF and Partial Autocorrelations
#		2 Taylor Effect
#		3 Long Memory Behaviour
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------



################################################################################
## 1 Autocorrelation and Partial Autocorrelations


	# This example plots the autocorrelation function and the 
	# partial autocorrelation function 

	# Settings:
	par(mfrow = c(2, 1))
	data(nyseres)

	# Autocorrelations and Partial Autocorrelations:
	# Figures 1/2: ACF and PACF of NYSE log Returns
	acf(nyseres)
	pacf(nyseres)

	
################################################################################
## 2 Taylor Effect


	# This example Investigates the Taylor effect of the  
	# standardized volatilities of the NYSE Composite Index 
	# and compare the results with simulated residuals derived 
	# from a t-distributed iid time series. 

	# Settings:
	par(mfrow = c(2, 1))
	data(nyseres)

	# Taylor Effect / NYSE Residuals:	
	# Figure 1 - NYSE Residuals
	teffectPlot(nyseres, lag.max=6, ymax=0.3, standardize=F)

	# Compare with t-distributed iid Time Series:
	# Figure 2 - t-distributed Random Variates
	teffectPlot(rt(length(x),4), lag.max=6, ymax=0.3)


################################################################################
##  3 Long Memory Behaviour


	# This example plots the long memory autocorrelation function 
	# of the volatility calculated from the NYSE residuals 
	# and estimate the Hurst-Exponent from the slope of the 
	# log-log plot. 

	# Settings:
	# Calculate Hurst Exponent and Plot Long Memory ACF
	# Data: NYSE Composite Index log Returns
	par(mfrow = c(3, 1))
	data(nyseres)
	
	# Long Memory Autocorrelation Function:	
	result = lmacfPlot(abs(nyseres), lag.max = 126, ci = 0.95, 
		main = "ACF NYSE-Index", doprint = TRUE)

	# Program Test - Try a time series which exhibits no long memory:
	# Data: Gaussian Random Numbers
	x = rnorm(n=dim(nyseres)[1])
	result = lmacfPlot(abs(x), lag.max = 126, ci = 0.95, 
		main = "ACF iid Gauss", doprint = TRUE)

		