
#
# Example: 
#	Performs Correlation Tests
# 
# Description: 
#	This example investigates correlations between yesterdays and
#  	todays volatilities of the NYSE stock market index. It compares 
#	the results with those obtained from a resampled time series.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	# Investigate Correlations in Time Series
	# Data: NYSE Composite Index log Returns
	data(nyseres)

# Correlation Tests:

	x = nyseres[6000:7000, 1]
	# Standardize
	x = (x-mean(x))/sqrt(var(x)) 
	lag = 1
	print(corTest(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative="two.sided", method="spearman"))
	print(corTest(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative="two.sided", method="kendall"))

# Compare with Resampled Series:	

	# Resampled Empirical Time Series
	x = sample(x)
	print(corTest(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative="two.sided", method="spearman"))
	print(corTest(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative="two.sided", method="kendall"))

		