
#
# Example: 
#	Explore Scaling Law Behavior of a Time Series
#   
# Description: 
#	This example plots the scaling laws for the NYSE, alpha-
#  	stable and Gaussian distributed residuals. It checks the 
#  	slope and intercept obtained from the straight line fit.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	# Create a Scaling Law Plot
	# Data: NYSE Composite Index log Returns
	par(mfrow = c(2, 2))
	data(nyseres)

# Scaling Law Plot:

  	x = nyseres[,1]
  	print(scalinglawPlot(x, span = 6, doplot = TRUE))

# Compare with alpha-Stable Distribution:

	print(scalinglawPlot(x = rstable(length(x), alpha = 1.85, 
		beta = 0), doplot = TRUE))

# Compare with Gaussian Distribution:

	print(scalinglawPlot(x=rnorm(length(x)), doplot=TRUE))

	