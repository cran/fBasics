
#
# Example: 
#	Fit the Parameters of the Student-t Density by MLE
#   
# Description: 
#	This example fits the NYSE residuals to a Student-t, to a
#	hyperbolic, and to a normal inverse Gaussian distribution
#  	via maximum log-likelihood estimation and plot the result. 
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Load Data:
	data(nyseres)

	
# ------------------------------------------------------------------------------
# PART I: MLE Fit to Student-t Density:
	
	# NYSE - Fit the Parameters and Plot:
	# Data: NYSE Composite Index Returns\n")
	x = nyseres[,1]
	# standardize time series
	s = (x-mean(x))/sqrt(var(x))
	# Note, this may take some time:
	par(mfrow = c(3, 2), err = -1, cex = 0.5)
	options(warn = -1)
	fit = tFit(s, df = 4, doplot = TRUE, width = 0.5)
	fit

	# SIMULATED - Fit the Parameters and Plot:	
	# Data: Simulated Random Variates with df=4
	s = rt(length(s), df=8)
	# Note, this may take some time:
	fit = tFit(s, df=4, doplot=T, width=0.5)
	fit
	# Remark: The variance equals df/(df-2)	
	

# ------------------------------------------------------------------------------	
# PART II: MLE Fit to Hyperbolic Density
	
	# Fit the Parameters and Plot:
	# Data: NYSE Composite Index Returns
	x = nyseres[,1]
	# standardize time series
	s = (x-mean(x))/sqrt(var(x)) 
	# width - width of window for density plot
	fit = hypFit(x = s, alpha = 1, beta = 0, delta = 1, mu = 0, 
		doplot = TRUE, width = 0.5)
	fit

# Fit the Parameters and Plot:

	# Data: Simulated Random Variates HYP(1,0,1,0)
	s = rhyp(length(x), 1, 0, 1, 0) 
	# Note, this may take some time:
	fit = hypFit(s, alpha = 1, beta = 0, delta = 1, mu = 0, 
		doplot = TRUE, width = 0.5)
	fit	
	

# ------------------------------------------------------------------------------	
# PART III: MLE Fit to Normal Inverse Gaussian Density
	
	# Fit the Parameters and Plot:
	# Data: NYSE Composite Index Returns
	x = nyseres[,1]
	# standardize time series
	s = (x-mean(x))/sqrt(var(x))
	# Note, this may take some time:
	fit = nigFit(s, alpha = 1, beta = 0, delta = 1, mu = 0, 
		doplot = TRUE, width = 0.5)
	fit

	# Fit the Parameters and Plot:
	# Data: Simulated Random Variates NIG(1,0,1,0) \n")
	s = rnig(length(x), 1, 0, 1, 0) 
	# Note, this may take some time:
	fit = nigFit(s, alpha = 1, beta = 0, delta = 1, mu = 0, 
		doplot = TRUE, width = 0.5)
	fit

