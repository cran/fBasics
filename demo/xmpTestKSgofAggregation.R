
#
# Example: 
#	Investige NYSE Residuals by Aggregation in the Gaussian Limit
#
# Description:
#  	This example investigated by calculation of the value of the kurtosis 
#  	and the KS statistic together with its p.value how the distribution 
#  	functions of the NYSE residuals converge to a normal distribution 
#  	by aggregating the original times series in powers of two.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	# Investigate the Convergence to a Gaussian by Aggregation
	# Data: NYSE Composite Index log Returns
	data(nyseres)
	
# Aggregate in powers of two:

	x = nyseres[, 1]
	AggregationLevels = 8
	x = x[1:2^13]
	# Further Settings:
		x.length = x.kurtosis = x.statistic = x.pvalue = rep(NA, times=8)	
   		statistic.gof = function(x, ...) ksgof.test(x, ...)$statistic	
		pvalue.gof = function(x, ...) ksgof.test(x, ...)$p.value	
	# Investigate for all data subsets:		
		for ( i in 1:AggregationLevels){ 
			cat("  ",i)
		  	ncol = 2^(i-1)
			x.length[i] = length(x)/ncol
			if(i==1) 
				x.aggregated = x
			else
				x.aggregated = apply(matrix(x, byrow=T, ncol=ncol), 
					MARGIN=1, FUN=sum)
			# Remove ties:
			x.aggregated = unique(x.aggregated)
			x.kurtosis[i] = kurtosis(x.aggregated)
			x.mean = mean(x.aggregated)
			x.sdev = sqrt(var(x.aggregated))
			ksgof = ksTest(x=x.aggregated, y="pnorm", x.mean, 
				x.sdev, alternative="two.sided")
			x.statistic[i] = ksgof$statistic
			x.pvalue[i] = ksgof$p.value 
			}
	# Output as data.frame:
		cbind.data.frame(x.length, x.kurtosis, x.statistic, x.pvalue)

		