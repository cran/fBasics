
#
# Example: 
#	Compare Subsamples of Time Series by the KS Test
#
# Description: 
#  	This example cuts the NYSE log-Return time series into 4 parts and 
#  	compare the distributons of the individual time series by applying 
#  	the two-sample Kolmogoroff-Smirnov Test.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

  	# Cut a log-Return Series into Parts and Compare the Distributions
	# Data: NYSE Composite Index log Returns
	options(digits = 5)
	data(nyseres)

# Data Manipulations:

	x = unique(nyseres[, 1])
	x = matrix(x[1:(4*trunc(length(x)/4))], ncol = 4)
	x.resampled = matrix(sample(x)[1:(4*trunc(length(x)/4))], 
		ncol = 4) # resampled
	NumberOfSubsets = 4

# Write a "compare" function:

	compare = function(x, n) {  
		k = 0
		i = j = statistic = p.value = rep(NA,time=n*(n-1)/2)
  		for ( ii in 1:(n-1) ) {  
    		for ( jj in (ii+1):n ) { 
			k = k + 1 
        		ksgof = ksTest(x=x[,ii], y=x[,jj])
        		i[k] = ii
			j[k] = jj
			statistic[k] = ksgof$statistic
			p.value[k] = ksgof$p.value }}
   		cbind.data.frame(i, j, statistic, p.value)
		}
   
# Compare subsets of the NYSE time series:

	compare(x = x, NumberOfSubsets)
	
# Compare subsets of the resampled NYSE time series:

  	compare(x = x.resampled, NumberOfSubsets)
  	
  	