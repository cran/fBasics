
#
# Example: 
#	Investige Student's-t Distribution in the Gaussian Limit
#
# Description:
#	This example investigates by the Kolmogrov-Smirnov Test
#  	how random variables from Student's t distribution approach 
#	the Gaussian distribution with increasing degree of freedom df.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Set Parmaters:

	# How approach t-distributed random variables the Gaussian limit?"
	# Data: Sequence of 1000 t-distributed Random Variates
  	options(warn = -1)
  	par(mfrow = c(3, 2))

# Execute Tests:

	x = seq(-4, 4, length = 1000)
   	DegreesOfFreedom = c(2, 4, 8, 16, 32, 64)
	statistic = p.value = c(0, 0, 0, 0, 0, 0)
  	for (i in 1:length(DegreesOfFreedom)) { 
	 	plot(dnorm(x), type = "l")
    		lines(dt(x, df = DegreesOfFreedom[i]), col = 6)
    		result = ksTest(x = rt(10000, df = DegreesOfFreedom[i]), 
    			y = "pnorm", 0, 1, alternative = "two.sided")
		statistic[i] = result$statistic
		p.value[i] = result$p.value}	
	cbind.data.frame(DegreesOfFreedom, statistic, p.value)

	