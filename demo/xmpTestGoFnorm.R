
#
# Example: 
#	Perform Goodness-Of-Fit Tests for Testing Normality
#   
# Description: 
#	Test random deviates if they ar normally
#  	distributed. This testsuite includes the foloowing 
#  	test procedures:
#	  01  Omnibus Moments Test for Normality
#	  02  Geary's Test of Normality
#	  03  Studentized Range for Testing Normality
#	  04  D'Agostino's D-Statistic Test of Normality
#	  05  Kuiper V-Statistic Modified to Test Normality
#	  06  Watson U^2-Statistic Modified to Test Normality
#	  07  Durbin's Exact Test (Normal Distribution
#	  08  Anderson-Darling Statistic Modified to Test Normality
#	  09  Cramer-Von Mises W^2-Statistic to Test Normality
#	  10  Kolmogorov-Smirnov D-Statistic to Test Normality 
#	  11  Kolmogorov-Smirnov D-Statistic (Lilliefors Critical Values)
#	  12  Chi-Square Test of Normality (Equal Probability Classes)
#	  13  Shapiro-Francia W-Test of Normality for Large Samples
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	# Goodness-Of-Fit Suite for Testing Normality
	data(nyseres)

# GoF Testing financial market log-returns:

	x = nyseres[4001:5000, 1]
	x = (x-mean(x))/sqrt(var(x))
  	r = gofnorm(x, doprint = TRUE)

# GoF Testing normal innovations:

  	x = rnorm(length(x))
  	r = gofnorm(x, doprint = TRUE)
  
# GoF Testing t-distributed innovations:

	x = rt(length(x), df = 2)
  	r = gofnorm(x, doprint = TRUE)

  	