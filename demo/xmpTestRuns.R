
#
# Example: 
#	Perform a Runs Test on Time Series Data
#
# Description: 
#	This example shows how to perform a Runs Test using Traplettis
#  	function implemented from his 'tseries' R-package.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:	

	# Perform a Runs Test
	# Data: NYSE Composite Index log Returns
	data(nyseres)
	
# Runs Test:

	runsTest(nyseres[, 1])
	
# Re-sampled NYSE log-Returns:

	runsTest(sample(nyseres[, 1]))

	