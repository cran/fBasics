
#
# Example: 
#	Explore the Normal Inverse Gaussian Distributions
#
# Description: 
#	Plot the density for a normal inverse Gaussian 
#  	distribution with alpha=1 and beta=0 in the range 
#  	between -4 and 4 in steps of 0.1.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:
	
	par(mfcol = c(3, 2))

	
# NIG(1, 0, 1, 0) - Hyperbolic Distribution:	
	
	x = seq(-5, 5, length = 256)

	r = rnig(5000, alpha = 1, beta = 0, delta = 1, mu = 0)
	plot(r, type = "l", main = "RNIG(1, 0, 1, 0) Series")
	
	d = dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0)
	plot(x, log(d), type="l", main="DNIG(1, 0, 1, 0)")
	density = density(r, from = -5, to = 5, n = 128)
	points(density$x, log(density$y), col = "steelblue4")
	
	p = pnig(x, alpha=1, beta=0, delta=1, mu=0)
	plot(x, p, type = "l", main = "PNIG(1, 0, 1, 0)")
	points(sort(r), (1:length(r))/length(r), col = "steelblue4")
	
	
# NIG(1, 0.3, 1, 1) - Hyperbolic Distribution:	
	
	x = seq(-5, 5, length = 256)

	r = rnig(5000, alpha = 1, beta = 0.3, delta = 1, mu = 1)
	plot(r, type = "l", main = "RNIG(1, 0.3, 1, 1) Series")
	
	d = dnig(x, alpha = 1, beta = 0.3, delta = 1, mu = 1)
	plot(x, log(d), type = "l", main = "DNIG(1, 0.3, 1, 1)")
	density = density(r, from = -5, to = 5, n = 128)
	points(density$x, log(density$y), col = "steelblue4")
	
	p = pnig(x, alpha = 1, beta = 0.3, delta = 1, mu = 1)
	plot(x, p, type = "l", main = "PNIG(1, 0.3, 1, 1)")
	points(sort(r), (1:length(r))/length(r), col = "steelblue4")

	
# ------------------------------------------------------------------------------

