
#
# Example: 
#	Explore the Hyperbolic Distributions
#   
# Description: 
#	This example creates Random Variates and Plot the density
#   for a hyperbolic distribution with alpha=1 and beta=0 
#   and 0.3 delta=1 and mu=0 and 0.1 in the range 
#   between -4 and 4 in steps of 0.1.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:
	
	par(mfcol = c(3, 2), err = -1)
  	
	
# HYP(1, 0, 1, 0) - Hyperbolic Distribution:	
	
	x = seq(-6, 6, length = 256)

	r = rhyp(4096, alpha = 1, beta = 0, delta = 1, mu = 0)
	plot(r, type = "l", main = "RHYP(1, 0, 1, 0) Series")
	mean(r); var(r)
	
	d = dhyp(x, alpha = 1, beta = 0, delta = 1, mu = 0)
	plot(x, log(d), type = "l", main = "DHYP(1, 0, 1, 0)")
	density = density(r, from = -6, to = 6, n = 128)
	points(density$x, log(density$y), col = 4)
	
	p = phyp(x, alpha = 1, beta = 0, delta = 1, mu = 0)
	plot(x, p, type = "l", main = "PHYP(1, 0, 1, 0)")
	points(sort(r), (1:length(r))/length(r), col = 4)
	
	
# HYP(1, 0.3, 1, 0.0) - Hyperbolic Distribution:	
	
	x = seq(-7, 15, length = 256)
	
	r = rhyp(4096, alpha = 1, beta = 0.3, delta = 2, mu = 1)
	plot(r, type = "l", main = "RHYP(1, 0.3, 2, 1) Series")
	mean(r); var(r)

	d = dhyp(x, alpha = 1, beta = 0.3, delta = 2, mu = 1)
	plot(x, log(d), type="l", main="DHYP(1, 0.3, 2, 1)")
	density = density(r, from = -7, to = 15, n = 128)
	points(density$x, log(density$y), col = 4)

	p = phyp(x, alpha = 1, beta = 0.3, delta = 2, mu = 1)
	plot(x, p, ylim = c(0,1), type = "l", main = "PHYP(1, 0.3, 2, 1)")
	points(sort(r), (1:length(r))/length(r), col = 4)

