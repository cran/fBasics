
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
	
	par(mfcol = c(3, 2), cex = 0.7)

	
# GH(1, 0, 1, 0, 1) - Generalized Hyperbolic Distribution:	
	
	x = seq(-5, 5, length = 256)

	r = rgh(5000, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
	plot(r, type = "l", main = "RGH(1, 0, 1, 0, 1) Series", col = "steelblue4")
	grid()
	
	d = dgh(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
	plot(x, log(d), type = "l", main = "DGH(1, 0, 1, 0, 1)")
	density = density(r, from = -5, to = 5, n = 128)
	points(density$x, log(density$y), col = "steelblue4")
	grid()
	
	p = pgh(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
	plot(x, p, type = "l", main = "PGH(1, 0, 1, 0, 1)")
	points(sort(r), (1:length(r))/length(r), col = "steelblue4")
	grid()
	
	
# GH(1, 0.3, 1, 1, 1) - Generalized Hyperbolic Distribution:	
	
	x = seq(-3, 7, length = 256)

	r = rgh(5000, alpha = 1, beta = 0.3, delta = 1, mu = 1, lambda = 1)
	plot(r, type = "l", main = "RNIG(1, 0.3, 1, 1) Series", col = "steelblue4")
	grid()
	
	d = dgh(x, alpha = 1, beta = 0.3, delta = 1, mu = 1, lambda = 1)
	plot(x, log(d), type = "l", main = "DGH(1, 0.3, 1, 1)")
	density = density(r, from = -5, to = 5, n = 128)
	points(density$x, log(density$y), col = "steelblue4")
	grid()
	
	p = pgh(x, alpha = 1, beta = 0.3, delta = 1, mu = 1, lambda = 1)
	plot(x, p, type = "l", main = "PGH(1, 0.3, 1, 1, 1)")
	points(sort(r), (1:length(r))/length(r), col = "steelblue4")
	grid()

	
# ------------------------------------------------------------------------------

