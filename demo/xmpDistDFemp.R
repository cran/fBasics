
# 
# Example:  
#	 Compute and plot an empirical cumulative distribution function
#    and write functions to compute a smoothed estimate.
#
# Description:
#   PART I: Compute and plot an empirical cumulative distribution function
#     Hint, use "ecdf" function from "stats" package
#   PART II: ssd
#	  Estimate probability densities using smoothing spline ANOVA 
#	  models with cubic spline, linear spline, or thin-plate spline 
#	  marginals for numerical variables, "ssdFit" Furthermore, write 
#	  simple function wrappers "dssd", "pssd", "qssd" and "rssd" for 
#	  the probability density, probabilities, quantiles and random
#	  variates. Hint: use the functions from R's "gss" Package. 
#     Hint, use "ssden" functions from "gss" package.
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


################################################################################
## PART I: Compute and plot an empirical cumulative distribution function


   # Requirements:
   # require(stats)
   # ... is preloaded
   ###

   
   # Simple didactical  ecdf  example:
   Fn <- ecdf(rnorm(12))
   print(Fn)
   summary(Fn)
   ###
   
   
   # == 1:12  if and only if there are no ties !
   12 * Fn(knots(Fn)) == 1:12 
   y <- round(rnorm(12), 1); y[3] <- y[1]
   Fn12 <- ecdf(y)
   Fn12
   print(knots(Fn12), dig = 2)
   ###
   
   
   # ~= 1:12  if there where no ties
   12 * Fn12(knots(Fn12))  
   summary(Fn12)
   summary.stepfun(Fn12)
   print(ls.Fn12 <- ls(env = environment(Fn12)))
   ###
   
   
   #[1] "f"  "method"  "n"  "ties"   "x"  "y"  "yleft"  "yright"
   12 * Fn12((-20:20)/10)
   ###

   
   # Plotting:
   op <- par(mfrow = c(3, 2), mgp = c(1.5, 0.8, 0), 
     mar = 0.1 + c(3, 3, 2, 1))
   F10 <- ecdf(rnorm(10))
   summary(F10)
   plot(F10)
   plot(F10, verticals= TRUE, do.p = FALSE)
   plot(Fn12)  
   xx <- unique(sort(c(seq(-3, 2, length = 201), knots(Fn12))))
   lines(xx, Fn12(xx), col = 'blue')
   abline(v = knots(Fn12), lty = 2,col = 'gray70')
   plot(xx, Fn12(xx), type='b', cex=.1)#- plot.default
   plot(Fn12, col.h='red', add= TRUE)  #- plot method
   abline(v=knots(Fn12),lty=2,col='gray70')
   plot(Fn12, verticals=TRUE, col.p='blue', col.h='red',col.v='bisque')
   ###
   
   
   # This works too (automatic call to  ecdf(.)):
   plot.ecdf(rnorm(24))
   par(op)
   ###

   
################################################################################
## PART II: SSDEN


   # Functions: 
   dssd = function(x, object) {	
	 # Evaluate density for smoothing spline density estimates. 
	 dssden(object = object, x = x) }	 
   pssd = function(q, object) {	
	 # Evaluate probability for smoothing spline density estimates. 
	 pssden(object = object, q = q) }
   qssd = function(p, object) {
	 # Evaluate quantiles for smoothing spline density estimates. 	
	 qssden(object = object, p = p) }
   rssd = function(n, object) {   
	 # Generate random deviates for smoothing spline density estimates. 
	 qssden(object = object, p = runif(n)) }
   ssdFit = function(x, ...) {
	 # Estimate probability densities using smoothing spline ANOVA 
	 # models with cubic spline, linear spline, or thin-plate spline 
	 #marginals for numerical variables. x is a numeric vector.	
	 ssden(~x, ...)	}
   ###
   

   # Requirements:
   require(gss)
   ###
	
   
   # Fit: Estimate from Empirical Distribution  
   data(nyseres)
   par(mfcol = c(3, 2), err = -1)
   # Take subset at random ...
   nyseres = sample(nyseres[, 1])[1:1000]
   e = (nyseres - mean(nyseres))/sqrt(var(nyseres))
   # This will take some time ...
   fit = ssdFit(e)
   ###
	
   
   # SSD - Empirical Distribution:	
   x = seq(-6, 6, length = 256)
   r = rssd(100, fit)
   plot(r, type = "l", main = "SSD Series")
   mean(r); var(r)	
   d = dssd(x, fit)
   plot(x, log(d), type = "l", main = "SSD Density")
   density = density(r, from = -6, to = 6, n = 128)
   points(density$x, log(density$y), col = 4)	
   p = pssd(x, fit)
   plot(x, p, type = "l", main = "SSD Probability")
   points(sort(r), (1:length(r))/length(r), col = 4)
   ###
	

################################################################################

