
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   R: see R's copyright and license file
#   date: Terry Therneau <therneau@mayo.edu>
#     R port by Th. Lumley <thomas@biostat.washington.edu>  K. Halvorsen 
#       <khal@alumni.uv.es>, and Kurt Hornik <Kurt.Hornik@R-project.org>
#   ts: Collected by Brian Ripley. See SOURCES
#   tseries: Compiled by Adrian Trapletti <a.trapletti@bluewin.ch>
# for ical:
#   libical: Libical is an Open Source implementation of the IETF's 
#	  iCalendar Calendaring and Scheduling protocols. (RFC 2445, 2446, 
#     and 2447). It parses iCal components and provides a C API for 
#     manipulating the component properties, parameters, and subcomponents.
#   Olsen's VTIMEZONE: These data files are released under the GNU 
#	  General Public License, in keeping with the license options of 
#     libical. 
# for the holiday database:
#   holiday information collected from the internet and governmental 
#	sources obtained from a few dozens of websites


########################|#######################################################
# FUNCTION:		   		DESCRIPTION:
#  dsymstb	   			 Returns density for symmetric stable DF
#   psymstb	   			  Returns probabilities for symmetric stable DF
#   qsymstb	   			  Returns quantiles for symmetric stable DF
#   rsymstb	   			  Returns random variates for symmetric stable DF
#  dstable	   			 Returns density for stable DF
#   pstable	   			  Returns probabilities for stable DF
#   qstable	   			  Returns quantiles for stable DF
#   rstable	   			  Returns random variates for stable DF

################################################################################


dsymstb = 
function (x, alpha)
{	# # A function implemented by Diethelm Wuertz
  
	# Description:
  	#	Return symmetric alpha-stable pdf
  	
  	# Notes: 
  	#	symstb:
  	#   Return symmetric alpha-stable pdf/cdf. The function implements 
  	#	J.H. McCulloch's Fortran program for symmetric distributions.
	#	Mc Cullochs approach has a density precision of 0.000066
	#	and a distribution precision of 0.000022 for alpha in the 
	#	range [0.84, 2.00]. We have added only first order tail 
	#	approximation to calculate the tail density and probability.
	#	This has still to be improved!

	# FUNCTION:
	
	# Return Value:
	.Fortran("symstb",
	  	as.double(x),
	  	as.double(1:length(x)),
	  	as.double(1:length(x)),
	  	as.integer(length(x)),
	  	as.double(alpha),
	  	PACKAGE = "fBasics")[[3]]
}


# ------------------------------------------------------------------------------


psymstb = 
function (q, alpha)
{	# A function implemented by Diethelm Wuertz

  	# Description:
  	#	Return symmetric alpha-stable cdf

  	# Notes: 
  	#	symstb:
  	#   Return symmetric alpha-stable pdf/cdf. The function
	#	implements J.H. McCulloch's Fortran program for symmetric
	#	distributions.
	#	Mc Cullochs approach has a density precision of 0.000066
	#	and a distribution precision of 0.000022 for alpha in the 
	#	range [0.84, 2.00]. We have added only first order tail 
	#	approximation to calculate the tail density and probability.
	#	This has still to be improved!
	
	# FUNCTION:
	
	# Return Value:
	.Fortran("symstb",
	  	as.double(q),
	  	as.double(1:length(q)),
	  	as.double(1:length(q)),
	  	as.integer(length(q)),
	  	as.double(alpha),
	  	PACKAGE = "fBasics")[[2]]
}


# ------------------------------------------------------------------------------


qsymstb = 
function(p, alpha)
{	# A function implemented by Diethelm Wuertz

	# FUNCTION:
	
	# Settings:
	maxiter = 1000
	
	# Parameter Check:
	if (alpha > +2)  stop("Error: alpha greater than 2")
	if (alpha <= 0)	 stop("Error: alpha less or equal 0")
	# Special Cases:
	if (alpha == 2) result = qnorm(p = p, mean = 0, sd = sqrt(2))
	if (alpha == 1) result = qcauchy(p = p) 
	
	# Continue:
	if (alpha != 1 && alpha != 2) {
	maxiter <<- maxiter
	myUniroot <-
		function (f, interval, lower = min(interval), upper = max(interval), 
		maxiter, ...) {
			if (f(lower, ...) * f(upper, ...) >= 0) {
				result = NA}
			else {
				result = .Internal(zeroin(function(arg) f(arg, ...), 
				lower, upper, tol = .Machine$double.eps, as.integer(maxiter)))[1]}
			result}
		
	froot = 
		function(x, alpha, p) {psymstb(q = x, alpha = alpha) - p }
	
	# Calculate:	
	result = rep(NA, times=length(p))
	for (i in 1:length(p)) {
		pp = p[i]
		# xmin = -(1-pp)/pp
		if (pp < 0.5) xmin = qcauchy(pp)
		else xmin = qnorm(pp, mean=0, sd=sqrt(2))
		# xmax = pp/(1-pp) 
		if (pp < 0.5) xmax = qnorm(pp, mean=0, sd=sqrt(2))
		else xmax = qcauchy(pp)			
		iteration = NA
		counter = 0
		while (is.na(iteration)) {
			iteration = myUniroot(f = froot, c(xmin, xmax), alpha = alpha, 
				maxiter = maxiter, p = pp)
			counter = counter + 1
			xmin = xmin - 2^counter
			xmax = xmax + 2^counter}
		result[i] = iteration } }
	
	# Return Value:
	result
}


# ------------------------------------------------------------------------------


rsymstb = 
function(n, alpha) 
{ 	# A function implemented by Diethelm Wuertz
	
  	# Description:
  	#	Return random deviates from the stable family 
	#	of probability distributions. The results of 
	#	Chambers, Mallows, and Stuck is used.

	# FUNCTION:
	
	# Calculate uniform and exponential distributed random numbers:
    theta = pi * (runif(n)-1/2)
    w = -log(runif(n))
	# Calculate Random Deviates:
    if (alpha == 1){
		result = rcauchy(n) }        
    else { 
		result = (sin(alpha*theta) / ((cos(theta))^(1/alpha))) *
			(cos((1-alpha)*theta)/w)^((1-alpha)/alpha)}	
	
	# Return Value:
    result
}


# ******************************************************************************


dstable = 
function(x, alpha, beta, gamma = 1, delta = 0, pm = 0)
{	# A function implemented by Diethelm Wuertz
	
	# Description:
  	#	Return alpha-stable density function (pdf) in form
  	#	of parmeterization 1. 
	#	The function uses the approach of J.P. Nolan for general 
	#	stable distributions. Nolan derived expressions in form 
	#	of integrals based on the charcteristic function for
	#	standardized stable random variables. These integrals
	#	can be numerically evaluated. 
	 
	# Arguments:
	#	alpha = index of stability, in the range (0,2]
  	#	beta  = skewness, in the range [-1, 1]
   	#	gamma = scale, in the range (0, infinity)
  	#	delta = location, in the range (-infinity, +infinity)
	#	param = type pf parmeterization:
	#		0. "S0" parameterization: based on the (M) representation
   	#		of Zolotarev for an alpha stable distribution with skewness
   	#		beta. Unlike the Zolotarev (M) parameterization, gamma and 
 	#		delta are straightforward scale and shift parameters. This
   	#		representation is continuous in all 4 parameters, and gives 
   	#		an intuitive meaning to gamma and delta that is lacking in 
  	#		other parameterizations.
 	#		1. "S" or "S1" parameterization: the parameterization used 
	#		by Samorodnitsky and Taqqu in the book Stable Non-Gaussian 
	#		Random Processes. It is a slight modification of Zolotarev's 
	#		(A) parameterization.
 	#		2. "S*" or "S2" parameterization: a modification of the S0 
	#		parameterization which is defined so that 
   	#		- the scale gamma agrees with the Gaussian scale (standard 
	#		  dev.) when alpha=2 and the Cauchy scale when alpha=1.
  	#		- the mode is exactly at delta.
 	#		3. "S3" parameterization: an internal parameterization.
   	#		The scale is the same as the S2 parameterization, the shift 
	#		is -beta*g(alpha), where g(alpha) is defined in the paper 
	#		below.
	#		Note, that up to now, only parameterization 0 is supported!
  	
  	# Notes: 
	#	The function doesn't apply for x[i] == 1, 
	#	this has to be fixed!

	# FUNCTION:
	
	# Settings:
	subdivisions = 1000
	
	# Internal Function:
	.integrate = function (f, lower, upper, subdivisions, rel.tol, 
	abs.tol, ...) {	# For Splus compatibility
		f = match.fun(f); ff = function(x) f(x, ...)
		wk = .External("call_dqags", ff, 
			rho = environment(), as.double(lower), 
			as.double(upper), as.double(abs.tol), 
			as.double(rel.tol), limit = as.integer(subdivisions), 
			PACKAGE = "base")
		wk[c("value", "abs.error", "subdivisions")] }
		
  	# Parameter Check:
	        if (pm != 0)     stop("Error: Only Parametrization 0 supported")
		if (alpha > +2)  stop("Error: alpha greater than 2")
		if (alpha <= 0)	 stop("Error: alpha less or equal 0")
		if (beta  < -1)  stop("Error: beta less than -1")
		if (beta  > +1)  stop("Error: beta greater than 1")
	# Special Cases:
		if (alpha == 2)  result = dnorm(x, mean=0, sd=sqrt(2))
		if (alpha == 1 & beta == 0) result = dcauchy(x) 
	# gamma, delta:
		x = (x-delta)/gamma
	# General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
    		if (abs(alpha-1) < 1 & alpha !=1 & abs(beta) <= 1) {
			subdivisions <<- subdivisions
			# Function to Integrate:
			"g" <<- 
			function(x, xarg, alpha, beta) {
				varzeta = -beta * tan(pi*alpha/2)
    				theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
      				v = (cos(alpha*theta0))^(1/(alpha-1)) *
        				(cos(x)/sin(alpha*(theta0+x)))^(alpha/(alpha-1)) *
        				(cos(alpha*theta0+(alpha-1)*x)/cos(x))
      				g = (xarg-varzeta)^(alpha/(alpha-1)) * v
      				gval = g * exp(-g) 
				gval}
			# Integration:	
			"fct" <<-
			function(xarg, alpha, beta) { 
				varzeta = -beta * tan(pi*alpha/2)
    				theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
				theta2 = optimize(f=g, lower=-theta0, upper=pi/2, maximum=TRUE,
					tol=.Machine$double.eps, xarg=xarg, alpha=alpha, 
					beta=beta)$maximum
				c2 = ( alpha / (pi*abs(alpha-1)*(xarg-varzeta)) ) 
        			result1 = .integrate(f=g, lower=-theta0, upper=theta2, 
          				subdivisions=subdivisions, 
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				result2 = .integrate(f=g, lower=theta2, upper=pi/2, 
          				subdivisions=subdivisions,
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				c2*(result1+result2) }
			# Loop over all x values:
    			result = rep(NA, times=length(x))  
    			for ( i in 1:length(result) ) { 
				varzeta = -beta * tan(pi*alpha/2)
      				if (x[i] == varzeta){
					theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
					result[i] = gamma(1+1/alpha)*cos(theta0) /
	     					(pi*(1+varzeta^2)^(1/(2*alpha)))} 
      				if (x[i] > varzeta) result[i] = 
      					fct(xarg=x[i], alpha=alpha, beta=beta)
      				if (x[i] < varzeta) result[i] = 
      					fct(xarg=-x[i], alpha=alpha, beta=-beta)}}
		# General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
		if (alpha == 1 & abs(beta) <= 1 & beta != 0) {
			subdivisions <<- subdivisions
			# Function to Integrate:
			"g" <<- 
			function(x, xarg, alpha, beta) {
				# x is a non-sorted vector!
      				v = (2/pi) * ((pi/2+beta*x) / cos(x)) *
					exp((1/beta)*(pi/2+beta*x)*tan(x))
      				g = exp( -pi*xarg/(2*beta) ) * v
				gval = g * exp(-g) 
				# replace NA at pi/2
				for (i in 1:length(gval)) if(is.na(gval[i])) gval[i] = 0
				gval }
			# Integration:	
			"fct" <<-
			function(xarg, alpha, beta) { 
				theta2 = optimize(f=g, lower=-pi/2, upper=pi/2, maximum=TRUE,
					tol=.Machine$double.eps, xarg=xarg, alpha=alpha, 
					beta=beta)$maximum
				c2 = 1 / (2*abs(beta)) 
        			result1 = .integrate(f=g, lower=-pi/2, upper=theta2, 
          				subdivisions=subdivisions, 
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps, 
          				xarg=xarg, alpha=alpha, beta=beta)$value
				result2 = .integrate(f=g, lower=theta2, upper=pi/2, 
          				subdivisions=subdivisions, 
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				c2*(result1+result2) }
			# Loop over all x values:
    			result = rep(NA, times=length(x))  
    			for ( i in 1:length(result) ) {
      				if (x[i] >= 0) result[i] = 
      					fct(xarg=x[i], alpha=alpha, beta=beta)
      				if (x[i] < 0) result[i] = 
      					fct(xarg=-x[i], alpha=alpha, beta=-beta) }}
  	
    # Return Value:
    result/gamma
}


# ------------------------------------------------------------------------------


pstable = 
function(q, alpha, beta, gamma = 1, delta = 0, pm = 0)
{	# A function implemented by Diethelm Wuertz

  	# FUNCTION:
  	
  	# Settings:
    subdivisions = 1000 	
  	x = q
  	
  	# Internal Function:
	.integrate = function (f, lower, upper, subdivisions, rel.tol, 
	abs.tol, ...) {	# For Splus compatibility
		f = match.fun(f); ff = function(x) f(x, ...)
		wk = .External("call_dqags", ff, 
			rho = environment(), as.double(lower), 
			as.double(upper), as.double(abs.tol), 
			as.double(rel.tol), limit = as.integer(subdivisions), 
			PACKAGE = "base")
		wk[c("value", "abs.error", "subdivisions")] }
		
	# Parameter Check:
	    if (pm != 0)     stop("Error: Only Parametrization 0 supported")
		if (alpha > +2)  stop("Error: alpha greater than 2")
		if (alpha <= 0)	 stop("Error: alpha less or equal 0")
		if (beta  < -1)  stop("Error: beta less than -1")
		if (beta  > +1)  stop("Error: beta greater than 1")
	# Special Cases:
		if (alpha == 2)  result = pnorm(x, mean=0, sd=sqrt(2))
		if (alpha == 1 & beta == 0) result = pcauchy(x) 
	# gamma, delta:
		x = (x-delta)/gamma
	# General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
    		if (abs(alpha-1) < 1 & alpha !=1 & abs(beta) <= 1) {
			tol <<- .Machine$double.eps
			subdivisions <<- subdivisions	
			# Function to Integrate:
			"G" <<- 
			function(x, xarg, alpha, beta) {
				varzeta = -beta * tan(pi*alpha/2)
    				theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
      				v = (cos(alpha*theta0))^(1/(alpha-1)) *
        				(cos(x)/sin(alpha*(theta0+x)))^(alpha/(alpha-1)) *
        				cos(alpha*theta0+(alpha-1)*x)/cos(x)
      				g = (xarg-varzeta)^(alpha/(alpha-1)) * v
      				gval = exp(-g)
				gval}
			# Integration:	
			"FCT" <<-
			function(xarg, alpha, beta) { 
				varzeta = -beta * tan(pi*alpha/2)
    				theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
				theta2 = optimize(f=G, lower=-theta0, upper=pi/2, maximum=TRUE,
					tol=.Machine$double.eps, xarg=xarg, alpha=alpha, 
					beta=beta)$maximum
				if (alpha < 1) c1 = (1/pi)*(pi/2-theta0)
				if (alpha > 1) c1 = 1
				c3 = sign(1-alpha)/pi
        			result1 = .integrate(f=G, lower=-theta0, upper=theta2, 
          				subdivisions=subdivisions, 
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				result2 = .integrate(f=G, lower=theta2, upper=pi/2, 
          				subdivisions=subdivisions, rel.tol=tol, abs.tol=tol,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				c1 + c3*(result1+result2) }
			# Loop over all x values:
    			result = rep(0, times=length(x))  
    			for ( i in 1:length(result) ) { 
				varzeta = -beta * tan(pi*alpha/2)
      				if (x[i] == varzeta){ 
					theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
					result[i] = (1/pi)*(pi/2-theta0)} 
      				if (x[i] > varzeta) result[i] = 
      					FCT(xarg=x[i], alpha=alpha, beta=beta)
      				if (x[i] < varzeta) result[i] = 
      					1-FCT(xarg=-x[i], alpha=alpha, beta=-beta)}}
		# General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
		if (alpha == 1 & abs(beta) <= 1 & beta != 0) {
			tol <<- .Machine$double.eps
			subdivisions <<- subdivisions	
			# Function to Integrate:
			"G" <<- 
			function(x, xarg, alpha, beta) {
				# x is a non-sorted vector!
      				v = (2/pi) * ((pi/2+beta*x) / cos(x)) *
					exp((1/beta)*(pi/2+beta*x)*tan(x))
      				g = exp( -pi*xarg/(2*beta) ) * v
				gval = exp(-g) 
				# replace NA at pi/2
				for (i in 1:length(gval)) if(is.na(gval[i])) gval[i] = 0
				gval 
			}
			# Integration:	
			"FUNC" <<-
			function(xarg, alpha, beta) { 
				theta2 = optimize(f=G, lower=-pi/2, upper=pi/2, maximum=TRUE,
					tol=.Machine$double.eps, xarg=xarg, alpha=alpha, 
					beta=beta)$maximum
				c3 = 1/pi
        			result1 = .integrate(f=G, lower=-pi/2, upper=theta2, 
          				subdivisions=subdivisions, 
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				result2 = .integrate(f=G, lower=theta2, upper=pi/2, 
          				subdivisions=subdivisions, 
					rel.tol=.Machine$double.eps, abs.tol=.Machine$double.eps,
          				xarg=xarg, alpha=alpha, beta=beta)$value
				c3*(result1+result2) 
			}
			# Loop over all x values:
    			result = rep(0, times=length(x))  
    			for ( i in 1:length(result) ) { 
      				if (beta >= 0) result[i] = 
      					FUNC(xarg=x[i], alpha=alpha, beta=beta)
      				if (beta < 0) result[i] = 
      					1-FUNC(xarg=-x[i], alpha=alpha, beta=-beta)
     			}
		}
  	
	# Return Value:
    result
}


# ------------------------------------------------------------------------------


qstable = 
function(p, alpha, beta, gamma = 1, delta = 0, pm = 0)
{	# A function implemented by Diethelm Wuertz

	# FUNCTION:
	
	# Settings:
	maxiter = 1000 
    subdivisions = 1000

	# Parameter Check:
	if (pm != 0)     stop("Error: Only Parametrization 0 supported")
	if (alpha > +2)  stop("Error: alpha greater than 2")
	if (alpha <= 0)	 stop("Error: alpha less or equal 0")
	if (beta  < -1)  stop("Error: beta less than -1")
	if (beta  > +1)  stop("Error: beta greater than 1")
	# Special Cases:
	if (alpha == 2)  result = qnorm(p, mean=0, sd=sqrt(2))
	if (alpha == 1 & beta == 0) result = qcauchy(p) 
	# Range 0 < alpha < 2:
	if (abs(alpha-1) < 1) {
		myUniroot = 
		function (f, interval, lower=min(interval), upper = max(interval), 
			maxiter, ...) 
			{
			if (f(lower, ...) * f(upper, ...) >= 0) result = NA
			else result = .Internal(zeroin(function(arg) f(arg, ...), 
				lower, upper, tol=.Machine$double.eps, as.integer(maxiter)))[1]
			result}
		froot = 
		function(x, alpha, beta, subdivisions, p) {
			pstable(q=x, alpha=alpha, beta=beta)-p }
		# Calculate:
		subdivisions <<-subdivisions
		result = rep(NA, times=length(p))
		for (i in 1:length(p)) {
			pp = p[i]
			if (beta < 0) {
				xmin = -(1-pp)/pp
				#xmax = pp/(1-pp)
				if (pp < 0.5) xmax = qnorm(pp, mean=0, sd=sqrt(2))
				else xmax = qcauchy(pp)}
			if (beta > 0 ) {
				#xmin = -(1-pp)/pp
				if (pp < 0.5) xmin = qcauchy(pp)
				else xmin = qnorm(pp, mean=0, sd=sqrt(2))
				xmax = pp/(1-pp)}
			if (beta == 0 ) {
				#xmin = -(1-pp)/pp
				if (pp < 0.5) xmin = qcauchy(pp)
				else xmin = qnorm(pp, mean=0, sd=sqrt(2))
				#xmax = pp/(1-pp) 
				if (pp < 0.5) xmax = qnorm(pp, mean=0, sd=sqrt(2))
				else xmax = qcauchy(pp)}
			iteration = NA
			counter = 0
			while (is.na(iteration)) {
				iteration = myUniroot(f=froot, c(xmin, xmax), 
					alpha=alpha, beta=beta,
					maxiter=maxiter, subdivisions=subdivisions, p=pp)
				counter = counter + 1
				xmin = xmin-2^counter
				xmax = xmax+2^counter}
			result[i] = iteration
		}
	}
	
	# Return Value:
	result*gamma+delta
}


# ------------------------------------------------------------------------------


rstable = 
function(n, alpha, beta, gamma = 1, delta = 0, pm = 0)
{ 	# A function implemented by Diethelm Wuertz

  	# Description:
  	#	Return random deviates from the stable family 
	#	of probability distributions.

	# FUNCTION:
	
  	# Calculate uniform and exponential distributed random numbers:
    theta = pi * (runif(n)-1/2)
    w = -log(runif(n))
  	# If alpha is equal 1 then:
    if (alpha == 1){
	   	result = rcauchy(n) }        
  		# Otherwise, if alpha is different from 1:
    else { 
      	c = (1+(beta*tan(pi*alpha/2))^2)^(1/(2*alpha))
      	theta0 = (1/alpha)*atan(beta*tan(pi*alpha/2))
      	result = ( c*sin(alpha*(theta+theta0))/
        	(cos(theta))^(1/alpha) ) *
        	(cos(theta-alpha*(theta+theta0))/w)^((1-alpha)/alpha) 
		# Use Parametrization 0:
		result = result - beta * tan(alpha*pi/2)}
  	
	# Return Value:
    result * gamma + delta
}
	

# ------------------------------------------------------------------------------

