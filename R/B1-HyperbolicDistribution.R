
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:		   		DESCRIPTION:		
#  dhyp		   		     Returns density for hyperbolic DF
#   phyp		   		  Returns probability for hyperbolic DF
#   qhyp		   		  Returns quantiles for hyperbolic DF
#   rhyp		   	      Returns random variates for hyperbolic DF
#  dnig		   		     Returns density for inverse Gaussian DF
#   pnig		   	      Returns probability for for inverse Gaussian DF
#   qnig		   		  Returns quantiles for for inverse Gaussian DF 
#   rnig		   	      Returns random variates for inverse Gaussian DF
################################################################################


dhyp = 
function (x, alpha = 1, beta = 0, delta = 1, mu = 0)
{	# A function implemented by Diethelm Wuertz
	
  	# Description:
	#	Return Hyperbolic Density Function PDF:
   	   
	# Arguments:
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter
	 
	# FUNCTION:
   	
	# Density:
	efun = exp( -alpha*sqrt(delta^2 + (x-mu)^2) + beta*(x-mu) )
	sqr = sqrt(alpha^2-beta^2)
	prefac = sqr / ( 2 * alpha * delta * besselK(delta*sqr, nu = 1) )
	ans = prefac * efun

	# Return Value:
    ans
}

  
# ------------------------------------------------------------------------------


phyp = 
function(q, alpha = 1, beta = 0, delta = 1, mu = 0, ...)
{	# A function implemented by Diethelm Wuertz

  	# Description:
	#	Return cumulative probability of Hyperbolic PDF:
  	 
	# Arguments:
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter

	# Function:
	
	# Cumulative Probability:
	ans = NULL
	for (Q in q) {
		ans = c(ans, integrate(dhyp, -Inf, Q, stop.on.error = FALSE, 
			alpha = alpha, beta = beta, delta = delta, mu = mu, ...)$value ) }

	# Return Value:
	ans
}


# ------------------------------------------------------------------------------


qhyp = 
function(p, alpha = 1, beta = 0, delta = 1, mu = 0)
{	# A function implemented by Diethelm Wuertz

	# Description:
	
	# Arguments:
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter

	# Note:
	#	This procedure will not run under Splus.

	# FUNCTION:
	
	# Settings:
	maxiter = 10000
	tol = .Machine$double.eps^0.25
	
	# Internal Functions:
	myUniroot <-
	function (f, lower, upper, tol, maxiter, ...) {
		if (f(lower, ...) * f(upper, ...) >= 0) root = NA
		else root = .Internal(zeroin(function(arg) f(arg, ...), 
			lower, upper, tol, as.integer(maxiter)))[1]
		root }		
	froot = 
	function(x, alpha, beta, delta, p) {
		phyp(q=x, alpha=alpha, beta=beta, delta=delta, mu=0) - p }
	
	# Loop over all p's:			
	result = NULL	
	for (pp in p) {
		lower = -1
		upper = +1			
		counter = 0
		iteration = NA
		while (is.na(iteration)) {
			iteration = myUniroot(f=froot, lower=lower, upper=upper, tol=tol, 
				maxiter=maxiter, alpha=alpha, beta=beta, delta=delta, p=pp)
			counter = counter + 1
			lower = lower-2^counter
			upper = upper+2^counter}		
		result = c(result, iteration) }
			
	# Return Value:
	result + mu
}	


# ------------------------------------------------------------------------------


rhyp = 
function (n, alpha = 1, beta = 0, delta = 1, mu = 0)
{	# A function implemented by Diethelm Wuertz
	
  	# Description:
	#	Returns random deviates of Hyperbolic PDF:
  	
	# Arguments:
	#	n - number of random deviates to be generated
	#   alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter
	
	# Notes:
	#	I have removed my original Fortran program and replaced it by
	# 	the dhyperb() function from the HyperbolicDist Package, written
	#   by David Scott, Ai-Wei Lee, Jennifer Tso, Richard Trendall 
	
	# FUNCTION:
	
	# Internal Function:
  	rhyperb = function (n, theta) {
	    hyp.pi = theta[1]
	    zeta = theta[2]
	    delta = theta[3]
	    mu = theta[4]
	    alpha = as.numeric(.hyperb.change.pars(1, 2, theta))[1] * delta
	    beta = as.numeric(.hyperb.change.pars(1, 2, theta))[2] * delta
	    phi = as.numeric(.hyperb.change.pars(1, 3, theta))[1] * delta
	    gamma = as.numeric(.hyperb.change.pars(1, 3, theta))[2] * delta
	    theta.start = -sqrt(phi * gamma)
	    t = -sqrt(gamma/phi)
	    w = sqrt(phi/gamma)
	    delta1 = exp(theta.start)/phi
	    delta2 = (w - t) * exp(theta.start)
	    delta3 = exp(-gamma * w)/gamma
	    k = 1/(delta1 + delta2 + delta3)
	    r = k * delta1
	    v = 1 - k * delta3
	    output = numeric(n)
	    need.value = TRUE
	    for (i in 1:n) {
	        while (need.value == TRUE) {
	            U = runif(1)
	            E = rexp(1)
	            if (U <= r) {
	                x = 1/phi * log(phi * U/k)
	                if (E >= alpha * (sqrt(1 + x^2) + x)) {
	                  need.value = FALSE } }
	            if ((U > r) & (U <= v)) {
	                x = t - 1/phi + U * exp(-theta.start)/k
	                if (E >= alpha * sqrt(1 + x^2) - beta * x + theta.start) {
	                  need.value = FALSE
	                } }
	            if (U > v) {
	                x = 1/gamma * log(k/gamma) - 1/gamma * log(1 - U)
	                if (E >= alpha * (sqrt(1 + x^2) - x)) {
	                  need.value = FALSE } } }
	        output[i] = delta * x + mu
	        need.value = TRUE }
	    output }
	    
	# Internal Function:
	.hyperb.change.pars = function (from, to, theta) {
	    delta <- theta[3]
	    mu <- theta[4]
	   	hyperb.pi <- theta[1]
	    zeta <- theta[2] 
	    if (from == 1 && to == 2) {
	        alpha <- zeta * sqrt(1 + hyperb.pi^2)/delta
	        beta <- zeta * hyperb.pi/delta
	        output = c(alpha = alpha, beta = beta, delta = delta, mu = mu) }
	    if (from == 1 && to == 3) {
	        phi <- zeta/delta * (sqrt(1 + hyperb.pi^2) + hyperb.pi)
	        gamma <- zeta/delta * (sqrt(1 + hyperb.pi^2) - hyperb.pi)
	        output = c(phi = phi, gamma = gamma, delta = delta, mu = mu)}
	    output }
	
	# Result - Use Standard Parameterization:
	Zeta = delta * sqrt(alpha^2 - beta^2)
	Pi = beta / sqrt(alpha^2 - beta^2)
	theta = c(Pi, Zeta, delta, mu)
	ans = rhyperb(n = n, theta = theta)

	# Return Value:
 	ans
}


# ******************************************************************************


dnig = 
function (x, alpha = 1, beta = 0, delta = 1, mu = 0)
{	# A function implemented by Diethelm Wuertz
	
  	# Description:
	#	Return Normal Inverse Gaussian Density Function PDF
	
	# Arguments:
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter
	
	# Notes:
	#	Function Calls:
	#	Splus: 
	#		Needs: xK1, x * Modified Bessel Function K1  
	#	Fortran:
	#		DLL/OBJ: rarm.dll rarm.obj
	#		SUBROUTINE DNIG(density, x, n, alpha, beta, delta, mu)
	#			density - density
	#			x       - x-vector
	#			n       - number of points
	#			alpha, beta, delta, mu
	#				- parameters of the density function

   	# FUNCTION:
   	
   	# Compute:
	result = .Fortran("dnig",
	  	as.double(1:length(x)),
		as.double(x),
		as.integer(length(x)),
		as.double(alpha),
		as.double(beta),
	    as.double(delta),
		as.double(mu),
		PACKAGE = "fBasics")[[1]]
		
	# Return Value:
	result
}


# ------------------------------------------------------------------------------


pnig = 
function (q, alpha = 1, beta = 0, delta = 1, mu = 0)
{	# A function implemented by Diethelm Wuertz

  	# Description:
	#	Return cumulative probability of inverse Gaussian PDF:

	# Arguments:
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter
	
	# Notes:
	#	Function Calls:
	#	SUBROUTINE PNIG(c, x, n, alpha, beta, delta, mu)
	#		c - cumulative probability
	#		q - q-vector
	#		n - number of points
	#		alpha, beta, delta, mu
	#		  - parameters of the density function

	# FUNCTION:
	result = .Fortran("pnig",
	  	as.double(1:length(q)),
		as.double(q),
		as.integer(length(q)),
		as.double(alpha),
		as.double(beta),
	    as.double(delta),
		as.double(mu),
		PACKAGE = "fBasics")
  	
	# Return Value:
    result[[1]]
}


# ------------------------------------------------------------------------------


qnig = 
function(p, alpha = 1, beta = 0, delta = 1, mu = 0)
{	# A function implemented by Diethelm Wuertz

	# Description:

	# Arguments:
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter
 
	# FUNCTIONS:
	
	# Settings:
	maxiter = 10000
	tol = .Machine$double.eps^0.25
	
	# Internal Functions:
	.uniroot = function (f, lower, upper, tol, maxiter, ...) {
		if (f(lower, ...) * f(upper, ...) >= 0) root = NA
		else root = .Internal(zeroin(function(arg) f(arg, ...), 
			lower, upper, tol, as.integer(maxiter)))[1]
		root}		
	froot = function(x, alpha, beta, delta, p) {
		pnig(q = x, alpha = alpha, beta = beta, delta = delta, mu = 0) - p }
	
	# Loop over all p's				
	result = NULL	
	for (pp in p) {
		lower = -1
		upper = +1			
		counter = 0
		iteration = NA
		while (is.na(iteration)) {
			iteration = .uniroot(f = froot, lower = lower, upper = upper, 
				tol = tol, maxiter = maxiter, alpha = alpha, beta = beta, 
				delta = delta, p = pp)
			counter = counter + 1
			lower = lower-2^counter
			upper = upper+2^counter}		
	result = c(result, iteration) }	
	
	# Return Value:
	result + mu
}	


# ******************************************************************************


rnig = 
function(n, alpha = 1, beta = 0, delta = 1, mu = 0)
{ 	# A function implemented by Diethelm Wuertz

  	# Description:
  	#	Return normal inverse Gaussian distributed random variates
  	
  	# Arguments:
  	#   n - number of deviates to be generated
	#	alpha, beta - Shape Parameter, |beta| <= alpha
	#	delta  - Scale Parameter, 0 <= delta
	#	mu - Location Parameter

	# FUNCTION:	
	
	# Settings:
	gamma = sqrt(alpha*alpha - beta*beta)
	
	# GAMMA = 0:
	if (gamma == 0) {
		V = rnorm(n)^2
		Z = delta*delta / V
   		X = sqrt(Z)*rnorm(n) }
	
   	# GAMMA > 0:
	else { 
		U = runif(n)
		V = rnorm(n)^2
		z1 = function(v, delta, gamma) {
			delta/gamma + v/(gamma^2) - sqrt( 2*v*delta/(gamma^3) + 
			(v/(gamma^2))^2 ) }
		z2 = function(v, delta, gamma) {
			(delta/gamma)^2 / z1(v = v, delta = delta, gamma = gamma)}
		pz1 = function(v, delta, gamma) {
			delta / (delta + gamma * z1(v = v, delta = delta, gamma = gamma) ) }
		s = (1-sign(U-pz1(v = V, delta = delta, gamma = gamma)))/2
		Z = z1(v = V, delta = delta, gamma = gamma)*s + z2(v = V, delta = delta, 
			gamma=gamma)*(1-s)
		X = mu + beta*Z + sqrt(Z)*rnorm(n) }
	
	# Return Value:
	X
}


# ******************************************************************************

