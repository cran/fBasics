
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
#  tFit					 Fits Parameters of a Student-t Density
#  hypFit				 Fits Parameters of a hyperbolic Density
#  nigFit				 Fits Parameters of a normal inverse Gaussian Density
################################################################################


tFit = 
function(x, df, doplot = TRUE, span = seq(from = -10, to = 10, by = 0.1), ...)
{	# A function implemented by Diethelm Wuertz
   	
	# Description:
	#	Return Maximum log-likelihood estimated
   	#	Paramters for Student-t Distribution:
   	  
	# Notes:
	#   Function Calls: nlminb(), density() 
   	
	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Internal Function:
	.nlopt = function(start.p, objective.f, ...) {
		opt = nlm(f = objective.f, p = start.p, ...)
		list(estimate = opt$estimate, objective = opt$minimum, 			
		gradient = opt$gradient, message = opt$code) }
	
	# Settings:
	steps <<- 0
	
	# Log-likelihood Function:
	etmle = function(x, y = x) { 
		# Prevent from negative df's
		if (x[1] <= 0) x[1] = x.save
	   	f = -sum(log(dt(y, x[1])))
		# Print Iteration Path:
		steps <<- steps + 1
		cat("\n Optimization Step:         ", steps)
	   	cat("\n Objective Function Value:  ", -f)
		cat("\n Students df Estimate:      ", x[1], "\n") 
	   	x.save <<- x[1]
		f }
		
  	# Minimization:
	r = .nlopt(objective.f = etmle, start.p = c(df), print.level = 0, y = x)
	
	# Optional Plot:
	if (doplot) {
		par(err=-1)
		z = density(x, n = 100, ...)
		x = z$x[z$y > 0]
		y = z$y[z$y > 0]
		plot(x, log(y), xlim = c(span[1], span[length(span)]), 
			type = "p", xlab = "x", ylab = "log f(x)", ...)
		title("STUDENT-T: Parameter Estimation")
		y = dt(span, df = r$estimate)
		lines(x = span, y = log(y), col = "steelblue3") 
		grid() }
		
	# Return Values:
	list(estimate = r$estimate, objective = -r$objective, message = r$message,		
		gradient = r$gradient, steps = steps) 
}


# ------------------------------------------------------------------------------


hypFit = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, 
doplot = TRUE, span = seq(from = -10, to = 10, by = 0.1), ...)
{	# A function implemented by Diethelm Wuertz
   	
	# Description:
	#	Return Maximum log-likelihood estimated
   	#	Paramters for Hyperbolic Distribution:
   	   
	# Notes:
	#	Function Calls: density() 

	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Internal Function:
	.nlopt = function(start.p, objective.f, ...) {
		opt = nlm(f = objective.f, p = start.p, ...)
		list(estimate = opt$estimate, objective = opt$minimum, 			
		gradient = opt$gradient, message = opt$code)}
	
	# Settings:
	steps <<- 0
	
	# Log-likelihood Function:
	ehypmle = function(x, y = x){ 
	   	f = -sum(log(dhyp(y, x[1], x[2], x[3], x[4])))
		# Print Iteration Path:
		steps <<- steps + 1
		cat("\n Optimization Step:         ", steps)
	   	cat("\n Objective Function Value:  ", -f)
		cat("\n Parameter Estimates:       ", x[1], x[2], x[3], x[4], "\n") 
	   	f }
	   	
   	# Minimization:
	r = .nlopt(objective.f = ehypmle, start.p = c(alpha, beta, delta, mu), 
		print.level = 0, y = x)
		
	# Optional Plot:
	if(doplot) {
		par(err=-1)
		z = density(s, n = 100, ...)
		x = z$x[z$y > 0]
		y = z$y[z$y > 0]
		plot(x, log(y), xlim = c(span[1],span[length(span)]), 
			type = "p", xlab = "x", ylab = "log f(x)", ...)
		title("HYP: Parameter Estimation")
		y = dhyp(span, 
			alpha = r$estimate[1], 
			beta = r$estimate[2], 
			delta = r$estimate[3], 
			mu = r$estimate[4])
		lines(x = span, y = log(y), col = "steelblue3")
		grid() }
		
	# Return Value:
	list(estimate = r$estimate, objective = -r$objective, 
		message = r$message, gradient = r$gradient, steps = steps) 
}


# ------------------------------------------------------------------------------


nigFit = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, 
doplot = TRUE, span = seq(from = -10, to = 10, by = 0.1), ...)
{	# A function implemented by Diethelm Wuertz
   	
	# Description:
	#	Return Maximum log-likelihood estimated
   	#	Paramters for Inverse Gaussian Distribution:
   	  
	# Notes:
	#	Function Calls: 
	#	nlminb(), density() 
   	
	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Internal Function:
	.nlopt = function(start.p, objective.f, ...) {
		opt = nlm(f = objective.f, p = start.p, ...)
		list(estimate = opt$estimate, objective = opt$minimum, 			
		gradient = opt$gradient, message = opt$code) }
	
	# Settings:
	steps <<- 0
	
	# Log-likelihood Function:
	enigmle = function(x, y = x){ 
	   	f = -sum(log(dnig(y, x[1], x[2], x[3], x[4])))
		# Print Iteration Path:
		steps <<- steps + 1
		cat("\n Optimization Step:         ", steps)
	   	cat("\n Objective Function Value:  ", -f)
		cat("\n Parameter Estimates:       ", x[1], x[2], x[3], x[4], "\n") 
	   	f }
	   	
   	# Minimization:
	r = .nlopt(objective.f = enigmle, start.p = c(alpha, 
		beta, delta, mu), print.level = 0, y = x)
		
	# Optional Plot:
	if (doplot) {
		par(err=-1)
		z = density(x, n=100, ...)
		x = z$x[z$y>0]
		y = z$y[z$y>0]
		plot(x, log(y), xlim=c(span[1], span[length(span)]), type="p", 
			xlab="x", ylab="log f(x)", ...)
		title("NIG: Parameter Estimation")
		y = dnig(span, 
			alpha=r$estimate[1], 
			beta=r$estimate[2], 
			delta=r$estimate[3], 
			mu=r$estimate[4])
		lines(x=span, y=log(y), col="steelblue3") 
		grid() }
	
	# Return Value:
	list(estimate = r$estimate, objective = -r$objective,	
		message = r$message, gradient = r$gradient, steps = steps) 
}


# ******************************************************************************

