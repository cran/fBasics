
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
# S-PLUS WRAPPER:      DESCRIPTION:
#  which.min            Determines index of the minimum of a numeric vector
#  which.max            Determines index of the maximum of a numeric vector
#  model.weights        Returns weights of a model frame
#  strsplit             Splits elements of a character vector into substrings
#  match.fun            Verifies function from a 'function argument'
#  cov                  Computes covariance of vectors
#  forwardsolve         Solves linear equations for lower triangular systems 
#  %x%                  Computes generalised kronecker product of two arrays
#  data                 Loads or lists specified data sets
#  NROW                 Returns number of rows in a vector, array or data frame 
#  NCOL                 Returns number of columns ... 
#  sd                   Computes the standard deviation 
#  nlm                  Wraps R's "nlm" on S's "nlminb" optimizer
#  optim                Wraps R's "optim" on S's "nlminb" optimizer
#  download.file        Downloads files from Internet using "lynx" or "wget"
#  embed                Embeds time series into a low-dimensional space
#  as.Date              Convers date represenatation
################################################################################


if (!exists("which.min")) 
{	
which.min = 
function(x)
{	# A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
 	ans = order(x)[1]
 	
 	# Return Value:
    ans
}}

 
# ------------------------------------------------------------------------------


if (!exists("which.max")) 
{	
which.max = 
function(x)
{	# A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
 	ans = rev(order(x))[1]
 	
 	# Return Value:
    ans
}}

 
# ------------------------------------------------------------------------------


if (!exists("model.weights")) 
{
model.weights = 
function (x) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
	ans = x$"(weights)"
	
	# Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("strsplit")) {
strsplit = 
function(x, split = " ") 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = lapply(lapply(X = x, FUN = unpaste, sep = split), unlist) 
    
    # Return Value:
    ans
}}
 

# ------------------------------------------------------------------------------
  

if (!exists("match.fun")) {
match.fun =
function(FUN)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    if (is.function(FUN)) {
        ans = FUN
    } else {
        ans = getFunction(as.character(FUN))
    }
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("cov")) {
cov = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = cov.wt(x = x, ...)
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("forwardsolve")) {
forwardsolve = 
function (l, x, k = ncol(l)) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = backsolve(t(l), x, k = k)
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("%x%")) {
"%x%" =
function(X, Y) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = kronecker(X, Y)
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("data")) {
data = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    invisible(x)
}}


# ------------------------------------------------------------------------------


if (!exists("NROW")) {
NROW = 
function (x) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    if (is.array(x) || is.data.frame(x)) {
        ans = nrow(x) 
    } else {
        ans = length(x) 
    }
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("NCOL")) {
NCOL = 
function (x) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    if (is.array(x) && length(dim(x)) > 1 || is.data.frame(x)) {
        ans = ncol(x) 
    } else { 
        ans = as.integer(1) 
    }
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("sd")) {
sd = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    if (is.matrix(x)) 
        ans = apply(x, 2, sd)
    else if (is.vector(x)) 
        ans = sqrt(var(x))
    else if (is.data.frame(x)) 
        ans = sapply(x, sd)
    else 
        ans = sqrt(var(as.vector(x)))
        
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("nlm")) {
nlm = 
function(f, p, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    opt = nlminb(start = p, objective = f, ...)
    ans = list()
    ans$minimum = opt$objective
    ans$estimate = opt$parameters
    ans$gradient = opt$grad.norm
    ans$code = opt$message
    ans$iterations = opt$iterations
    
    # Return Value:
    ans
}}
    
    
# ------------------------------------------------------------------------------


if (!exists("optim")) {
optim = 
function(par, fn, hessian = TRUE, method = "Nelder-Mead", ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    # nlminb(start, objective, gradient = NULL, hessian = NULL, scale = 1, 
    #   control = NULL, lower = - Inf, upper = Inf, ...)

    opt = nlminb(start = par, objective = fn, gradient = NULL, 
        hessian = NULL, scale = 1, 
        control = nlminb.control(iter.max = 10000), 
        lower = -Inf, upper = Inf, ...)
        
    ans = list()
    ans$par = opt$parameters
    ans$value = opt$objective
    ans$counts = c(opt$f.evals, opt$g.evals)
    ans$message = opt$message
    if (is.logical(opt$convergence))
        ans$convergence = opt$convergence
    else
        ans$convergence = TRUE
    
    # Hessian:
    
    # Description:
    #   Compute numerical approx. to Hessian of f, evaluated at x
    #   usually need to pass additional parameters (e.g. data)
    # Details:
    #   N.B. this uses no numerical sophistication, so expect 3-4 
    #   figure accuracy, at best
    # Arguments:
    #   f - name of function that defines log likelihood (or negative of it)
    #   x - scalar or vector of parameters that give the point at which
    #       you want the hessian estimated (usually will be the mle )
    # Value:
    #   matrix of 2nd derivatives. Size is n x n, where n = length(x).
    # Author:
    #   Philip Dixon
    
    # Compute Hessian:
    f = fn
    x0 = ans$par
    n = length(x0)
    grad = rep(0, n)
    mdelta = hess = matrix(0, nrow = n, ncol = n)
    delta = 0.0001 * (sign(x0) + (x0 == 0)) * pmax(abs(x0), 0.01)
    diag(mdelta) = delta
    f0 = f(x0, ...)
    for(i in 1:n) {
        grad[i] = f(x0 + mdelta[, i], ...)
    }
    for(i in 1:n) {
        for(j in i:n) {
            hess[i, j] = 
                f(x0 + mdelta[, i] + mdelta[, j], ...) - grad[i] - grad[j]
            hess[j, i] = hess[i, j]
        }
    }   
    # Add Hessian:
    ans$hess = hess
    ans$grad = grad
    ans$f0 = f(x0, ...)
    ans$hessian = (hess + f0)/outer(delta, delta, "*")
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("download.file")) {
download.file =
function(url, destfile, method, ...)
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Download:
    if (method == "lynx") {
        command = paste("lynx.exe -source", url, ">", destfile, sep = " ")
        dos(command, ...)
        return(invisible())
    } 
    if (method == "wget") {
        command = paste("wget.exe", url, "-O", destfile, sep = " ")
        system(command, minimized = TRUE, ...)
        return(invisible())
    } 
        
}}


################################################################################

if (!exists("embed")) {
embed = 
function (x, dimension = 1) 
{	# A Function implemented by Diethelm Wuertz

	# Description:
	#	Embeds time series into a low-dimensional space
	
	# Note:
	#	A modified copy from R's embed()
	
	# For S-Plus compatibility:
    if (is.matrix(x)) {
        n = nrow(x)
        m = ncol(x)
        if ((dimension < 1) | (dimension > n)) 
            stop("Wrong embedding dimension")
        y = matrix(0, n - dimension + 1, dimension * m)
        for (i in (1:m)) y[, seq(i, by = m, length = dimension)] = 
        	Recall(as.vector(x[, i]), dimension)
        ans = y
    } else if (is.vector(x) || is.ts(x)) {
        n = length(x)
        if ((dimension < 1) | (dimension > n)) 
            stop("Wrong embedding dimension")
        m = n - dimension + 1
        ans = matrix(x[1:m + rep(dimension:1, rep(m, dimension)) - 1], m)
    } else {
	    stop("x is not a vector or matrix")
	}
    
    # Return Value:
    ans
}
}


# ------------------------------------------------------------------------------


if (!exists("as.Date")) 
{	
as.Date = 
function(x, format = "%d-%m-%y")
{	# A Function implemented by Diethelm Wuertz

	# Description:
	#	Mimics R's as.Date function
	
	# Used by yahooImport ...
	ans = timeDate(s, in.format = format, format = "%Y-%02m-%02d")	
	
	# Return Value:
    ans	
}
}


# ------------------------------------------------------------------------------

