
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
# MA 02111-1307 USA

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
#  acfPlot				 Autocorrelations function plot
#  pacfPlot				 Partial autocorrelation function Plot
#  ccfPlot				 Cross correlation function plot
#  teffectPlot			 Estimates and plots the Taylor Effect
#  lmacfPlot			 Estimates and plots the Long Memory ACF
# FUNCTION:             DESCRIPTION:
#  logpdfPlot            Returns a pdf plot on logarithmic scale(s)
#  qqgaussPlot           Returns a Gaussian Quantile-Quantile plot
#  scalinglawPlot        Evaluates and displays a scaling law behavior
################################################################################


acfPlot = 
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Autocorrelations function plot
	
	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Return value:
	acf(x = x, ...)
}


# ------------------------------------------------------------------------------


pacfPlot = 
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#
	
	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Return value:
	pacf(x = x, ...)
}


# ------------------------------------------------------------------------------


ccfPlot = 
function(x, y, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#
	
	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	y = as.vector(y)
	
	# Return value:
	ccf(x = x, y = y, ...)
}


# ------------------------------------------------------------------------------


teffectPlot =
function (x, deltas = seq(from = 0.2, to = 3.0, by = 0.2), lag.max = 10, 
ymax = NA, standardize = TRUE)
{	# A function implemented by Diethelm Wuertz
	
	# Description:
  	#	Evaluate and Display Taylor Effect

	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Standardize:
	if(standardize) x = (x-mean(x))/sqrt(var(x))
    	data = matrix(data=rep(0,times=lag.max*length(deltas)),
    	nrow = lag.max, byrow = TRUE)
  	for (id in 1:length(deltas))
    	data[,id] = as.double(acf(abs(x)^deltas[id], 
      		lag.max = lag.max, type="corr", plot = FALSE)$acf)[2:(lag.max+1)]
  	if (is.na(ymax)) ymax = max(data)
  	
  	# Plot:
  	plot(deltas, data[1,], ylim = c(0, ymax), type = "n", 
    	xlab = "Exponent Delta", ylab = "autocorrelation",
    	main = "Taylor Effect")
  	xl = 1:length(deltas)
  	for (il in 1:(lag.max)){
	 	yp = max(data[il,])
	 	yl = xl[data[il,] == yp]
    	lines(deltas, data[il,], col = il)
    	points(deltas[yl],yp)
    	lines (c(1, 1), c(0, ymax))}
    		
	# Return Value:
  	invisible(data)
}


# ------------------------------------------------------------------------------


lmacfPlot = 
function(x, lag.max = 50, ci = 0.95, main = "ACF", doprint = TRUE)
{	# A function implemented by Diethelm Wuertz
	
	# Description:
  	#	Evaluate and display long memory autocorrelation Function.

	# FUNCTION:
	
	# Transform:
	x = as.vector(x)
	
	# Compute:
    z = acf(x, lag.max = lag.max, type = "correlation", plot = FALSE)
    z$acf[1] = 0
    cl = qnorm(0.5 + ci/2)/sqrt(z$n.used)
    z.min = min(z$acf, -cl)
  	
    # lin-lin plot excluding one:
    x = seq(0, lag.max, by = 1)
    y = z$acf 
    plot(x = x, y = y, type = "h", main = main, 
      	xlab = "lag", ylab = "ACF", xlim = c(0, lag.max))
    points(x = 0, y = 0)
    if (doprint) {
	cat ('\nLong Memory Autocorrelation Function:\n')
      	paste (cat ('\n  Maximum Lag        '),cat(lag.max))
      	paste (cat ('\n  Cut-Off ConfLevel  '),cat(cl))}
    lines(x=c(0,(lag.max+1)), y=c(cl,cl), lty=2, col=4)
   	
    # log-log:
    x = x[y > cl]
    y = y[y > cl]
   	# log-log:
	if (length(x) < 10) {
		fit = c(NA, NA)
		hurst = NA
		cat("\n  The time series exhibits no long memory! \n") }
	else {
    	plot(x = log(x), y = log(y), type = "l", xlab = "log(lag)", 
      		ylab = "log(ACF)", main = "log-log")
   	fit = lsfit(log(x), log(y))$coefficients
	###	fit = l1fit(log(x), log(y))$coefficients
    abline(fit[1], fit[2], col = 1)
    hurst = 1 + fit[2]/2 
    if (doprint) {
		paste (cat ('\n  Plot-Intercept     '), cat(fit[1]))
      	paste (cat ('\n  Plot-Slope         '), cat(fit[2]))
      	paste (cat ('\n  Hurst Exponent     '), cat(hurst), cat("\n")) } }
      			
	# Return Value:
  	list(fit = fit, hurst = hurst)
}


# ******************************************************************************


logpdfPlot = 
function(x, n = 50, doplot = TRUE, type = c("lin-log", "log-log"), ...)
{   # A function implemented by D. Wuertz
    
    # Description:
    #   Returns a pdf plot on a lin-log scale in
    #   comparisin to a Gaussian density plot
    #   and return the break-midpoints and the
    #   counts obtained from the histogram of
    #   the empirical data.
    
    # FUNCTION:
    
    # Transform:
	x = as.vector(x)
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Select Type:
    type = type[1]
    
    # Internal Function Log-LogPlot:
    loglogpdfPlot = function(x, n = 50, doplot = TRUE, ...) {
        # Internal FUNCTION:
        .hist = function(x, cells="FD", include.lowest=FALSE) { result = 
            hist(x, breaks=cells, include.lowest=include.lowest, plot=FALSE)
            prob.counts = result$counts/sum(result$counts) /
                diff(result$breaks)[1]
            list(breaks=result$breaks, counts=prob.counts) }
        # Histogram Count & Breaks:
        histogram = .hist(x, cells="fd", include.lowest=FALSE)
            yh = histogram$counts; xh = histogram$breaks
            xh = xh[1:(length(xh)-1)] + diff(xh)/2
            xh = xh[yh > 0]; yh = yh[yh > 0]
            yh1 = yh[xh < 0]; xh1 = abs(xh[xh < 0])
            yh2 = yh[xh > 0]; xh2 = xh[xh > 0]
            plot(log(xh1), log(yh1), type="p", ...) 
            par(err=-1); points(log(xh2), log(yh2), col=2) 
        # Compare with a Gaussian plot:
        xg = seq(from=min(xh1[1], xh[2]), 
            to=max(xh1[length(xh1)], xh2[length(xh2)]), length=n)
        xg = xg[xg > 0]
            yg = log(dnorm(xg, mean(x), sqrt(var(x))))
        par(err=-1); lines(log(xg), yg, col=3)
        # Return Value:
        invisible(list(breaks = c(xh1, xh2), counts = c(yh1, yh2), 
            fbreaks=c(-rev(xg), xg), fcounts=c(-rev(yg), yg))) }
        
    # Internal FUNCTION:
    .hist = function(x, cells="FD", include.lowest=FALSE) { 
        result = hist(x, breaks = cells, include.lowest = include.lowest, 
            plot = FALSE)
        prob.counts = result$counts/sum(result$counts)/diff(result$breaks)[1]
        list(breaks = result$breaks, counts = prob.counts) }

    # Lin-Log Plot:
    if (type == "lin-log") {
        # Histogram Count & Break-Midpoints:
        histogram = .hist(x, cells = "FD", include.lowest = FALSE)
            yh = histogram$counts
            xh = histogram$breaks
            xh = xh[1:(length(xh)-1)] + diff(xh)/2
            xh = xh[yh>0]
            yh = log(yh[yh > 0])
            if (doplot) {
                par(err=-1)
                plot(xh, yh, type = "p", ...)} 
        # Compare with a Gaussian Plot:
        xg = seq(from = xh[1], to = xh[length(xh)], length = n)
            yg = log(dnorm(xg, mean(x), sqrt(var(x))))
            if (doplot) { 
                par(err=-1)
                lines(xg, yg, col=2)}
        # Result:
        result = invisible(list(breaks = xh, counts = yh, 
            fbreaks = xg, fcounts = yg))}
    
    # Log-Log Plot:
    if (type == "log-log") {
        result = loglogpdfPlot(x = x, n = n, doplot = doplot, ...) }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


qqgaussPlot = 
function(x, span = 5, col = "steelblue4", main = "Normal Q-Q Plot", ...)
{   # A function implemented by D. Wuertz
    
    # Description:
    #   Returns a Quantile-Quantile plot.

    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Transform:
	x = as.vector(x)
    
    # Standardized qqnorm():
    y = (x-mean(x)) / sqrt(var(x))
    
    # Further Settings:
    y[abs(y) < span]
    lim = c(-span, span)
    
    # Plot qqnorm:
    qqnorm(y = y, main = main, xlim = lim, ylim = lim, col = col, ...) 

    # Add Line:
    qqline(y, ...)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


scalinglawPlot =
function(x, span = ceiling(log(length(x)/252)/log(2)), doplot = TRUE, ...)
{   # A function implemented by D. Wuertz
  
    # Description:
    #   Investigates the scaling law.
    #   The input "x" requires log-returns.

    # FUNCTION: 
    
    # Transform:
	x = as.vector(x)
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    logtimesteps = span
    xmean = mean(x)
    
    # x have to be logarithmic returns
    y = (x-xmean)
    logprices = cumsum(y)
    
    # Scaling Power Low:
    scale = function (nx, logprices) {
        sum(abs(diff(logprices, lag = (2^nx))))}
        
    nx = 0:logtimesteps; x = nx*log(2)
    y = log(apply(matrix(nx), 1, scale, logprices))
    fit = lsfit(x, y)$coefficients
    # Robust Fit:       
    # fit = l1fit(x, y)$coefficients
    alfa = 1.0/fit[2]
    if (doplot) { 
        plot(x, y, xlab = "log-time", ylab = "log-volatility", ...)
        title(main = "Scaling Law Plot", ...)
        grid()
        abline(fit[1], fit[2], col=2)
        abline(fit[1], 0.5, col=3) }
    
    # Return Value:
    list(exponent = as.numeric(alfa), fit = fit)
}


# ******************************************************************************

