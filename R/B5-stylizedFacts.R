
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


################################################################################
# FUNCTION:		   		DESCRIPTION:
#  acfPlot				 Autocorrelations function plot
#  pacfPlot				 Partial autocorrelation function Plot
#  ccfPlot				 Cross correlation function plot
#  teffectPlot			 Estimates and plots the Taylor Effect
#  lmacfPlot			 Estimates and plots the Long Memory ACF
################################################################################


acfPlot = 
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Autocorrelations function plot
	
	# FUNCTION:
	
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
	
	# Return value:
	ccf(x = x, y = y, ...)
}


# ------------------------------------------------------------------------------


teffectPlot =
function (x, deltas=seq(from=0.2, to=3.0, by=0.2), lag.max=10, ymax=NA, 
standardize=TRUE)
{	# A function implemented by Diethelm Wuertz
	
	# Description:
  	#	Evaluate and Display Taylor Effect

	# FUNCTION:
	
	# Standardize:
	if(standardize) x = (x-mean(x))/sqrt(var(x))
    	data = matrix(data=rep(0,times=lag.max*length(deltas)),
    	nrow = lag.max, byrow = TRUE)
  	for (id in 1:length(deltas))
    	data[,id] = as.double(acf(abs(x)^deltas[id], 
      		lag.max=lag.max, type="corr", plot=FALSE)$acf)[2:(lag.max+1)]
  	if (is.na(ymax)) ymax = max(data)
  	
  	# Plot:
  	plot(deltas, data[1,], ylim=c(0,ymax), type="n", 
    	xlab="Exponent Delta", ylab="autocorrelation",
    	main="Taylor Effect")
  	xl = 1:length(deltas)
  	for (il in 1:(lag.max)){
	 	yp = max(data[il,])
	 	yl = xl[data[il,]==yp]
    	lines(deltas, data[il,],col=il)
    	points(deltas[yl],yp)
    	lines (c(1,1),c(0,ymax))}
    		
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


# ------------------------------------------------------------------------------

