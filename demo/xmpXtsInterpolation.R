
#
# Example: 
#	Interpolate of Intra Day Data Records
#   
# Description: 
#	This example shows how to interpolate the October 1997 FDAX time 
#	series data on a 15 minutes time scale. It compares the log prices
#	and returns on event- and 15 min time scales. In addition the ACF 
#	of volatilities, the periodogram, the scaling law behaviour and
#	the tail properties are investigated.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Run:
	
	file.available = FALSE
    if (file.available) {
	    
		# Settings:
		options(object.size = 5e8)
		par(mfrow = c(2, 2))
		data(fdax97m)
	
		# Load Price Information:
		# Use October 1997 DAX Index Futures Data:
		prices = list(t = fdax97m[,"XDATE"], x = fdax97m[,"FDAX"])
		logprices = xts.log(prices)
		returns = xts.diff(logprices)
		plot(logprices$x, type = "l", main = "Index by Event")
		plot(returns$x, type = "l", main = "Returns by Event")  	
	
		# Investigate TS in Equidistant Time Steps, i.e. 15 minutes:
	   	from = 19971006 # start on first monday (included)
	  	to   = 19971102 # end at last sunday (to be extrapolated)
		prices = xts.cut(prices, from, to)
		eptprices = xts.interp(prices, deltat = 15)    
		eptlogprices = xts.log(eptprices)   
	  	eptreturns = xts.diff(eptlogprices)
		plot(eptlogprices$x, type = "l", main = "Index by 15 min")
		plot(eptreturns$x, type = "l", main = "Returns by 15 min")
		
		# Time Series Properties:
		par(ask = TRUE)	
		series = eptreturns$x
	   	acf(abs(series), type = "corr", plot = TRUE)
	   	spectrum(abs(series), method = "pgram", spans = c(5, 7), 
	   		plot = TRUE)
	   	scalinglawPlot(eptlogprices$x, span = 10, doplot = TRUE)
		qqgaussPlot(eptreturns$x, main = "QQ Plot")
		par(ask = FALSE)
	
	}
		