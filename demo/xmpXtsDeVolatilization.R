
#
# Example: 
#	De-Volatilize a High Frequency Time Series
#   
# Description: 
#	This example shows how to de-volatilize the October 1997 FDAX 
#	times series data. It compares the result by time series plots 
#	and quantile-quantile plots.
#
# Notes:
#    The file "fdax97m.csv" is too large and therefore not part 
#    of  this distribution. Please contact: inf@rmetrics.org
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
	
		# Load Example Data File:
		prices = list(t = fdax97m[,"XDATE"], x = fdax97m[,"FDAX"])
		prices = xts.cut(prices, from.date = 19971006, to.date = 19971026)
	
		# Load Price Information:
		logprices = xts.log(prices)
		returns = xts.diff(logprices)
		plot(logprices$x,type = "l", main = "Prices in event time")
		plot(returns$x,type = "l", main = "Returns in event time")
		   	
		# Devolatilize Time Series With dv-Series Algorithm:
		kParameter = 8 
		AverageVolatility = 10*var(returns$x)
		dvseries = xts.dvs(prices, k = kParameter, 
			volatility = AverageVolatility,
			main = "De-Volatilized Prices") 
		qqgaussPlot(diff(dvseries$x), main = "QQplot: dv-series") 
		
	} else {
		
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n")
	}
	
	