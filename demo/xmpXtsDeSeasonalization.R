
#
# Example: 
#	De-Seasonalize an Intra Day Time Series in Upsilon Time
#
# Description:
#	This example shows how to d-seasonalize a tick-by-tick or time&sales 
#	time series according to the de-sesonalization approach. It plots
#	prices and returns in event time, displays the time map, and 
#       finally prints the prices in upsilon time.
#
# Notes:
# 	The file "fdax97m.csv" is too large and therefore not part 
# 	of  this distribution. Please contact: inf@rmetrics.org
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
		data(fdax9710)
	
		# Load Example Data File:
		prices = list(t = fdax9710[,"XDATE"], x = fdax9710[,"FDAX"])
		prices = xts.cut(prices, from.date=19971006, to.date=19971026)
		MeanTimeInterval = 60
		
		# Load Price Information:
		logprices = xts.log(prices)
		returns = xts.diff(logprices)
		plot(logprices$x,type="l", main="Prices in event time")
		plot(returns$x,type="l", main="Returns in event time")
	   	
		# Create Hourly Upsilon Time Map:
		tmap = xts.map(prices, mean.deltat = MeanTimeInterval, alpha = 1.05)
		tmap
		
		# Extract Data Records According to Time Map:
	  	upsilon.prices = xts.upsilon(prices, weekly.map = tmap$ymap, 
			main = "Prices in upsilon time")
		plot(x = tmap$xmap, y = tmap$ymap, type = "l", 
			main = "Time Mapping")  
			 
	} else {
		
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n")
	}
	