
#
# Example: 
#   Working with high frquency data on a business time scale.
#	1. Plot intra-daily/weekly histograms of volatility for HF Data
#	2. Interpolation of Intra Day Data Records
#	3. De-Seasonalize an Intra Day Time Series in Upsilon Time
#	4. De-Volatilize a High Frequency Time Series
#
# Desciption:
#
#   PART I:
#	Plot intra-daily/weekly histograms of volatility for HF Data 
#   [formerly: xmpXtsDailyWeeklyHists]
#		This example shows how to plot intr-daily and intra-weekly 
#       	histograms of hourly volatilities for German DAX Futures traded 
#       	during 1997. The file has minutely averaged data records on a 
#       	variable minutes time scale.       
#       Use the function xts.dwh() with arguments:
#         xts - is the data list(t, x) input, may be either a price,
#               a logprice, or return; choose properly "do.log" and  
#         do.diff - flags:
#               xts prices  -        dol.log = TRUE    do.diff = TRUE
#               xts logprices -      dol.log = FALSE   do.diff = TRUE
#               xts (log)returns -   dol.log = FALSE   do.diff = FALSE
#         from.date - (CCYYMMDD) and 
#         to.date - cut out a proper part of the time series. Start on  
#               Monday, so the weekly plot also starts on Monday.
#         deltat - is the width of the bins in minutes. 
#         period - may be one of daily|weekly|both
#
#	PART II: 
#	Interpolate of Intra Day Data Records
#   [formerly: xmpXtsInterpolation] 
#		This example shows how to interpolate the October 1997 FDAX time 
#		series data on a 15 minutes time scale. It compares the log prices
#		and returns on event- and 15 min time scales. In addition the ACF 
#		of volatilities, the periodogram, the scaling law behaviour and
#		the tail properties are investigated.
#
#	PART III: 
#	De-Seasonalize an Intra Day Time Series in Upsilon Time
#   [formerly: xmpXtsDeSeasonalization]
#		This example shows how to d-seasonalize a tick-by-tick or time&sales 
#		time series according to the de-sesonalization approach. It plots
#		prices and returns in event time, displays the time map, and 
#       finally prints the prices in upsilon time.
#
#	PART IV:
#	De-Volatilize a High Frequency Time Series
#   [formerly: xmpXtsDeVolatilization]
#		This example shows how to de-volatilize the October 1997 FDAX 
#		times series data. It compares the result by time series plots 
#		and quantile-quantile plots.
#
# Notes:
#    The file "fdax97m.csv" is too large and therefore not part 
#    of  this distribution. Please contact: info@rmetrics.org
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


################################################################################
    
   
# Part I:
# Plot intra-daily/weekly histograms of volatility for HF Data

	file.available = FALSE
    if (file.available) {
	    # Settings:
		options(object.size = 5e8) 
		par(mfrow = c(2, 1))
		data(fdax97m)
		# Load Example Data File:
		xts = list(t = fdax97m[, "XDATE"], x = fdax97m[, "FDAX"])
		xts = xts.cut(xts, from.date = 19970106, to.date = 19971228)
		# Create Daily and Weekly Histograms:
		result = xts.dwh (xts, period = "both", dolog = TRUE, 
			dodiff = TRUE, deltat = 30, doplot = TRUE) }			
	if (!file.available) {
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n") }


# ------------------------------------------------------------------------------


# Part II:
# Interpolate of Intra Day Data Records
	
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
	   	spectrum(abs(series), method = "pgram", spans = c(5, 7), plot = TRUE)
	   	scalinglawPlot(eptlogprices$x, span = 10, doplot = TRUE)
		qqgaussPlot(eptreturns$x, main = "QQ Plot")
		par(ask = FALSE) }
	if (!file.available) {	
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n") }
    	
			
# ------------------------------------------------------------------------------
   
      
# PART III:
# De-Seasonalize an Intra Day Time Series in Upsilon Time

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
			main = "Time Mapping") }			 
	if (!file.available) {	
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n") }
		
    		
# ------------------------------------------------------------------------------


# PART IV:
# De-Volatilize a High Frequency Time Series

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
		qqgaussPlot(diff(dvseries$x), main = "QQplot: dv-series") }	
	if (!file.available) {	
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n") }
	

# ------------------------------------------------------------------------------

			