
#
# Example: 
#	Plot intra-daily/weekly histograms of volatility for HF Data 
#
# Description: 
#	This example shows how to plot intr-daily and intra-weekly 
#       histograms of hourly volatilities for German DAX Futures traded 
#       during 1997. The file has minutely averaged data records on a 
#       variable minutes time scale. 
#       
#       Use the function xts.dwh() with arguments:
#         "xts" 
#               is the data list(t, x) input, may be either a price,
#               a logprice, or return; choose properly "do.log" and  
#         "do.diff" flags:
#               xts prices  -        dol.log=T    do.diff=T
#               xts logprices -      dol.log=F    do.diff=T
#               xts (log)returns -   dol.log=F    do.diff=F
#         "from.date" 
#               (CCYYMMDD) and 
#         "to.date" 
#               cut out a proper part of the time series. Start on Monday, 
#               so the weekly plot also starts on Monday.
#         "deltat" 
#               is the width of the bins in minutes. 
#         "period" 
#               may be one of daily|weekly|both
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
		par(mfrow = c(2, 1))
		data(fdax97m)
	
		# Load Example Data File:
		xts = list(t = fdax97m[,"XDATE"], x = fdax97m[,"FDAX"])
		xts = xts.cut(xts, from.date = 19970106, to.date = 19971228)
	
		# Create Daily and Weekly Histograms:
		result = xts.dwh (xts, period = "both", dolog = TRUE, 
			dodiff = TRUE, deltat = 30, doplot = TRUE)
	} else {
		
		cat("\n\n  The datafile fdax97m is too large and therefore not part")
    	cat("\n  of this distribution. Please contact: inf@rmetrics.org\n\n")
	}

