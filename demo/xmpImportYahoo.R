
#
# Example: 
#	Import Data from Yahoo's Internet Site
#   
# Description: 
#	This example shows how to download stock market data from
#	the Yahoo internet site. Here we do it for the SP500 Index, 
#	with symbol ^SPC. Other US Index examples may be:
#	  ^DJI   Dow Jones Industrial
#	  ^GSPC   S&P 500 Index
#	  ^OEX   S&P 100 Index
#	  ^IXIC  NASDAQ Composite
#	  ^NDX   NASDAQ 100 Index
#	  ^NYA   NYSE Composite Index
#
# Note: 
#	If the service provider changes the data file format
#  	it may become necessary to modify the library function.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------
	
	
# Settings:

	symbol = "^DJI"
	file = "DJI.CSV"
	source = "http://chart.yahoo.com/table.csv?"
	query = paste ("s=", symbol,"&a=1&b=1&c=1998&g=d&q=q&y=0&z=", symbol, "&x=.csv", sep="")
	
# Download:	

	yahooImport(file, source, query, try = TRUE)

		