
#
# Example: 
#	Import Data from the following Internet sites:
#   Economagic - www.economagic.com
#   Yahoo - chart.yahoo.com
#   Forecasts - www.forecasts.org
#
# Description: 
#	These examples show how to download data from Internet Sites.
#   Part I: Economagic's Internet Site: 
#	  We download the Fed Funds data published 
#  	  by the Fed St. Louis. This file is named as "fedfunds+2"
#  	  on the economagic website and located in the "fedstl"
#     directory. This file is saved as a csv file with filename
#     "fedfunds2.csv".
# 	PART II: Yahoo's Internet Site:
#     Here we download data for the SP500 Index, 
#	  with symbol ^SPC. Other US Index examples may be:
#	    ^DJI   Dow Jones Industrial
#	    ^GSPC   S&P 500 Index
#	    ^OEX   S&P 100 Index
#	    ^IXIC  NASDAQ Composite
#	    ^NDX   NASDAQ 100 Index
#	    ^NYA   NYSE Composite Index
# 	PART II: Forecasts' Internet Site
#
# Note: 
#	If the service provider changes the data file format
#  	it may become necessary to modify the library function.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


################################################################################
## Part I: Economagic's Internet Site


# Settings:

	file = "fedfunds2.csv"
	source = "http://www.economagic.com/em-cgi/data.exe/"
	query = "fedstl/fedfunds+2"

# Download:

	economagicImport(file, source, query, try = TRUE)

	
################################################################################
## Part II: Yahoo's Internet Site


# Settings:

	symbol = "^DJI"
	file = "DJI.CSV"
	source = "http://chart.yahoo.com/table.csv?"
	# 1.1.1999 - 31.12.2000
	# Note months count from 0 to 11 !
	query = paste ("s=", symbol,"&a=0&b=1&c=1999&d=11&e=31&f=2000&x=.csv", sep="")
	
# Download:	

	yahooImport(file, source, query, try = TRUE)


################################################################################
## Part III: Forecasts's Internet Site

	# The R function can be found in "funBasics.R"
	

################################################################################

