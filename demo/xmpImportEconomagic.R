
#
# Example: 
#	Import Data from Economagic's Internet Site
#
# Description: 
#	This example shows how to download data from Economagic's
#	Internet site. We download the Fed Funds data published 
#  	by the Fed St. Louis. This file is named as "fedfunds+2"
#  	on the economagic website and located in the "fedstl"
#  	directory. This file is saved as a csv file with filename
#  	"fedfunds2.csv".
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

	file = "fedfunds2.csv"
	source = "http://www.economagic.com/em-cgi/data.exe/"
	query = "fedstl/fedfunds+2"

# Download:

	economagicImport(file, source, query, try = TRUE)

	