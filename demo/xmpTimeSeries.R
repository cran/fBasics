
#
# Example: 
#	Time Series Representations
#   
# Description: 
#	This example shows how to represent a financial time series
#	using vectors, matrixes, data.frames, and timeSeries objects.
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


################################################################################
# Representation of an univariate time series as a vector, matrix or data frame

	# What possibilities do we have to represent regular monthly, quarterly,
	# or annual data? We assume that we know the date when the data was
	# recorded. For example and of month data can bedelivered at the last
	# calendar date, or business day of a month. Here we consider the 
	# foreign exchange rates between the USD and EUR in the year 2003.
	
	# We write the values as a simple numeric vector x:
	x = c(
  		1.0622, 1.0785, 1.0797, 1.0862, 1.1556, 1.1674, 
  		1.1365, 1.1155, 1.1267, 1.1714, 1.1710, 1.2298)
  	
  	# We attribute the last day in the month to these values:
  	dates = c( 
  		"2003-01-31", "2003-02-28", "2003-03-31", 
  		"2003-04-30", "2003-05-31", "2003-06-30",
		"2003-07-31", "2003-08-31", "2003-09-30", 
		"2003-10-31", "2003-11-30", "2003-12-31")
	attr(x, "dates") <- dates
	
	# And we attribute an appropriate unit for the instrument:
	attr(x, "units") <- "USDEUR"

	# So this vector completely determines the time series:
	print(x)
	class(x)
	plot(x, type = "l")
	
	# Using a matrix representation we can use this concept for 
	# multivariate time series, e.g. add the USDCAD rate:
	y = c(
		1.5414, 1.5121, 1.4761, 1.4582, 1.3840, 1.3525,
		1.3821, 1.3963, 1.3634, 1.3221, 1.3130, 1.3128)
	X = matrix(c(x, y), ncol = 2)
	attr(X, "dates") <- dates
	attr(X, "units") <- c("USDEUR", "USDCAD")
	print(X)
	class(X)
	plot(x, type = "l", ylim = c(min(c(x, y)), max(c(x, y)) ))
	lines(y, col = "red")
	
	# Instead of using attributes we can attribute the units and
	# dates as column and rownames to the matrix:
	X = matrix(c(x, y), ncol = 2)
	colnames(X) = c("USDEUR", "USDCAD")
	rownames(X) = dates
	X
	
		
################################################################################  	
# Representation as a regular time series object of class 'ts':


	# R offers objects of classes 'ts' and 'mts' to represent
	# regular time series.
	x = c(
  		1.0622, 1.0785, 1.0797, 1.0862, 1.1556, 1.1674, 
  		1.1365, 1.1155, 1.1267, 1.1714, 1.1710, 1.2298)
	x.ts = ts(x, start = c(2003, 1), frequency = 12)
	###
	
	
	# Unfortunately, the full date gets lost, but in most cases
	# this would be irrelevant.
	# Optionally 'units' information and if needed full dates
	# can be added as attributes:
	attr(x.ts, "dates") <- dates
	attr(x.ts, "units") <- "USDEUR"
	print(x.ts)
	plot(x.ts)
	class(x.ts)
	unclass(x.ts)
	###
	
	
	# Now let us consider a multivariate time series:
	y = c(
		1.5414, 1.5121, 1.4761, 1.4582, 1.3840, 1.3525,
		1.3821, 1.3963, 1.3634, 1.3221, 1.3130, 1.3128)
	Y.ts = ts(cbind(USDEUR = x, USDCAD = y))	
	Y.ts
	###
	
	
	# Optionally full dates have to be attached as an attribute:
	attr(Y.ts, "dates") = dates
	Y.ts
	###
	
	
	# Plotting a multivariate 'ts' in one graph can be done as:
	ts.plot(Y.ts)
	# Or in multiple graphs using:
	plot(Y.ts)
	###
	
	
################################################################################
# Rmetrics 'timeSeries' class
	
	
	# USDEUR:
	x = c(
  		1.0622, 1.0785, 1.0797, 1.0862, 1.1556, 1.1674, 
  		1.1365, 1.1155, 1.1267, 1.1714, 1.1710, 1.2298)	
  	# USDCAD:
  	y = c(
		1.5414, 1.5121, 1.4761, 1.4582, 1.3840, 1.3525,
		1.3821, 1.3963, 1.3634, 1.3221, 1.3130, 1.3128)
	# Create Dates:
	oneDay.inSeconds = 3600*24
	dates = timeSequence(from = "2003-02-01", to = "2004-01-01", by = "month",
		format = "%Y-%m-%d", FinCenter = "GMT") - oneDay.inSeconds
	dates
	class(dates)
	###
	
	
	# Create Series:
	units = c("USDEUR", "USDCAD")
	FinCenter = "GMT"
	series = timeSeries(cbind(x, y), dates, units, FinCenter = "GMT")	
	series
	class(series)
	###
	
	
	# Time Series Slots
	slotNames(series)
	###
	
	
	# Print some of the slots ...
	series@Data
	series@positions
	series@units	
	series@format
	series@FinCenter
	###
	
	
	# Plot:
	plot(series)
	###
	
	
################################################################################

 