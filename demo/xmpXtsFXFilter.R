
#
# Example: 
#	Parse and Filter High Freuency Foreign Exchange Data
#
# Description:
#	This example shows how to parse and filter hig frequency
#	foreign exchange data records.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


################################################################################
# Settings:

	data(usdthb)

# Read The FX Data File:

	usdthb = data.frame(usdthb)
	NumberOfRecords = length(usdthb[,1])
	NumberOfRecords
	par(mfrow = c(2, 1))
	plot(usdthb[,4], type="l", 
		xlab = "Tick Number from Reuters THB=", 
		ylab = "100*log(Bid[n]/Bid[1])      Bid",
		ylim = c(-20,30), main="USDTHB June 1997 unfiltered")
	lines(x = c(1, NumberOfRecords), y=rep(usdthb[1,4], 2), col = 4)
	lines(-100*log(usdthb[1,4]/usdthb[,4]))
	lines(x = c(1, NumberOfRecords), y = c(0, 0), col = 4)

# Create a Contributor List:

	# xts.contributors(x, include)
	#	This function expects a data.frame file with intradaily records
	#	from the FX market in the same format. "Include" specifies up 
	#	to which percentage market makers are included.
	parser.table = fxdata.contributors(usdthb, include = 4)
	parser.table

# Now we parse the Data:

	usdthb = fxdata.parser(usdthb, parser.table)
	NumberOfRecords.parsed = length(usdthb[,1])
	NumberOfRecords.parsed 

# And finally filter the Data:

	usdthb = fxdata.filter(usdthb, parameter="strong")
	# Quick And Dirty Time Scaling
	Records = length(usdthb$accepted[,4])
	scale = NumberOfRecords/Records
	# Plot
	plot(x = (1:Records)*scale, y = usdthb$accepted[, 4], type = "l", 
		xlab = "Tick Number from Reuters THB=", 
		ylab = "100*log(Bid[n]/Bid[1])      Bid", 
		ylim = c(-20,30), main="USDTHB June 1997 filtered")
	lines(x = c(1, NumberOfRecords), y=rep(usdthb$accepted[1, 4], 2), 
		col = 4)
	lines(x =(1:Records)*scale, y=-100*log(usdthb$accepted[1, 4] /
		usdthb$accepted[, 4]))
	lines(x = c(1, NumberOfRecords), y = c(0, 0), col = 4)

# First 10 Accepted Records:

	NumberOfRecords.Accepted = length(usdthb$accepted[,1])
	NumberOfRecords.Accepted
	usdthb$accepted[1:min(10, NumberOfRecords.Accepted),]

# First 10 Rejected Records:

	NumberOfRecords.Rejected = length(usdthb$rejected[,1])
	NumberOfRecords.Rejected
	usdthb$rejected[1:min(10, NumberOfRecords.Rejected),]

# Rejected:

	Percent.Rejections = 100*NumberOfRecords.Rejected/NumberOfRecords
	Percent.Rejections

	