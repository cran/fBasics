\name{fBasicsData}

\alias{fBasicsData}

\alias{audusd}
\alias{bmwres}
\alias{fdax9710}
\alias{nyseres}
\alias{usdthb}

\alias{timezones.db}


\title{fBasics Data Sets}


\description{

	A collection of Data Sets used in the examples of the \code{fBasics}
	library.
	\cr
	
	The data sets are:
	
	\tabular{rll}{	
	 	1	\tab	\code{audusd.csv} 	
	 		\tab 	Reuters Tick-by-Tick AUDUSD rates 1997-10, \cr
	 	2	\tab	\code{usdthb.csv} 	
	 		\tab 	Reuters Tick-by-Tick USDTHB rates 1997, \cr
     	3	\tab	\code{fdax9710.csv} 	
     		\tab 	Minute-by-Minute DAX Futures Prices for 1997-10*, \cr
     	4	\tab	\code{fdax97m.csv} 	
     		\tab 	Minutely Time and Sales DAX Futures for 1997, \cr
     	5	\tab	\code{bmwres.csv} 	
     		\tab 	Daily log Returns of German BMW Stock Proces, \cr
     	6	\tab	\code{nyseres.csv} 	
     		\tab 	Daily log Returns of the NYSE Composite Index. }
     		
    *The file \code{fdax97m.csv} is too large and therefore not part of 
    the \code{fBasics} distribution. Please contact \emph{inf@rmetrics.org}.
    
     		
	Additionally the timezone datafile is:
	
	\tabular{rll}{
	 	1	\tab	\code{timezones.db} 	
	 		\tab 	file with all available Ical time zone names. }
	
}


\details{

	\bold{High Frequency Data for the AUDUSD and USDTHB:}
	\cr\cr
	\code{audusd} and \code{usdthb} archive high frequency exchange rates 
	for the Australian / US Dollar exchange rate in October 1997 and exchange 
	rates for the US Dollar / Thailand Bhat exchange rate in June 1997:
	A comma delimited CSV file with 6 columns. The first column,
	named \code{XDATE}, contains date/time entries in ISO-8601 format 
	as [CCYYMMDDhhmm], the second column, named \code{DELAY}, gives 
	the delay in minutes between the time stamp of Reuter's data
	record and arrival time at the local database, the third column
	named \code{CONTRIBUTOR} is Reuter's identification, a 4 character
	code, the fourth and fifth column, named \code{BID} and \code{ASK}
	are the bid and ask price quotations, and finally the sixth
	column, named \code{FLAG}, is not used and has zeros as entries.
	\cr
	
	\bold{DAX Futures Data:}
	\cr\cr
	\code{fdax9710} archives returns of minute-by-minute prices for 
	Dax Futures in October 1997:
	A comma delimited CSV file with 2 columns. The first column,
	named \code{XDATE}, contains date/time entries in ISO-8601 format 
	as [CCYYMMDDhhmm], the second column, named \code{FDAX}, gives 
	an averaged price of the Dax Futures, i.e. the mean of
	all volume weighted time and sales within the same minute.
	\cr
	\code{fdax97m} archives returns for minute-by-minute prices for Dax 
	Futures in 1997: A comma delimited CSV file with 2 columns. The first 
	column, named \code{XDATE}, contains date/time entries in ISO-8601 
	format as [CCYYMMDDhhmm], the second column, named \code{FDAX}, gives 
	a minutely averaged price during opening hours of the exchange, 
	i.e. the mean of all volume weighted time and sales within the 
	same minute.
	\cr
	
	\bold{Log returns for BMW Shares and NYSE Composite Index:}
	\cr\cr
	\code{bmwres} and \code{nyseres} archive log returns of the German 
	BMW stock listed in the German  DAX30 and log returns of the NYSE 
	Composite Index, both on a daily trading day time scale just numbering 
	the log returns: A one column CSV file with column names \code{BMW} 
	or \code{NYSERES}, respectively. The entries are the differences
	of the logarithmic prices on two succeeding trading days.
	\cr
	
	\bold{Calendar Data:}
	\cr\cr
	\code{holidays.db.R} is the database of world wide holidays with
	informations collected from several sources on the internet.
	\cr
    \code{timezones.csv} is a list of timezones information extracted
    from Olsen's database. Downloadable from
    \cr
    \emph{ftp://elsie.nci.nih.gov/pub/}
 	
}


\source{

	
	\code{audusd}\cr
	\code{usdthb}
	\cr
	The data were collected by D. Wuertz and R. Schnidrig from the 
	Reuter's data feed.
	
	\code{fdax9710}\cr
	\code{fdax97m}
	\cr
	The data were extracted from time and sales data records from
	the Frankfurt Futures Exchange.
	
	\code{bmwres}
	\cr
	The data were published in the EVIS software package.
	
	\code{nyseres}
	\cr
	The data were downloaded from the web site of the New York Stock 
	Exchange and the residuals were calculated as logarithmic price 
	differences.\cr
	\emph{http://www.nyse.com}.
	
}


\examples{
##  plot -
    xmpBasics("\nStart: Plot Residuals NYSE Composite Index > ")
    data(nyseres)
    x = as.ts(nyseres)
    par(mfrow = c(2, 1), cex = 0.75)
	plot(100*x, type = "l", col = "steelblue4",
	  main = "NYSE Composite Index")
	grid()
	plot(cumsum(x), type = "l", col = "steelblue4",
	  main = "Cumulated NYSE Index")
	grid()
}


\keyword{datasets}

