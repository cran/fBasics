\name{MoFiTSData}

\alias{MoFiTSData}

\alias{CPI.dat}
\alias{DowJones30}
\alias{highFreq3M.df}
\alias{IP.dat}
\alias{lexrates.dat}
\alias{msft.dat}
\alias{shiller.annual}
\alias{shiller.dat}
\alias{singleIndex.dat}
\alias{varex.ts}
\alias{yhoo.df}


\title{fBasics Data Sets from MoFiTS}


\description{
        
     Data sets used for the examples from the book "Modeling Financial 
     Time Series with S-Plus" by E. Zivot and J. Wang.
     
     The data sets are:
     
     \tabular{rll}{
        1   \tab    \code{CPI.dat}  
            \tab    Seasonally adjusted US Consumer Price Index, \cr 
        2   \tab    \code{DowJones30.csv}   
            \tab    Dow Jones Industrial Average Index, \cr 
        3   \tab    \code{highFreq3M.df}    
            \tab    3M High Frequency Stock Market Data, \cr
        4   \tab    \code{IP.dat}   
            \tab    Seasonally Adjusted US Industrial Production Index, \cr              
        5	\tab	\code{lexrates.dat}
        	\tab	Spot and Forward Exchange Rate Data, \cr
       	6   \tab    \code{msft.dat}     
            \tab    Open, High, Low, Close of Microsoft Stocks, \cr
        7   \tab    \code{shiller.dat}    
            \tab    Robert Shiller's Monthly Economic Data Set, \cr 
        8	\tab	\code{shiller.annual}
        	\tab	Robert Shiller's Monthly Economic Data Set, \cr
        9   \tab    \code{singleIndex.dat.csv}  
            \tab    Microsoft Stocks and SP500 Index Data, \cr 
        10 	\tab	\code{varex.ts.csv}
        	\tab	Real Stock Returns and Output Growth Data, \cr
        11  \tab    \code{yhoo.df}  
            \tab    Yahoo Stock Information. }      
                        
}


\details{


    \bold{Dow Jones Industrial Average:}
    \cr\cr
    The file \code{DowJones30.csv} contains closing prices for the 
    30 stocks represented in the Dow Jones Industrial Average Index. 
    \cr
    Data are downloadable and can be updated from Yahoo's web site. 
    \cr
    

    \bold{Microsoft Stocks and SP500 Index Data:}
    \cr\cr
    The file \code{singleIndex.dat.csv} contains the monthly closing 
    prices for Microsoft Corporation and the SP 500 index.
    \cr
    Data are downloadable and can be updated from Yahoo's web site.
    \cr
 
    
    \bold{Open, High, Low, Close of Microsoft Stocks:}
    \cr\cr
    The file \code{msft.dat.csv} contains data representing the open, 
    high, low, close and volume information for Microsoft stocks. 
    \cr
    Data are downloadable and can be updated from Yahoo's web site.
    \cr
    
    
    \bold{IP and CPI Index Data:}
    \cr\cr
    The file \code{IP.dat.csv} contains data representing seasonally 
    adjusted US Industrial Production Index and the file 
    \code{CPI.dat.csv} contains data representing seasonally 
    adjusted US Consumer Price Index. 
    \cr
    Data are downloadable and can be updated from Economagics's web site. 
    \cr
    
        
    \bold{3M high Frequency Data:}
    \cr\cr
    The file \code{highFreq3M.df.csv} holds Tsay's data for 3M. Date 
    information is expressed as the day of the month and the number of 
    seconds from midnight Data is for December 1999. Columns are: day - 
    integer representing the trading day of the month, sec - trade.time 
    integer representing the trading time recorded as the number of 
    seconds from midnight, price transaction price in dollars. Downloadable
    from:
    \cr
    \emph{http://www.gsb.uchicago.edu/fac/ruey.tsay/teaching/fts}
    \cr
    
    
    \bold{Spot and Forward Exchange Rate Data:}
    \cr\cr
    The file \code{lexrates.dat.csv} holds log spot exchange and 
    forward exchange rates between 
    \code{USCNS, USCNF} -  USD and CAD, Canadian Dollar,
    \code{USDMS, USDMF} -  USD and DEM, Deutsche Mark,
    \code{USFRS, USFRF} -  USD and FFR, French Franc,
    \code{USILS, USJYF} -  USD and JPY, Italian Lira, and 
    \code{USUKS, USUKF} -  USD and GBP, Japanese Yen.
    \cr
    \emph{Source: Thompson Financial, formerly Datastream}, see also:
    \emph{Zivot, E. (2000) Cointegration and forward and spot exchange 
    	rate regressions, Journal of International Money and Finance, 
    	19, 387--401 and 785--812.}
    \cr
    
    
    \bold{Robert Shillers Monthlly and Annual Economic Data:}
    \cr\cr
    The files \code{shiller.dat.csv} and \code{shiller.annual.csv} hold
	data used in the book "Irrational Exuberance" by Robert Shiller. The 
	data are 
	\code{price} - monthly nominal US SP stock market prices,
	\code{dividend} - nominal SP Composite Index dividends,
	\code{earnings} - nominal SP Composite Index earnings,
	\code{cpi} - US Consumer Price Indexes,
	\code{real.price} - real US stock market prices,
	\code{real.dividend} - real SP Composite Index dividends,
	\code{real.earnings} - real SP Composite Index earnings,
	\code{pe.10} - price-earnings ratios,
	\code{dp.ratio} - dividend-price ratios,
	\code{dp.yield} - dividend-price yield. The last two are only 
	listed in \code{shiller.annual}.
	The series start January 1871 and end on March 2001.
	Data are available from:
    \cr
    \emph{Shiller, R.J. (1989) Market Volatility, MIT Press.}\cr
	\emph{Shiller, R.J. (2001) Irrational Exuberance, Broadway Books.}
	\cr
	
	    
    \bold{Yahoo Stock Information:}
    \cr\cr  
    The file \code{yhoo.df.csv} contains data representing daily 
    transaction information of Yahoo stock, with the following 
    six columns: Date, Open, High, Low, Close, Volume.
    \cr
    Data are downloadable and can be updated from Yahoo's web site.
    
    
    \bold{Real Stock Returns and Output Growth Data:}
    \cr\cr  
    The file \code{varex.ts.csv} contains real stock returns and output 
	growth data. The column MARKET.REAL lists continuously compounded 
	real returns on the SP500 index, the column RF.REAL lists real 
	interest rates of 30-day US Treasury Bills, the column INF lists
	continuously compounded growth rate of US CPI, and the column
	IPG lists continuously compounded growth rate of US industrial 
	production. The data are monthly starting December 1959 and 
	ending December 2000.
    \cr
    Data are downloadable and can be updated from Economagic's web site.
    	
}

\examples{
## no examples ...
}


\keyword{datasets}

