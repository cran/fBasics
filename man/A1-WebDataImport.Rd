\name{WebDataImport}

\alias{WebDataImport}

\alias{economagicImport}
\alias{yahooImport}
\alias{keystatsImport}
\alias{fredImport}

\title{Import Market Data from the Internet}


\description{
    
    A collection and description of functions to import 
    financial and economic market from the Internet.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{economagicImport} \tab Economic series from Economagic's Web site, \cr
    \code{yahooImport} \tab Daily stock market data from Yahoo's Web site, \cr
    \code{keystatsImport} \tab Key Statistics from Yahoo's Web site, \cr
    \code{fredImport} \tab Time series from St. Louis FRED Web site. }

}


\usage{
economagicImport(file = "tempfile", 
    source = "http://www.economagic.com/em-cgi/data.exe/", query, 
    frequency = c("quarterly", "monthly", "daily"), save = FALSE, 
    colname = "VALUE", try = TRUE)
    
yahooImport(file = "tempfile", 
    source = "http://chart.yahoo.com/table.csv?", query, save = FALSE,
    sep = ";", swap = 20, try = TRUE)
keystatsImport(file = "tempfile", 
    source = "http://finance.yahoo.com/q/ks?s=", query, save = FALSE, 
    try = TRUE) 
    
fredImport(file = "tempfile", 
    source = "http://research.stlouisfed.org/fred2/series/", 
    query, frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
}


\arguments{

    \item{colname}{
        a character string which defines the name of the value
        column. By default "VALUE".
        }
    \item{file}{
        a character string with filename, usually
        having extension ".csv", where to save the downloaded data.
        }
    \item{frequency}{
        a character string, one of "quarterly", "monthly", or "daily",
        defining the frequency of the data records.
        }
    \item{query}{
        a character string, denoting the location of the data at the 
        web site.
        }
    \item{save}{
        a logical value, if set to TRUE the downloaded data file will 
        be stored under the path and file name specified by the
        string \code{file}. By default FALSE.
        }
    \item{sep}{
        a character value, defining the field separator for the 
        destination file which saves the downloaded data records.
        By default a semicolon.
        }
    \item{source}{
        a character string with the download URL.
        }
    \item{swap}{
        [yahooImport] - \cr
        an integer value which determines when we swap from the
        19th to 20th century, by default 20, i.e. we swap 1920.
        This is necessary since Yahoo does not list the century in
        its dates, e.g. "15-Aug-02".
        }
    \item{try}{
        a logical value, if set to TRUE the Internet access will be
        checked.
        }
        
}


\value{

    A data frame with the downloaded data records or key statistics.
    
}


\details{

    \bold{Import data from www.economagic.com}
    \cr\cr
    Frequently requested data files from Economagic for the US economy include:

    \tabular{ll}{
    \code{[query]}                    \tab Description: \cr
                                      \tab \cr
    \code{var/leading-ind-long}       \tab Index of Leading Economic Indicators  \cr
    \code{beana/t102l01}              \tab Real Gross Domestic Product  \cr
    \code{fedstl/trsp500}             \tab SP 500 Total Return \cr
    \code{fedstl/gnp}                 \tab Gross National Product in Current Dollars \cr
    \code{var/cpiu-long}              \tab Consumer Price Index - All Urban Consumers \cr
    \code{feddal/ru}                  \tab Unemployment Rate \cr
    \code{fedstl/indpro}              \tab Total Industrial Production Index \cr
    \code{fedstl/exjpus+2}            \tab FX Rate: Japanese Yen to one US Dollar \cr
    \code{fedstl/fedfunds+2}          \tab Federal Funds Rate \cr
    \code{fedstl/mdiscrt+2}           \tab Discount Rate \cr
    \code{fedbog/tcm30y+2}            \tab 30-Year Treasury Constant Maturity Rate \cr
    \code{fedstl/mprime+2}            \tab Bank Prime Loan Rate \cr
    \code{fedstl/tb3ms+2}             \tab 3-Month Treasury Bills - Secondary Market \cr
    \code{fedstl/tb6ms+2}             \tab 6-Month Treasury Bills - Secondary Market \cr
    \code{fedbog/cm+2}                \tab 30 Year Federal Home Loan Mortgages \cr
    \code{var/west-texas-crude-long}  \tab Price of West Texas Intermediate Crude }

  
    \bold{Import data from chart.yahoo.com:}
    \cr\cr
    The query string is given as 
    \cr\cr
            \code{s=SYMBOL&a=DD&b=MM&c=CCYY&g=d&q=q&z=SYMBOL&x=.csv} 
    \cr\cr
    where \code{SYMBOL} has to replaced by the symbol name of the 
    instrument, and \code{DD}, \code{MM}, and \code{CCYY} by the 
    day, month-1 and century/year when the time series should start.

    Here are some examples of symbols:

    \tabular{ll}{
    \code{[query]}  \tab Description: \cr
                    \tab \cr
    \code{^DJI}     \tab Dow Jones 30 Industrial Averages \cr
    \code{^NYA}     \tab New York Stock Exchange Composite \cr
    \code{^NDX}     \tab Nasdaq 100 Index \cr
    \code{^IXIC}    \tab Nasdaq Composite Index \cr
    \code{^TYX}     \tab US 30Y Treasury Bond Index \cr
    \code{IBM}      \tab BM DJIA Stock \cr
    \code{KO}       \tab Coca-Cola DJIA Stock }

    The meaning of the tokens in the query string are the following:

    \tabular{ll}{
    Token           \tab Description  \cr
                    \tab \cr
    \code{s}        \tab Selected Ticker-Symbol \cr
    \code{a}        \tab First Quote starts with Month (mm) \cr
    \code{b}        \tab First Quote starts with Day (dd) \cr
    \code{c}        \tab First Quote starts with Year (ccyy) \cr
    \code{d}        \tab Last Quote ends with Month (mm) \cr
    \code{e}        \tab Last Quote ends with Day (dd) \cr
    \code{f}        \tab Last Quote ends with Year (ccyy) \cr
    \code{z}        \tab Selected Ticker-Symbol }

    Note, that month tokens range between 0 and 11 for January to 
    December!
    \cr
    
    
    \bold{Key Statistics data from finance.yahoo.com:}
    \cr\cr
    The functions downloads the key statistics for the specified
    equity query and returns the result as a two column data frame. 
    The key names included are: "Market Cap", "Enterprise Value",
    "Trailing P/E", "Forward P/E", "PEG Ratio", "Price/Sales",
    "Price/Book", "Enterprise Value/Revenue", "Enterprise Value/EBITDA",
    "Annual Dividend", "Dividend Yield", "Beta",
    "52-Week Change", "52-Week High", and "52-Week Low".
            
}


\note{
  
    Note, that if the service provider changes the data file format 
    it may become necessary to modify and update the functions. 
    
    The R package \code{tseries} from Adrian Trapletti offers an 
    alternative function to download stock market data and indexes 
    from Yahoo's Internet site.
  
}


\examples{
\dontrun{
## economagicImport -
   xmpBasics("\nStart: Daily Foreign Exchange Rates > ")
   USDEUR = economagicImport(query = "fedny/day-fxus2eu", 
     frequency = "daily", colname = "USDEUR", try = TRUE)
   # Print if Internet Download was Successful:
   if(!is.null(USDEUR)) print(USDEUR[1:20, ])

## economagicImport -
   xmpBasics("\nNext: USFEDFUNDS Monthly US FedFunds Rates > ")
   USFEDFUNDS = economagicImport(query = "fedstl/fedfunds+2", 
     frequency = "monthly", colname = "USFEDFUNDS", try = TRUE)
   if(!is.null(USFEDFUNDS)) print(USFEDFUNDS[1:20, ])
   
## economagicImport -
   xmpBasics("\nNext: USDGNP Quarterly GNP Data Records > ")
   USGNP = economagicImport(query = "fedstl/gnp", 
     frequency = "quarterly", colname = "USGNP", try = TRUE)
   if(!is.null(USGNP)) print(USGNP[1:20, ])  
     
## yahooImport -
   xmpBasics("\nNext: IBM Shares from Yahoo > ")
   # [test 19/20 century change 01-12-1999 -- 31-01-2000]
   IBM = yahooImport(
     query = "s=IBM&a=11&b=1&c=1999&d=0&q=31&f=2000&z=IBM&x=.csv", 
     try = TRUE)
   if (!is.null(IBM)) print(IBM[1:20, ])
   
## keystatsImport -
   xmpBasics("\nNext: Key Statistics IBM Shares from Yahoo > ")  
   keystatsImport(query = "IBM"  
   
## fredImport -
   xmpBasics("\nNext: DPRIME Daily Bank Prime Load Rate Records > ")
   DPRIME = fredImport(query = DPRIME)
   if (!is.null(DPRIME)) print(DPRIME[1:20, ])       
}
}

\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{data}

