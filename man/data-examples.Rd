\name{fBasicsData}
\alias{fBasicsData}

\alias{Capitalization}
\alias{cars2}
\alias{DowJones30}
\alias{HedgeFund}
\alias{msft.dat}
\alias{nyse}
\alias{PensionFund}
\alias{swissEconomy}
\alias{SWXLP}
\alias{usdthb}

%--------------------------------------------------------------
\title{fBasics data sets} 

\description{
  The following data sets are part of this package:
  \tabular{ll}{
    \code{Capitalization} \tab Market capitalization of domestic companies, \cr
    \code{cars2} \tab Data for various car models, \cr
    \code{DowJones30} \tab Down Jones 30 stocks, \cr
    \code{HedgeFund} \tab Hennessee Hedge Fund Indices, \cr
    \code{msft.dat} \tab Daily Microsoft OHLC prices and volume, \cr
    \code{nyse} \tab NYSE composite Index, \cr
    \code{PensionFund} \tab Swiss Pension Fund LPP-2005, \cr
    \code{swissEconomy} \tab Swiss Economic Data, \cr
    \code{SWXLP} \tab Swiss Pension Fund LPP-2000, \cr
    \code{usdthb} \tab Tick data of USD to THB.
  }    
}

%--------------------------------------------------------------
\details{

  All datasets are data frames. A brief description is given below.
  
  \bold{Capitalization:}\cr
  \code{Capitalization}
  contains market capitalization of 13 domestic companies for 6 years
  (from 2003 to 2008) in USD millions. Each row contains the data for
  one company/stock exchange.
 
  \bold{cars2:}\cr
  \code{cars2}
  contains columns \code{rowNames} (model), \code{Price},
  \code{Country}, \code{Reliability}, \code{Mileage}, (\code{Type}),
  (\code{Weight}), \code{Disp.} (engine displacement) and \code{HP}
  (\code{net horsepower}) reprsenting the indicated properties of 60 car
  models.
 
  \bold{DowJones30:}\cr
  \code{DowJones30}
  contains 2529 daily observations from the \sQuote{Dow Jones 30} Index
  series.  The first row contains the dates (from 1990-12-31 to
  2001-01-02). Each of the remaining thirty columns represents the
  closing price of a stock in the Index.
 
  \bold{HedgeFund:}\cr
  \code{HedgeFund}
  contains monthly percentage returns of 16 hedge fund strategies from
  Hennessee Group LLC for year 2005.
 
  \bold{msft.dat:}\cr
  \code{msft.dat}
  contains daily prices (open, high, low and close) and volumes for the
  Microsoft stocks.  It is a data frame with column names
  \code{"\%Y-\%m-\%d"}, \code{"Open"}, \code{"High"}, \code{"Low"},
  \code{"Close"}, \code{"Volume"}.

  \emph{Note:} there is a dataset, \code{MSFT}, in package
  \pkg{timeSeries} which contains the same data but is of class
  \code{"timeSeries"}.
 
  \bold{nyse:}\cr
  \code{nyse}
  contains daily records of the NYSE Composite Index from 1966-01-04 to
  2002-12-31 (9311 observations). The data is in column \code{"NYSE"}
  (second column). The first column contains the dates.
 
  \bold{PensionFund:}\cr
  \code{PensionFund}
  is a daily data set of the Swiss pension fund benchmark LPP-2005.
  The data set ranges from 2005-11-01 to 2007-04-11.
  The columns are named: SBI, SPI, SII, LMI, MPI, ALT, LPP25, LPP40, LPP60.
 
  \bold{swissEconomy:}\cr
  \code{swissEconomy}
  contains the GDP per capita (\code{GDPR}), exports (\code{EXPO}),
  imports (\code{IMPO}), interest rates (\code{INTR}), inflation
  (\code{INFL}), unemployment (\code{UNEM}) and population (\code{POPU})
  foryears 1964 to 1999 for Switzerland.
 
  \bold{SWXLP:}\cr
  \code{SWXLP}
  is a daily data set of the Swiss pension fund benchmark LPP-2000.  The
  data set ranges from 2000-01-03 to 2007-05-08 (1917 observations). The
  first column contains the dates. The remaining columns are named: SBI,
  SPI, SII, LP25, LP40, LP60.
 
  \bold{usdthb:}\cr
  \code{usdthb}
  Tick data of US Dollar (USD) in Thailand Bhat (THB) collected from
  Reuters. The date is in the first column in YYYYMMDDhhmm format. The
  remaining columns contain: delay time (\code{DELAY}), contributor
  (\code{CONTRIBUTOR}), bid (\code{BID}) and ask (\code{ASK}) prices,
  and quality flag (\code{FLAG}). It covers the Asia FX crisis in June
  1997.

}

%--------------------------------------------------------------
\references{
 \bold{Capitalization:}\cr
 \emph{World Federation of Stock Exchanges},
 http://www.world-exchanges.org/statistics.
 
 \bold{cars2:}\cr
 Derived from the car90 dataset within the rpart package.
 The car90 dataset is based on the car.all dataset in S-PLUS.
 Original data comes from:
 April 1990, \emph{Consumer Reports Magazine},
 pages 235-255, 281-285 and 287-288.
 % Package rpart (dataset: car90)
 % This is derived (with permission) from the data set car.all
 % in S-PLUS, but with some further clean up of variable names and definitions. 
 % http://ftp.yzu.edu.tw/CRAN/doc/contrib/Lam-IntroductionToR_LHL.pdf
 % http://www.uni-muenster.de/ZIV.BennoSueselbeck/s-html/helpfiles/car.all.html
 
 \bold{DowJones30}\cr
 https://www.yahoo.com.
 
 \bold{HedgeFund:}\cr
 http://www.hennesseegroup.com/indices/returns/year/2005.html.
 
 \bold{msft.dat:}\cr
 https://www.yahoo.com.
 
 \bold{nyse:}\cr
 https://www.nyse.com.
 
 \bold{PensionFund:}\cr
 SBI, SPI, SII: SIX (Swiss Exchange Zurich);
 LPP25, LPP40, LPP60: Banque Pictet Geneva;
 LMI, MPI, ALT: Recalculated from the indices and benchmarks.
 
 \bold{swissEconomy:}\cr
 https://www.oecd.org/ and https://www.imf.org/.
 
 \bold{SWXLP:}\cr
 SBI, SPI, SII: SIX (Swiss Exchange Zurich);
 LPP25, LPP40, LPP60: Banque Pictet Geneva.
 
 \bold{usdthb:}\cr
 Reuters Select Feed Terminal (1997).

 % \bold{Example:}\cr
 % McCullough B.D., Renfro C.G. (1998);
 % \emph{Benchmarks and Software Standards: A Case Study of GARCH Procedures},
 % Journal of Economic and Social Measurement 25, 59--71. 
}

%--------------------------------------------------------------
\examples{
## Plot DowJones30 Example Data Set
   series <- timeSeries::as.timeSeries(DowJones30)
   head(series)
   plot(series[,1:6], type = "l")

## msft.dat contains (almost?) the same data as MSFT in package timeSeries
data(MSFT, package = "timeSeries")

m1 <- as.matrix(msft.dat[, -1]) # drop date stamps in column 1
m2 <- as.matrix(MSFT)
all.equal(m1, m2, check.attributes = FALSE) # TRUE
## compare the dates:
all.equal(format(msft.dat[ , 1]), format(time(MSFT))) # TRUE
}
 
\keyword{datasets}
