\name{timeDateClass}

\alias{timeDateClass}

\alias{rulesFinCenter}
\alias{listFinCenter}

\alias{timeDate}
\alias{timeDate-class}
\alias{timeCalendar}
\alias{timeSequence}
\alias{Sys.timeDate}

\alias{timeLastDayInMonth}
\alias{timeNdayOnOrAfter}
\alias{timeNdayOnOrBefore}
\alias{timeNthNdayInMonth}
\alias{timeLastNdayInMonth}

\alias{is.timeDate}
\alias{print.timeDate}
\alias{summary.timeDate}
\alias{format.timeDate}


\title{timeDate Class}


\description{
    
    A collection of functions for managing date and time around the
    globe for any financial center. The concept allows for dealing
    with time zones, day light saving time and holiday calendars
    independent of the date and time specifications of the operating 
    system implemented on your computer. This is an important issue
    especially for R running under Microsoft's Windows operating
    system.
    
    IMPORTANT: \cr
    The \code{timeDate} and \code{timeSeries} classes are so far
    tested only under MS Windows XP operating system. Note, that
    your time zone environment variable must be set to "GMT". The
    reason for this is, that the functions work internally with "GMT"
    POSIX date and time objects whereas the Daylight Saving Time is 
    added on top of this through the information in the Ical Calendar
    Library.
    \cr
    
    The functions are:

    \tabular{rll}{   
    		\tab
    		\tab	\cr
    		\tab
    		\tab	\bold{Financial Centers:} \cr
        1   \tab    \code{rulesFinCenter}   
            \tab    Returns DST rules for a financial center, \cr
        2   \tab    \code{listFinCenter}    
            \tab    Lists all supported financial centers. \cr  
    		\tab
    		\tab	\cr
    		\tab		
    		\tab	\bold{Generation of 'timeDate' Objects:} \cr   
        1   \tab    \code{timeDate}         
            \tab    S4: Creates 'timeDate' object from a character vector, \cr
        2   \tab    \code{timeCalendar}     
            \tab    S4: Creates 'timeDate' object from calendar atoms, \cr
        3   \tab    \code{timeSequence}         
            \tab    S4: Creates regularly spaced object of class 'timeDate', \cr
        4   \tab    \code{Sys.timeDate}         
            \tab    Returns system time as an object of class 'timeDate'. \cr           
    		\tab
    		\tab	\cr
    		\tab
    		\tab	\bold{Special Monthly 'timeDate' Sequences:} \cr                
		1   \tab    \code{timeLastDayInMonth}   
            \tab    Computes the last day in a given month and year, \cr
        2   \tab    \code{timeNdayOnOrAfter}        
            \tab    Computes date that is a "on-or-after" n-day, \cr  
        3   \tab    \code{timeNdayOnOrBefore}       
            \tab    Computes date that is a "on-or-before" n-day, \cr
        4   \tab    \code{timeNthNdayInMonth}       
            \tab    Computes n-th ocurrance of a n-day in year/month, \cr
        5   \tab    \code{timeLastNdayInMonth}      
            \tab    Computes the last n-day in year/month. \cr            
            \tab
    		\tab	\cr
    		\tab
    		\tab	\bold{Representation of 'timedate' Objects:} \cr                
       	1   \tab    \code{is.timeDate}      
            \tab    Checks if the object is of class 'timeDate', \cr
       	2   \tab    \code{print.timeDate}       
            \tab    Prints 'timeDate', 'FinCenter' and 'Data' Slot, \cr
        3   \tab    \code{summary.timeDate}         
            \tab    Summarizes details of a 'timeDate' object, \cr
        4   \tab    \code{format.timeDate}      
            \tab    Formats 'timeDate' as ISO conform character string.}
        
}


\usage{
rulesFinCenter(FinCenter = myFinCenter)
listFinCenter(pattern = "*")

timeDate(charvec, format = NULL, zone = "GMT", FinCenter = myFinCenter) 
timeCalendar(y = currentYear, m = 1:12, d = NULL, h = NULL, min = NULL, 
    s = NULL, FinCenter = myFinCenter)
timeSequence(from = "2004-01-01", to = format(Sys.time(), "\%Y-\%m-\%d"), by =
    "day", length.out = NULL, format = "", FinCenter = myFinCenter)
Sys.timeDate(FinCenter = myFinCenter) 

timeLastDayInMonth(charvec, format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeNdayOnOrAfter(charvec, nday = 1, format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeNdayOnOrBefore(charvec, nday = 1, format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeNthNdayInMonth(charvec, nday = 1, nth = 1, format = "\%Y-\%m-\%d", 
	FinCenter = "GMT")
timeLastNdayInMonth(charvec, nday = 1, format = "\%Y-\%m-\%d", FinCenter = "GMT")

is.timeDate(object) 
\method{print}{timeDate}(x, \dots) 
\method{summary}{timeDate}(object, \dots)
\method{format}{timeDate}(x, \dots)
}


\arguments{
  
    \item{by}{
        a character string, containing one of "sec", "min",
        "hour", "day", "week", "month" or "year".
        This can optionally be preceded by an integer and a
        space, or followed by "s". 
        }
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }
    \item{format}{
        the format specification of the input character vector.
        }
    \item{from, to}{
        starting date, required, and end date, ptional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{h, min, s}{
        hours of the days (0-23), defaults are 0,
        minutes of the days (0-59), defaults are 0, and 
        seconds of the days (0-59), defaults are 0. 
        }
    \item{length.out}{
        length.out integer, optional. Desired length  
        of the sequence, if specified "to" will be ignored.
        }
    \item{nday}{
        an integer vector with entries ranging from 
        \code{0} (Sunday) to \code{6} (Saturday).
        }
    \item{nth}{
        an integer vector numbering the n-th occurence.
        }     
    \item{object}{
    	[is.timeData][summary] - \cr
    	an object of class \code{timeDate}.
    	}
    \item{pattern}{
        a pattern character string which can be recognized
        by the \code{grep} function. Wild cards are allowed.
        }
    \item{x}{
    	[format][print] - \cr
    	an object of class \code{timeDate}.
    	}
   	\item{y, m, d}{
        calendar years (e.g. 1997), defaults are 1960,
        calendar months (1-12), defaults are 1, and 
        calendar days (1-31), defaults are 1. 
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }   
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{rulesFinCenter}\cr
    \code{listFinCenter}
    \cr
    the first function returns a printed list of DST rules, the second
    lists time zones available in the database.
    \cr
    
    \code{timedate}\cr
    \code{timeCalendar}\cr
    \code{timeSequence}
    \cr
    return a S4 object of class 'timeDate'.
    \cr
    
    \code{Sys.timeDate}
    \cr
    returns the system time as an object of class 'timeDate'.
    \cr
    
    \code{timeLastDayInMonth}\cr
    \code{timeNdayOnOrAfter}\cr
    \code{timeNdayOnOrBefore}\cr
    \code{timeNthNdayInMonth}\cr
    \code{timeLastNdayInMonth}
    \cr
    return \code{timeDate} objects on special dates. For the function 
    \code{timeLastDayInMonth} the last day in a given month and year 
    will be returned, for \code{timeNdayOnOrAfter} the date in the 
    specified month that is a n-day  on or after a given date, for 
    \code{timeNdayOnOrBefore} the date that is a n-day on or before a 
    given date, for \code{timeNthNdayInMonth} the \code{nth} ocurrance 
    of a \code{nday} (nth = 1,...,5) in \code{year}, \code{month}, and
    for {timeLastNdayInMonth} the last \code{nday} in \code{year},
    \code{month}. 
    \cr
    
    \code{is.timeDate}
    \cr
    returns \code{TRUE} or \code{FALSE} depending on whether its
    argument is of \code{timeDate} type or not.
    \cr
    
    \emph{Print method:}
    \cr
    prints the financial center and date and time vector for a 
    \code{timeDate} object.
    \cr
    
    \emph{Summary method:}
    \cr
    returns a summary report of the details of a \code{timeDate} 
    object. This includes the starting and end date, the number of 
    dates the format and financial center in use.
    \cr
    
    \emph{Format Method:}
    \cr
    returns an ISO conform formatted character string.
}


\details{

    For the management of chronological objects under R three concepts 
    are available: The first is the implementation of date and time in R's 
    \code{chron} package neglecting locals, time zones and day light saving 
    times. This approach is appropriate for economic time series. The second 
    approach, available in R's base package implements the POSIX standard 
    to date and time objects, named \code{"POSIXt"}. Unfortunately, the 
    representation of these objects is operating system dependent and 
    especially under MS Windows several problems appear in the management 
    of time zones and day light saving times. Here we present a solution
    to overcome these difficulties with POSIX objects and introduce a 
    new S4 class of 'timeDate' objects which allow for powerful methods 
    to represent dates and times in different financial centers around 
    the world. Many of the basic functionalities of these objects are in 
    common with S-Plus' \code{timeDate} objects and thus many of your 
    privately written functions for \code{FinMetrics} may also be used 
    within R's environment.

    A major difference is the time zone concept which is replaced by the
    "Financial Center" concept. The \code{FinCenter} character variable
    specifies where you are living and at which financial center you are
    working. With the variable \code{myFinCenter} you can overwrite the
    default setting with your personal settings.  
    With the specification of the \code{FinCenter} your system knows what
    rules rules for day light saving times should be applied, what is your
    holiday calendar, what are your interest rate conventions. (Not all
    specifications are already implemented.) Many other 
    aspects can be easily accessed when a financial center is named. So we 
    can distinguish between Frankfurt and Zurich, which both belong to the 
    same time zone, but differed in DST changes in the eighties and have
    different holiday calendars. Futhermore, since the underlying time 
    refers to "GMT" and DST rules and all other information is available 
    in local (ASCII) databases, we are sure, that R delivers with such a 
    date/time concept on every computer independent of the operating system 
    in use, identical results. 

    Another important feature of the \code{"timeDate"} concept used here is 
    the fact that we don't rely on American or European ways to write dates.
    We use consequently the ISO-8601 standard for date and time notations.
    \cr
    
    
    \bold{Financial Centers}
    \cr\cr
    There are two functions concerned with the financial centers. The 
    first, \code{rulesFinCenter}, lists the daylight saving rules for a 
    selected financial center, and the second, \code{listFinCenter}, 
    lists all centers available in the database. There is no dependency 
    on the POSIX implementation of your operating system because all 
    time zone and day light saving time information is stored locally 
    in ASCII files. It is important to say, that the \code{TZ} environment 
    variable must set to \code{"GMT"} in your system environment that 
    there are no conflicts with the POSIX time zone management.
       
    Through the definition of financial centers it becomes possible to 
    introduce in the future a specification structure for financial
    centers, which includes further information for a center like
    holiday calendars, currency and interest rate conventions or many
    others.
    \cr
    
    
    \bold{Generation of timeDate Objects}
    \cr\cr
    We have defined a 'timeDate' class which is in many aspects similar
    to the S-Plus class with the same name, but has also some important
    advantageous differeneces. The S4 class has four Slots, the \code{Data} 
    slot which holds date and time as 'POSIXlt' objects in the standard
    ISO-8601 format, the \code{Dim} slot which gives the dimension
    of the data object, the \code{format} specification slot and the
    \code{FinCenter} slot which holds the name of the financial center. 
    
    Three functions allow to cgenerate date/time objects: \code{timeDate} 
    from character vectors, \code{timeCalendar} from date and time atoms, 
    and \code{timeSequence} from a "from/to" or from a "from/length" sequence 
    specification. Note, time zone transformations are easily handled by
    by the \code{timeDate} functions which can also take \code{timeDate} 
    and \code{POSIXt} objects as inputs, while transforming them between 
    financial centers and/or time zones specified by the arguments \code{zone} 
    and \code{FinCenter}. Finally the function \code{Sys.timeDate} returns 
    current system time in form of a \code{timeDate} object.
    \cr
    
    
    \bold{Special Monthly timeDate Sequences}
    \cr\cr
    We have implemented five functions to generate special monthly 
    sequences. These are functions to compute the last day in a given 
    month and year, to compute the dates in a month that is a n-day on or 
    after a given date, to compute the dates in a month that is a n-day 
    on or before a specified date, to compute the n-th ocurrances of a 
    n-day for a specified year/month vectors, and finally to compute 
    the last n-day for a specified year/month value or vector. n-days
    are numbered from 0 to 6 where 0 correspond to the Sunday and 6 to 
    the Saturday.
    \cr
    
    
    \bold{Tests and Representation of timeDate Objects:}
    \cr\cr
    We have implemented four S3 methods to test and represent
    \code{timeDate} objects. The method \code{is.timeDate} checks if 
    a given object is of class \code{"timeDate"}. The print method 
    returns the date/time in square \code{"[]"} brackets to dsitinguish
    the output from other date and time objects. On top of the date and
    time output the name of the \code{FinCenter} is printed. The summary 
    method returns a printed report with information about the 
    \code{"timeDate"} object. Finally, the format methods allows to 
    transform objects into a ISO conform formatted character strings.

}


\notes{

    These functions were written for Rmetrics users using R and Rmetrics
    under Microsoft's Windows XP operating system where timze zones, 
    daylight saving times and holiday calendars are insuffeciently 
    supported. The functions are untested for other system environments, 
    but may be used.
    
    The usage of the Ical Library and the introduction of the FinCenter
    concept was originally develloped for R Version 1.5. The 
    \code{timeDate} and \code{timeSeries} objects were added for 
    R Version 1.8.1. Minor changes were made to adapt the functions 
    for R Version 1.9.1. As a consequence, newer concepts like the 
    \code{Date} objects were not yet considered and included in this 
    collection of date and time concepts.
    
    IT IS IMPORTANT THAT YOU SET YOUR ENVIRONMENT VARIABLE TZ TO 'GMT',
    OTHERWISE SOME OF THE 'TIMEDATE' FUNCTIONS WOULD NOT PROPERLY WORK! 
    
    Beside the examples given in the manual pages additional demo files
    are available with much more examples including also those from the 
    book of Zivot and Wang.

}


\references
{
Bateman R., (2000);
    \emph{Time Functionality in the Standard C Library},
    Novell AppNotes, September 2000 Issue, 73--85.

ISO-8601, (1988);
    \emph{Data Elements and Interchange Formats -
    Information Interchange, Representation of Dates and Time},
    International Organization for Standardization,
    Reference Number ISO 8601, 14 pages.

James D.A., Pregibon D. (1992),
    \emph{Chronological Objects for Data Analysis},
    Reprint.
    
Ripley B.D., Hornik K. (2001);
    \emph{Date-Time Classes},
    R-News, Vol. 1/2 June 2001, 8--12.
    
Zivot, E., Wang J. (2003);
	\emph{Modeling Financial Time Series with S-Plus},
	Springer, New-York.
}


\seealso
{
	We recommend to inspect the help pages for the POSIX time and date 
	class, \code{?Dates},  and the help pages from the contributed R packages
	\code{chron} and \code{date}.
}


\examples{
## c -
   xmpBasics("\nStart: Create Character Vectors > ") 
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   dts; tms
   
## timeDate -
   xmpBasics("\nNext: Construct timeDate Object from Character Vector > ") 
   timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "GMT" )
   timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich") 
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S", 
     zone = "GMT", FinCenter = "GMT")
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S", 
     zone = "Europe/Zurich", FinCenter = "Europe/Zurich")
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S", 
     zone = "GMT", FinCenter = "Europe/Zurich") 
    
## timeCalendar - 
   xmpBasics("\nNext: Create a timeDate Object From Calendar Atoms > ")
   timeCalendar( m = c(9, 1, 8, 2), d = c(28, 15, 30, 9), 
     y = c(1989, 2001, 2004, 1990), FinCenter = "GMT") 
   timeCalendar(m = c(9, 1, 8, 2), d = c(28, 15, 30, 9), 
     y = c(1989, 2001, 2004, 1990), FinCenter = "Europe/Zurich")
   timeCalendar(h = c(9, 14), min = c(15, 23)) 
  
## timeSequence - 
   xmpBasics("\nNext: Create a Regularly Spaced timeDate Objects > ")
   timeSequence(from = "2004-03-12", to = "2004-04-11", 
        format = "\%Y-\%m-\%d", FinCenter = "GMT")  
   timeSequence(from = "2004-03-12", to = "2004-04-11", 
        format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")
   
## timeDate - 
   xmpBasics("\nNext: Note, ISO and American Formats are Auto-Detected > ")
   timeDate("2004-12-11", FinCenter = "GMT")    
   timeDate("12/11/2004", FinCenter = "GMT")    
}


\author{

    Diethelm Wuertz for this R-port.
    
}


\keyword{chron}

