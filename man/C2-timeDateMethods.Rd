\name{TimeDateMethods}

\alias{TimeDateMethods}
\alias{timeDateMethods}

\alias{[.timeDate}
\alias{+.timeDate}
\alias{-.timeDate}
\alias{Ops.timeDate}
\alias{diff.timeDate}
\alias{difftimeDate}

\alias{c.timeDate}
\alias{rep.timeDate}

\alias{start.timeDate}
\alias{end.timeDate}

\alias{modify.timeDate}
\alias{rev.timeDate}

\alias{as.character.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}
\alias{julian.POSIXt}
\alias{julian.timeDate}
\alias{atoms.timeDate}
\alias{months.timeDate}


\title{timeDate Methods}


\description{
    
    A collection and description of methods for 
    objects of class 'timeDate'. Included are S3 
    methods for mathematical operations and for 
    transformations between different representations 
    of date/time objects.
    \cr
    
    IMPORTANT: \cr
    The \code{timeDate} and \code{timeSeries} classes are so far
    tested only under MS Windows XP operating system. Note, that
    your time zone environment variable must be set to "GMT". The
    reason for this is, that the functions work internally with "GMT"
    POSIX date and time objects whereas the Daylight Saving Time is 
    added on top of this through the information in the Ical Calendar
    Library.
    \cr
    
    The methods for Math Operations with 'timeDate' Objects are:

    \tabular{ll}{   
 	\code{[.timeDate} \tab Extracts, replaces subsets from 'timeDate' objects, \cr
	\code{+.timeDate} \tab Performs arithmetic + ops on 'timeDate' objects, \cr
	\code{-.timeDate} \tab  Performs arithmetic - ops on 'timeDate' objects, \cr
	\code{Ops.timeDate} \tab 'Ops' generic functions for 'timeDate' objects, \cr
	\code{diff.timeDate} \tab Returns suitably lagged and iterated differences, \cr
	\code{difftimeDate} \tab Returns a difference of two 'timeDate' objects, \cr
	\code{c.timeDate} \tab Concatenates objects of class 'timeDate', \cr
	\code{rep.timeDate} \tab Replicates objects of class 'timeDate', \cr
	\code{start.timeDate} \tab Extracts the first object of a 'timeDate' vector, \cr
	\code{end.timeDate} \tab Extracts the last object of a 'timeDate' vector, \cr
	\code{modify.timeDate} \tab Sorts, rounds or truncates a 'timeDate' object, \cr
	\code{rev.timeDate} \tab Reverts  a 'timeDate' vector object. }          
     		
    The methods for Transformations of 'timeDate' Objects are:
        
    \tabular{ll}{       
	\code{as.character.timeDate} \tab Returns a 'timeDate' object as character string, \cr
	\code{as.data.frame.timeDate} \tab Returns a 'timeDate' object as data frame, \cr
	\code{as.POSIXct.timeDate} \tab Returns a 'timeDate' object as POSIXct object, \cr
	\code{as.POSIXlt} \tab As POSIXlt, Use Method (Overwrite), \cr
	\code{as.POSIXlt.default} \tab Default method for POSIXlt, \cr
	\code{julian.timeDate} \tab Returns Julian day counts since 1970-01-01, \cr
	\code{julian.POSIXt} \tab A Julian Patch, \cr
	\code{atoms.timeDate} \tab Returns date/time atoms from a 'timeDate' object, \cr
	\code{atoms} \tab Extract atoms, Use Method, \cr
	\code{atoms.default} \tab Default method for atoms, \cr
	\code{months.timeDate} \tab Extract months atom from a 'timeDate' object. }

}


\usage{
\method{[}{timeDate}(x, \dots, drop = TRUE)
\method{+}{timeDate}(e1, e2)
\method{-}{timeDate}(e1, e2)
\method{Ops}{timeDate}(e1, e2)
\method{diff}{timeDate}(x, lag = 1, differences = 1, \dots)
difftimeDate(time1, time2, 
	units = c("auto", "secs", "mins", "hours", "days", "weeks"))

\method{c}{timeDate}(\dots, recursive = FALSE)
\method{rep}{timeDate}(x, times, \dots)
\method{start}{timeDate}(x, \dots)
\method{end}{timeDate}(x, \dots)
\method{modify}{timeDate}(x, method = c("sort", "round", "trunc", 
	units = c("secs", "mins", "hours", "days"))
\method{rev}{timeDate}(x)

\method{as.character}{timeDate}(x, \dots)
\method{as.data.frame}{timeDate}(x, \dots)
\method{as.POSIXct}{timeDate}(x, tz = "")
\method{julian}{timeDate}(x, \dots)
\method{atoms}{timeDate}(x, \dots)
\method{months}{timeDate}(x, abbreviate = NULL)
}


\arguments{
  
    \item{abbreviate}{
    	[months] - \cr
    	currently not used.
    	}
    \item{differences}{
    	[lag] - \cr
    	an integer indicating the order of the difference.
    	}
    \item{e1, e2}{
    	["+"][["-"]["Ops"] - \cr
    	usually objects of class \code{timeDate}, in the case of
    	addition and subtraction \code{e2} may be of class
    	\code{numeric}.
    	}
    \item{lag}{
    	[lag] - \cr
    	an integer indicating which lag to use.  
		}
	\item{recursive}{
		[c] - \cr
		a logical. If recursive  is set to \code{TRUE}, the function 
		recursively descends through lists combining all their elements 
		into a vector. 
		}
    \item{time1, time2}{
    	[difftime] - \cr
    	two objects objects of class \code{timeDate}.
    	}
    \item{times}{
    	[rep] - \cr
    	an optional non-negative integer. A vector giving the number of 
    	times to repeat each element if of length \code{length(x)}, or 
    	to repeat the whole vector if of length \code{1}.
    	}
    \item{tz}{
        inputs the time zone to POSIX objects, i.e. the time zone, 
        \code{zone}, or financial center string, \code{FinCenter}, 
        as used by \code{timeDate} objects.
        }
    \item{units}{
    	a character string denoting the date/time units in which the 
    	results are desired.
    	}
    \item{x}{
    	an object of class \code{timeDate}.
    	} 
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{"["}
    \cr
    returns a subset from a \code{timeDate} object.
    \cr
    
    \code{"+"}\cr
    \code{"-"}
    \cr
    the plus operator \code{"+"} performs arithmetic "+" operation on 
    \code{timeDate} objects, and 
    the minus operator \code{"-"} returns a \code{difftime} object if 
    both arguments \code{e1} and \code{e2} are \code{timeDate} objects, 
    or returns a \code{timeDate} object \code{e2} seconds earlier than 
    \code{e1}.
    \cr
    
    \code{Ops.timeDate}
    \cr
    returns the \code{Ops} grouped object.
    \cr
    
    \code{diff}\cr
    \code{difftimeDate}
    \cr
    For the first function, \code{diff.timeDate}, if \code{x} is a vector 
    of length \code{n} and \code{differences=1}, then the computed result 
    is equal to the successive differences \code{x[(1+lag):n] - x[1:(n-lag)]}. 
    If \code{difference} is larger than one this algorithm is applied
    recursively to \code{x}. Note that the returned value is a vector 
    which is shorter than \code{x}.
    The second function, \code{difftimeDate}, takes a difference of two 
    \code{timeDate} objects and returns an object of class \code{difftime} 
    with an attribute indicating the units.
    \cr
    
    \code{c}\cr
    \code{rep}
    \cr
    \code{c} returns all its arguments to be coerced to a \code{timeDate} 
    object which is the type of the returned value, and 
    \code{rep} returns a vector of repeated elements belonging to the 
    same class as \code{x}.
    \cr
    
    \code{start}\cr
    \code{end}
    \cr
    return from \code{x} the earliest or latest entry as an object of 
    class \code{timeDate}, respectively.
    \cr
    
    \code{modify}\cr
    \code{rev}
    \cr
    \code{modify} returns \code{x} as a sorted, rounded or truncated 
    object of the same class, depending on the \code{method} selection, 
    and
    \code{rev} returns \code{x} as a \code{timeDate} object in reversed 
    order.
    \cr
    
    
    \code{as.character}\cr
    \code{as.data.frame}
    \cr
    return a \code{timeDate} object trnasformed into a character or a
    data frame formatted object.
    \cr
 
       
    \code{as.POSIXct}
    \cr
    return a \code{timeDate} object trnasformed into a POSIX type
    formatted object.
    \cr
    

    \code{julian}
    \cr
    return a \code{timeDate} object as a Julian count.
    \cr
    
    \code{atoms}\cr
    \code{months.timeDate}
    \cr
    extrac from a \code{timeDate} object the calendar atoms, i.e,
    the year, month, day, and optionally hour, minute and second.
    \cr
    
    
}


\details{

    \bold{Mathematical Operations:}
    \cr\cr
    This is a collection of S3 methods for objects of class \code{timeDate}
    to perform mathematical operations. Included are methods to extracts 
    or replace subsets from \code{timeDate} objects, to perform arithmetic 
    \code{"+"} and \code{"-"} operations, to group 'Ops' generic functions, 
    to return suitably lagged and iterated differences, to return differences 
    of two \code{timeDate} objects, to concatenate objects, to replicate 
    objects, to rounds objects, to truncates objects, to extract the first 
    or last entry of a vector, to sort the objects of the elements of a 
    date/time vector, and to revert \code{timeDate} vector objects.
    \cr
    
    \bold{Transformation of Objects:}
    \cr\cr
    This is a collection of S3 methods for objects of class \code{timeDate}
    to transform those objects between different representations.
    Included are methods to transform \code{timeDate} objects to character 
    strings, to data frames, to POSIXct or POSIXlt objects, to Julian
    counts, to extract date/time atoms from calendar dates, and to 
    extract the months atom from a \code{timeDate} object.
    \cr

}


\note{

    These functions were written for Rmetrics users using R and Rmetrics
    under Microsoft's Windows XP operating system where timze zones, 
    daylight saving times and holiday calendars are insuffeciently 
    supported. The functions are untested for other system environments, 
    but may be used.
    
    Please, also be aware of the notes remarked in the help page describing
    the \code{timeDate} class.
    
    IT IS IMPORTANT THAT YOU SET YOUR ENVIRONMENT VARIABLE TZ TO 'GMT',
    OTHERWISE SOME OF THE 'TIMEDATE' FUNCTIONS WOULD NOT PROPERLY WORK! 
   
}


\references{

Bateman R.,  (2000);
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


\examples{
## c -
   xmpBasics("\nStart: Create Character Vectors > ") 
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   dts; tms
   
## "+/-" - 
   xmpBasics("\nStart: Add One Day to a Given timeDate Object > ")
   GMT = timeDate(dts, FinCenter = "GMT")
   ZUR = timeDate(dts, FinCenter = "Europe/Zurich")
   GMT + 24*3600
   ZUR[2] - ZUR[1]
  
## "[" - 
   xmpBasics("\nNext: Subsets from and Lops for timeDate Objects > ")
   GMT[GMT < GMT[2]]
   ZUR[ZUR < ZUR[3]] == ZUR[1:3]

## diff - 
   xmpBasics("\nNext: Suitably Lagged and Iterated Differences > ")
   diff(GMT)
   diff(GMT, lag = 2)
   diff(GMT, lag = 1, diff = 2)
   difftimeDate(GMT[1:2], GMT[-(1:2)])
   
## c | rep - 
   xmpBasics("\nNext: Concatenate and Replicate timeDate Objects > ") 
   c(GMT[1:2], ZUR[1:2])
   c(ZUR[1:2], GMT[1:2])
   rep(ZUR[2], times = 3)
   rep(ZUR[2:3], times = 2) 

## round | truncate -
   xmpBasics("\nNext: Round and Truncate timeDate Objects > ")
   modify(GMT, "round", "days")
   modify(ZUR, "round", "days")
   modify(GMT, "trunc", "days")
   modify(ZUR, "trunc", "days")

## start | end | sort - 
   xmpBasics("\nNext: Extract First/Last, Sort timeDate Objects > ")
   c(start(ZUR), end(ZUR))
   modify(ZUR, "sort") 

## as -
   xmpBasics("\nNext: Convert timeDate Objects to Other Objects > ")
   as.character(ZUR)
   as.data.frame(ZUR)
   as.POSIXct(ZUR)
   
## julian - 
   xmpBasics("\nNext: Julian Time in Days Since 1970-01-01 > ")
   julian(ZUR)  
   as.integer(julian(ZUR))
   julian(ZUR, "days")  
   as.integer(julian(ZUR, "days"))
    
## atoms - 
   xmpBasics("\nNext: Atoms from a timeDate Object > ")
   atoms(ZUR)
   atoms(ZUR)[,3]
   atoms(ZUR)[, "d"]
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

