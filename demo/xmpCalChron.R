
#
# Example:
#	Learn to manage chronological objects from the 'chron' package.
#
# Description:
#	This is an excourse to the chronological objects in R's chron package. 
# 	The print methods for "dates" and "chron" objects are patched for 
#   a proper printing in ISO-8601 formats - the usage is unchanged. 
#   Furthermore, some, maybe useful, functions and methods have been 
#   added to generate sequences by quarters, to convert chron's objects 
#   to julian day counts, to find starting/ending dates, and to extract 
#   time and date "atoms" in a Splus like manner, and to extract day,
#   month, quarter and year atoms as integers. The latter are methods,
#   similar functionality can be created for other time/date objects
#   than chron's. In detail we will cover the following topics:
#
#		1  Introduction - Requirements
#       2  Generation of Objects
# 	  	3  Representation of Objects
# 	  	4  Mathematical Operations
#     	5  Object Conversions
# 	  	6  Object Transformations
#
# Author:
#	(C) These examples were written by Diethelm Wuertz, GPL.
#
# Notes:
#   These examples and exercise require R's chron package. Note that
#	the chron package is not part of this contribution!
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


################################################################################
# 1 Introduction - Requirements

	
    # Don't forget to load the chron package!
	loaded = require(chron)
	if (!loaded) {
		stop("\nLibrary "chron" required!")
	

################################################################################   
# 2 Generation of Objects
   

   # Controls:
   # The following controls the behavior when faced with 2-digit years.
   # * To have 2-digit years actually refer to the first century
   #   options(chron.year.abb = FALSE)
   # * To flag all 2-digit years as error:
   #   options(chron.year.abb = TRUE, chron.year.expand = "year.strict")
   # * To allow 2-digit year abbreviations and guess(?) actual year:
   #   options(chron.year.abb = TRUE, chron.year.expand = "year.expand")
   options(chron.year.abb = TRUE, chron.year.expand = "year.expand")
   ###
   
 
   # Create Objects:
   # USE: times(x, ...)
   #      dates(x, ...)
   #      chron(dates., times., format, out.format, origin.)   
   # "times", "dates", "chron" - Create Chronological Objects from Strings:
   # Create times and dates Vectors:
   dts = c("09/28/89", "09/15/01", "08/30/04", "02/09/90")
   tms = c("23:12:55", "10:34:02", "08:30:00", "11:18:23")
   # Create the Objects:
   Times = times(tms)
   Times
   # [1] 23:12:55 10:34:02 08:30:00 11:18:23
   Dates = dates(dts)
   Dates
   # [1] 09/28/89 09/15/01 08/30/04 02/09/90
   Chron = chron(dts, tms)
   Chron
   # [1] (09/28/89 23:12:55) (09/15/01 10:34:02) 
   ###
   
   
   # 'class' - Inspect Class Properties of Objects:
   class(Times) 
   # [1] "times"
   unclass(Times)
   # [1] 0.9673032 0.4403009 0.3541667 0.4710995
   # attr(,"format")
   # [1] "h:m:s"
   class(Dates) 
   # [1] "dates" "times"  
   unclass(Dates) 
   # [1]  7210 11580 12660  7344
   # attr(,"format")
   # [1] "m/d/y"
   # attr(,"origin")
   # month   day  year 
   #     1     1  1970
   class(Chron)
   # [1] "chron" "dates" "times"
   unclass(Chron)
   # [1]  7210.967 11580.440 12660.354  7344.471
   #    attr(,"format")
   #   dates   times 
   # "m/d/y" "h:m:s" 
   # attr(,"origin")
   # month   day  year 
   #     1     1  1970
   ###
    
   
   # 'seq' - Create Sequences:
   # USE: seq.dates(from, to, by = "days", length., ...) 
   #        from/to are the starting/ending dates, they  can be a chron 
   #        or dates object, a character string, or a Julian date.  
   # Now generate regularly spaced sequences:
   # by - either a numeric value or one of the valid strings 
   # 'days', 'weeks', 'months', or 'years'.
   seq(min(Times), max(Times), by = 3/24) 
   seq(min(Dates), sort(Dates)[2], by = "months") 
   seq(min(Chron), min(Chron) + 4*366, by = "years")
   seq(max(Dates), length = 4) 
   # Create an end-of-month sequence: 
   seq(dates("02/28/92"), by = "months", length = 6) 
   ###
   
   
   # 'seq' - Quarters Patch:
   # Synonyme for seq.dates with SPlus like arguments (chronADDON)
   # USE: seq.chron(from, to, by = "days", length.out = NULL, k.by = 1, ...) 
   # seq(min(Dates), by = "quarters", length.out  = 6) 
   ###
   
   
################################################################################   
# 3 Representation of Objects
   

   # "format" - Convert to character, internal functions 
   # A vector or list specifying the input format of the input. The format 
   # can be either strings specifying one of the recognized formats below 
   # or a list of user-supplied functions to convert dates from character 
   # into Julian dates and vice versa. 
   # * The dates format can be any permutation of the characters "d", "m", 
   #   or "y" delimited by a separator (possibly null), e.g., "m/d/y", "d-m-y", 
   #   "ymd", are all valid; the format can also be permutations of the words 
   #   "day", "month" and "year" (with non-null separator), which produces 
   #   the month name, e.g., "month day year" produces "April 20 1992", 
   #   "day mon year" produces "20 Apr 1992". 
   # * The times format can be any permutation of "h", "m", and "s" separated 
   #   by any one non-special character. The default is "h:m:s".  
   # USE: format.times(x, format. = "h:m:s", simplify = FALSE, ...)  
   #      format.dates(x, format = "m/d/y", origin., simplify = FALSE, ...)
   #      format.chron(x, format = att$format, origin. = att$origin, 
   #        sep = " ", simplify, enclosed = c("(", ")"), ...) 
   ###
   
  
   # "print" - Printing Chronological Objects:
   print(Times); print(Dates); print(Chron)
   # [1] 23:12:55 10:34:02 08:30:00 11:18:23
   # [1] 09/28/79 09/15/91 08/30/94 02/09/80
   # [1] (09/28/79 23:12:55) (09/15/91 10:34:02) (08/30/94 08:30:00) ...
   # Simplify - Suppress Century:
   print(Dates, simplify = TRUE)
   # R: [1] 09/79 09/91 08/94 02/80
   ###
    
   
   # "dates" - ISO-8601 Formatting Chronological Objects:
   d1 = dates(Dates, format = "d/m/y", out = "yy-m-dd") 
   d2 = dates(Dates, format = "d/m/y", out = "yy-mon-dd") 
   d3 = dates(Dates, format = "d/m/y", out = "y-m-d") 
   d4 = dates(Dates, format = "d/m/y", out = "y-mon-d") 
   d5 = dates(Dates, format = "d/m/y", out = "yy-m-d")
   # Output - print unpatched
   # d1; d2; d3; d4; d5
   # [1] 1989-Sep-28  2001-Sep-15  2004-Aug-30  1990-Feb-09
   # [1] 1989-Sep-28  2001-Sep-15  2004-Aug-30  1990-Feb-09
   # [1] 89-09-28     01-09-15     04-08-30     90-02-09
   # [1] 1989-Sep-28  2001-Sep-15  2004-Aug-30  1990-Feb-09
   # [1] 1989-Sep-28  2001-Sep-15  2004-Aug-30  1990-Feb-09
   # Sorry, no way to produce CCYY-MM-DD ISO-8601 formatted dates ...
   # PRINT PATCHE:
   #   We overwrite print.dates ...
   print.dates =
   function(x, digits = NULL, quote = FALSE, prefix = "", simplify, ...) {
     # Print Dates:
     if (!as.logical(length(x))) {
     cat("dates(0)\n")
     return(invisible(x)) }
     if (missing(simplify) && is.null(simplify <- getOption("chron.simplify")))
     simplify <- FALSE
     # Patch - A quick and dirty hack
     save.month.abb <<- month.abb
     m = regexpr("mon", attributes(x)$format) < 0
     if (m) month.abb <<- c(paste("0", 1:9, sep = ""), "10", "11", "12")
     formatted = as.character(format.dates(x, simplify = simplify))
     print(formatted, quote = quote)
     month.abb <<- save.month.abb
     # Return Value:
     invisible(x) }
   # Now print again ...   
   d1; d2; d3; d4; d5
   # R: [1] 1989-09-28   2001-09-15   2004-08-30   1990-02-09
   #    [1] 1989-Sep-28  2001-Sep-15  2004-Aug-30  1990-Feb-09
   #    [1] 89-09-28     01-09-15     04-08-30     90-02-09
   #    [1] 1989-Sep-28  2001-Sep-15  2004-Aug-30  1990-Feb-09
   #    [1] 1989-09-28   2001-09-15   2004-08-30   1990-02-09 
   ###
 
       
################################################################################   
# 4 Mathematical Operations
 
     
   # "[" - Extracts or replaces subsets from objects
   Times[2:3]; Times[-(1:2)]
   Dates[2:3]; Dates[-(1:2)]
   Chron[2:3]; Chron[-(1:2)]
   ###
   
   
   # "+" - Performs arithmetic + operation on 'chron' objects
   # Add 5 hours, 5 days, and 5 1/2 days:
   times1 = times("00:00:00"); times2 = times1 + 5/24; times2
   dates1 = dates("1/1/00"); dates2 = dates1 + 5; dates2
   chron1 = chron("1/1/00", "00:00:00"); chron2 = chron1 + 5.5; chron2
   ###
  
     
   # "-" - Performs arithmetic - operation on 'chron' objects
   # Subtract 5 hours, 5 days, and 5 1/2 days:
   times1 = times("20:00:00"); times2 = times1 - 5/24; times2
   dates1 = dates("1/31/00"); dates2 = dates1 - 5; dates2
   chron1 = chron("1/1/00", "20:00:00"); chron2 = chron1 - 5.5; chron2
   ###
   
   
   # "-" - Performs arithmetic - operation on 'chron' objects
   # "time1-time2" - Compute Differences:
   # USE: diff(x, lag = 1, differences = 1, ...) 
   Times[2] - Times[1]; diff(Times)
   Dates[2] - Dates[1]; diff(Dates)
   Chron[2] - Chron[1]; diff(Chron)
   # Returns:
   # Time in days: 
   # [1] 4369.473
   # Time in days:
   # [1]  4369.473  1079.914 -5315.883
   ###

   
   # Group 'Ops' generic functions for 'chron' objects
   # time1 "lops" time 2
   # "=="  "!="  "<"  "<="  ">"  ">=" - Perform Logical Operations: 
   Dates[Dates > dates("01/01/00")]
   # Returns:
   # [1] 09/15/01 08/30/04
   Chron[Chron > chron("01/01/00", "00:00:00")]
   # Returns:
   # [1] (09/15/01 10:34:02) (08/30/04 08:30:00)
   # More Logical Operations ...
   chron1 == (chron2 - 5.5)
   chron1 != chron2
   chron1 <= chron2
   ###

   
   # "diff.*" - Returns suitably lagged and iterated differences
   ###
   
   
   # "difftime" - Takes a difference of two 'chron' objects
   ###
   
   
   # "c" - Concatenates objects of class 'chron'
   c(Times[1], Times[3])
   c(Dates[1], Dates[3])
   c(Chron[1], Chron[3])
   c(Times[1], Dates[1], Chron[1]) 
   ###
   
   
   # "rep" - Repeats objects of class 'chron'
   rep(Times, each = 2)
   rep(Dates, each = 2)
   rep(Times, each = 2)
   ###
   
   
   # 'trunc' - Truncate and Round Chronological Objects:
   # Truncate time from Objects:  
   Chron[1:2]
   # [1] (09/28/89 23:12:55) (09/15/01 10:34:02)
   trunc(Chron[1:2])
   # [1] 09/28/89 09/15/01
   ###
   
   
   # 'round' - Rounds objects of class 'chron'
   # Not available, add 12 hours and truncate ...
   trunc(Chron[1:2] + 0.5)
   # [1] 09/29/89 09/15/01
   ###
   
      
   # 'min', 'max' - Extracts the first/last Chronological Object:
   range = c(min(Dates), max(Dates))
   range
   # [1] 09/28/89 08/30/04
   ###
   
   
   # 'sort' - Sort and Revert Chronological Objects:
   sort(c(Dates[4], Dates[1:3]))
   # [1] 09/28/89 02/09/90 09/15/01 08/30/04
   reverted = sort(Dates)[length(Dates):1]
   reverted
   # [1] 08/30/04 09/15/01 02/09/90 09/28/89
   ###
   
   
################################################################################   
# 5 Object Conversions
   
  
   # "as.character" - Returns Object as Character Vector:
   as.character(Times) 
   # [1] "23:12:55" "10:34:02" "08:30:00" "11:18:23"
   as.character(Dates)
   # [1] "09/28/89" "09/15/01" "08/30/04" "02/09/90"
   as.character(Chron)
   # [1] "(09/28/89 23:12:55)" "(09/15/01 10:34:02)" "(08/30/04 08:30:00)" ...
   ###
   
   
   # "as.data.frame" - Returns Object as Data Frame:
   as.data.frame(Times[1:2])
   #      Times
   # 1 23:12:55
   # 2 10:34:02
   as.data.frame(Dates[1:2])
   #      Dates
   # 1 09/28/89
   # 2 09/15/01
   as.data.frame(Chron[1:2])
   #                Chron
   # 1 (09/28/89 23:12:55)
   # 2 (09/15/01 10:34:02)
   ###
   
   
   # "as.POSIXct" - Returns a 'chron' object as 'POSIXct' Object
   # "as.POSIXlt" - Returns a 'chron' object as 'POSIXlt' Object
   # as.POSIXct(Times); as.POSIXlt(Times) - Note, can't work for times ...
   as.POSIXct(Dates)
   # [1] "1989-09-28 02:00:00 W. Europe Daylight Time"
   as.POSIXlt(Dates)
   # [2] "2001-09-15 02:00:00 W. Europe Daylight Time" ...
   as.POSIXct(Chron)
   # [1] "1989-09-29 01:12:55 W. Europe Daylight Time"
   as.POSIXlt(Chron)
   # [2] "2001-09-15 12:34:02 W. Europe Daylight Time" ...
   # Both yield the same output.
   ###

   
################################################################################   
# 6 Object Transformations
  
   
   # "julian" - Extracts Julian time in days since 1970-01-01
   # USE: julian.default 
   #      julian.dates  (part of chronADDON)
   #      julian.chron  (part of chronADDON)
   # "origin."  
   #   A vector specifying the date with respect to which Julian dates  
   #   are computed. Default is c(month = 1, day = 1, year = 1970); you  
   #   may set the option chron.origin to specify your own default, e.g., 
   #   options(chron.origin = c(month=1, day=1, year=1990)). 
   
   # Is 1970 the default Julian origin?
   julian(1, 1, 1970) 
   julian(as.POSIXlt(chron("1/1/1970", "00:00:00")))
   julian(as.POSIXlt(dates("1/1/1970")))
   # Returns for all three:
   # [1] 0
   # Origin:
   origin(Dates); origin(Chron)
   # Returns for both:
   # month   day  year 
   #     1     1  1970 
   ###
   
   
   # "month.day.year" - Extract month, day and year 'Atoms':
   # USE: month.day.year(jul, origin.)
   MDY = month.day.year(jul = julian(1, 1, 2000)); MDY
   MDY = month.day.year(jul = as.integer(dates("1/1/00"))); MDY
   # Returns for both the list:
   # $month
   # [1] 1
   # $day
   # [1] 1
   # $year
   # [1] 2000
   # Print the result in form of a data frame:
   data.frame(MDY)
   # Returns:
   #   month day year
   # 1     1   1 2000
   # Works also for Vectors:
   mdy = month.day.year(jul = as.integer(julian(as.POSIXlt(Chron))))
   mdy
   mdy = month.day.year(jul = as.integer(julian(as.POSIXlt(Dates))))
   mdy
   ###
   
   
   # "day.of.week" - Day of Week Function:
   day.of.week(month = mdy$month, day= mdy$day, year=mdy$year)
   # November 12, 98, was a Wednesday.
   day.of.week(month = 11, day = 12, year = 1998)
   ###
   
    
   # "leap.year" - Leap Year Function:
   # USE: leap.year(y)
   # Which of the years are leap years?
   leap.year(c(1970, 2000))
   leap.year(Dates); leap.year(Chron)
   # [1] FALSE FALSE  TRUE FALSE ...
   ###
	
   
   # "weekdays" - Extract Weekdays:
   weekdays(Dates); weekdays(Chron)
   # [1] Thu Sat Mon Fri
   # Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat
   # [1] Thu Sat Mon Fri
   # Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat
   ###
   
   
   # "seconds", ... , "years" - Extract Atoms:
   x = Times
   seconds(x); minutes(x); hours(x)                      
   x = Dates
   seconds(x); minutes(x); hours(x)          
   days(x); months(x); quarters(x); years(x) # includes levels
   x = Chron
   seconds(x); minutes(x); hours(x)          
   days(x); months(x); quarters(x); years(x) # includes levels
   ###
   
}


################################################################################

