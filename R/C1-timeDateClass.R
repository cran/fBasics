
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


# Copyrights (C)
# for this R-port: 
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   R: see R's copyright and license file
#   date: Terry Therneau <therneau@mayo.edu>
#     R port by Th. Lumley <thomas@biostat.washington.edu>  K. Halvorsen 
#       <khal@alumni.uv.es>, and Kurt Hornik <Kurt.Hornik@R-project.org>
#   ts: Collected by Brian Ripley. See SOURCES
#   tseries: Compiled by Adrian Trapletti <a.trapletti@bluewin.ch>
# for ical:
#   libical: Libical is an Open Source implementation of the IETF's 
#     iCalendar Calendaring and Scheduling protocols. (RFC 2445, 2446, 
#     and 2447). It parses iCal components and provides a C API for 
#     manipulating the component properties, parameters, and subcomponents.
#   Olsen's VTIMEZONE: These data files are released under the GNU 
#     General Public License, in keeping with the license options of 
#     libical. 
# for the holiday database:
#   holiday information collected from the internet and governmental 
#   sources obtained from a few dozens of websites


################################################################################
# FUNCTION:            FINANCIAL CENTERS:
#  rulesFinCenter       Returns DST rules for a financial center
#  listFinCenter        Lists all supported financial centers
# FUNCTION:            GENERATION OF TIMEDATE OBJECTS:
#  setclass             S4: Class representation for timeDate objects
#  timeDate             S4: Creates a 'timeDate' object from a character vector
#  timeCalendar         S4: Creates a 'timeDate' object from calendar atoms
#  timeSequence         S4: Creates a regularly spaced 'timeDate' objec
#  Sys.timeDate         Returns system time as an object of class 'timeDate'   
# FUNCTION:            SPECIAL MONTHLY SEQUENCES:
#  timeLastDayInMonth   Computes the last day in a given month and year
#  timeNdayOnOrAfter    Computes date in month that is a n-day ON OR AFTER date
#  timeNdayOnOrBefore   Computes date in month that is a n-day ON OR BEFORE date
#  timeNthNdayInMonth   Computes n-th ocurrance of a n-day in year/month
#  timeLastNdayInMonth  Computes the last n-day in year/month
# FUNCTION:            TEST AND REPRESENTATION OF OBJECTS:
#  is.timeDate          Checks if the object is of class 'timeDate'
#  print.timeDate       Prints 'timeDate' including 'FinCenter' and 'Data' Slot
#  summary.timeDate     Summarizes details of a 'timeDate' object
#  format.timeDate      Formats 'timeDate' as ISO conform character string
################################################################################

# IMPORTANT FOR WINDOWS USERS:

#   Set your timezone environment variable to TZ = GMT !!!


# INTRODUCTION:

#   For the management of chronological objects under R three concepts 
#   are available: The first is the implementation of date and time in 
#   R’s "chron" package neglecting locals, time zones and day light saving 
#   times which are not really needed for economic time series. The second 
#   approach, available in R’s base package implements the POSIX standard 
#   to date and time objects, named "POSIXt". Unfortunately, the 
#   representation of these objects is operating system dependent and 
#   especially under MS Windows several problems appear in the management 
#   of time zones and day light saving times. Here we present a solution
#   to overcome these difficulties with POSIX objects and introduce a 
#   new S4 class of 'timeDate' objects which allow for powerful methods 
#   to represent dates and times in different financial centers around 
#   the world. Many of the basic functionalities of these objects are in 
#   common with SPlus’ 'timeDate' objects and thus many of the programs
#   written for FinMetrics can also be used within R's environment.

#   A major difference is the time zone concept which is replaced by the
#   "financial center" concept. Thus, rules for day light saving times, 
#   holiday calendars, interest rate conventions, and many other aspects
#   can be easily accessed when a financial center is named. So we can 
#   distinguish between Frankfurt and Zurich, which both belong to the 
#   same time zone, but differed in DST changes in the eighties and have
#   differentholiday calendars. Futhermore, since the underlying time 
#   refers to "GMT" and DST rules and all other information is available 
#   in local databases, we are sure, that R delivers with such a time/date 
#   concept on every computer independent of the implementation of the 
#   operating system in use, identical results. 

#   Another important feature of the "timeDate" concept used here is the
#   fact that we don't rely on American or European ways to write dates.
#   We use consequently the ISO-8601 standard for date and time notations.


################################################################################
# FINANCIAL CENTERS:
#   There are two functions concerned with the financial centers. The 
#   first lists the daylight saving rules for a selected financial
#   center, and the second lists all centers available in the database.
#   There is no dependency on the POSIX implementation of your operating
#   system because all time zone and day light saving time information
#   is stored locally in ASCII files. It is important to say, that
#   the "TZ" environment variable must set to "GMT" in your System
#   Environment that there are no conflicts with the POSIX time zone
#   management.


	
rulesFinCenter =
function(FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Show the day light saving rules for a financial center
    
    # Arguments:
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Internal Function for Conversion from Ical Tables:
    if (FALSE) {
	rulesFinCenter2 = 
	function(FinCenter = myFinCenter) {   
		# A function implemented by Diethelm Wuertz	
	    # Description:
	    #   Show the day light saving rules for a financial center	    
	    # Arguments:
	    #   FinCenter - a character string with the the location of the  
	    #       financial center named as "continent/city". 	    
	    # Value:
	    #   Returns a printed list of DST rules.	    
	    # Example:
	    #   > rulesFinCenter("Zurich")
	    #               ruleChanges offSet
	    #   1   1894-05-31 23:30:16   3600
	    #   2   1940-11-01 23:00:00   7200
	    #   3   1940-12-30 22:00:00   3600
	    #   5   1941-10-04 22:00:00   3600
	    #   6   1942-05-03 01:00:00   7200
	    #   7   1942-10-03 22:00:00   3600
	    #   8   1980-12-31 23:00:00   3600
	    #   9   1981-03-29 01:00:00   7200
	    #   ...	    
	    # Note:
	    #   Important, the "TZ" environment variable must set 
	    #   to "GMT" in your Windows Environment!	    
	    # Check Timezone:
	    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
	    if (FinCenter == "") FinCenter = "GMT"	    
	    # Read the Rules:
	    # Get IcalPath from .FirstLib
	    file = paste(IcalPath, FinCenter, sep = "")
	    zfile <- zip.file.extract(file, "Rdata.zip")
	    ical = read.table(zfile, skip = 2)	            
	    # GMT Offsets:
	    hm = as.integer(ical[,6])
	    sg = sign(hm)
	    hm = abs(hm)
	    h = floor(hm/100)
	    hms.off = sg * ( floor(hm/100)*3600 + (hm - 100*h)*60 + 0 )
	    hms.off	 
	    # When have the rules changed?
	    months.num = 1:12
	    names(months.num) = c(
	        "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
	        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	    Y = as.integer(ical[,4])
	    m = as.integer(months.num[as.character(ical[,3])])
	    d = as.integer(ical[,2])
	    CCYYMMDD = as.character(Y*10000+100*m+d)
	    hms = unlist(strsplit(as.character(ical[,5]), ":"))
	    hms = matrix(as.integer(hms), byrow=TRUE, ncol=3)
	    hms = 1000000 + 10000*hms[,1] + 100*hms[,2] + hms[,3]
	    hhmmss = substr(as.character(hms), 2, 7)
	    ruleChangesGMT = strptime(paste(CCYYMMDD, hhmmss), "%Y%m%d %H%M%S")
	    attr(ruleChangesGMT, "tzone") <- "GMT"	    
	    # Return Value:
	    data.frame(ruleChanges = as.character(ruleChangesGMT), 
	    	offSet = hms.off) } 
	}
       
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    # if (FinCenter == "") FinCenter = "GMT"
	   
	City = strsplit(FinCenter, "/")[[1]][length(strsplit(FinCenter, "/")[[1]])]
	fun = match.fun(City)
	
	# Return Value:
	fun()
}  


# ------------------------------------------------------------------------------


listFinCenter = 
function(pattern = "*")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   List available Time Zones in Database
    
    # Arguments:
    #   pattern - a pattern character string which can be recognized
    #       by the 'grep' functs. Wild cards are allowed.
    
    # Value:
    #   Returns a printed list of financia centers. 
    
    # Example:
    #   > listFinCenter("Europe/*")
    #    [1] "Europe/Amsterdam"   "Europe/Andorra"     "Europe/Athens"     
    #    [4] "Europe/Belfast"     "Europe/Belgrade"    "Europe/Berlin"     
    #    [7] "Europe/Bratislava"  "Europe/Brussels"    "Europe/Bucharest"  
    #   [10] "Europe/Budapest"    "Europe/Chisinau"    "Europe/Copenhagen" 
    #   [13] "Europe/Dublin"      "Europe/Gibraltar"   "Europe/Helsinki"   
    #   [16] "Europe/Istanbul"    ...   
    
    # Note:
    #   The timezone database is required.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Load Database:
    # require(fBasics)
    data(timezones.db)
    tz = as.character(unclass(timezones.db)$TIMEZONES)
    
    # Financial Centers:
    result = as.character(tz[grep(pattern = pattern, x = tz)])
    
    # Return Value:
    result
}
    

################################################################################
# GENERATION OF TIME DATE OBJECTS:   
#   We have defined a 'timeDate' class which is in many aspects similar
#   to the S-Plus class with the same name, but has also some important
#   differeneces. The class has four Slots, the 'Data' slot which holds 
#   date and time as 'POSIXlt' objects, the 'Dim' slot which gives the
#   length of the object, the 'format' specification and 'FinCenter' the 
#   the name of the financial center. Three functions allow to cgenerate
#   date/time objects: 'timeDate' from character vectors, 'timeCalendar'
#   from date and time atoms, and 'timeSequence' from a sequence 
#   specification. Note, time zone transformation is easily handled by
#   by the 'timeDate' functions which can also take 'timeDate' and
#   'POSIXt' objects as inputs, while transforming them between financial
#   centers and/or time zones specified by the arguments 'zone' and
#   'FinCenter'. Finally the function 'Sys.timeDate' returns system
#   time in form of a 'timeDate' object.


require(methods)


setClass("timeDate", 
    # A class implemented by Diethelm Wuertz
    
    # Description:
    #   Class representatation for 'timeDate' Objects.
    
    # CLASS:
    
    representation(
        Data = "POSIXlt",
        Dim = "numeric",
        format = "character",
        FinCenter = "character"
    )    
)   
    
  
# ------------------------------------------------------------------------------
   

timeDate = 
function(charvec, format = NULL, zone = "GMT", FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a "timeDate' object from a character vector
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   format - the format specification of the input character 
    #       vector.
    #   zone - the time zone or financial center where the data were 
    #       recorded.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns a S4 object of class 'timeDate'.
    
    # Examples:
    #   timeDate("2004-01-01") 
    #   timeDate("2004-01-01 00:00:00")
    #   timeDate("20040101")     
    #   timeDate("20040101000000")
    #   timeDate("1/1/2004") # American format
    #   timeDate("2004-01-01", FinCenter = "GMT")   
    #   timeDate("20040101", FinCenter = "GMT") 
    #   td = timeDate("2004-01-01", FinCenter = "GMT"); timeDate(td)
    #   td = timeDate("20040101", FinCenter = "GMT"); timeDate(td)
     
    # FUNCTION:

    # Trace:
    trace = FALSE
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
        
    # If charvec is of type "timeDate" extract data slot:
    if (inherits(charvec, "sdate")) {
        charvec = as.character(charvec)
        format = "%Y%m%d"
        zone = FinCenter }
    if (inherits(charvec, "timeDate")) 
        charvec = charvec@Data   
    # If charvec is of type "POSIXt" convert to character string:
    if (inherits(charvec, "POSIXt")) 
        charvec = format(charvec, "%Y-%m-%d %H:%M:%S") 
    
    # Dimension
    Dim = length(charvec)
        
    # ISO Format:
    iso.format = "%Y-%m-%d %H:%M:%S"
    if (is.character(charvec) & is.null(format)) {
        nchar.iso = mean(nchar(charvec))
        if (nchar.iso == 10) format = "%Y-%m-%d" 
        if (nchar.iso == 19) format = "%Y-%m-%d %H:%M:%S"
        if (nchar.iso ==  8) format = "%Y%m%d"
        if (nchar.iso == 14) format = "%Y%m%d%H%M%S"
        if (regexpr("/", charvec[1])[[1]] > 0) format = "%m/%d/%Y" }
        
    # Convert "charvec" to standard ISO format:
    charvec = format(strptime(charvec, format), iso.format) 
    
    # Trace Input:
    if (trace) { cat("\nInput: "); print(recFinCenter); print(charvec) }
    # Financial Centers:
    recFinCenter = zone # Time zone where the data were recorded
    useFinCenter = FinCenter # Time zone where the data were used

    # Internal Function:
    formatFinCenter = 
    function(charvec, FinCenter, type = c("gmt2any", "any2gm")){    
        # Description:
        #   Transformation of timeDate character vectors between
        #   financial centers
        # Arguments:
        #   charvec - ISO character vector as "%Y-%m-%d %H:%M:%S"
        #   format - the character string with format specification
        #   FinCenter - the Financial center as "Continent/City"
        #   type - what to convert, either "gmt2any", or "any2gm"
        # Value:
        #   returns an ISO character vector as "%Y-%m-%d %H:%M:%S"
        # FUNCTION:
        # Convert what?
        type = type[1]
        signum = 0
        if (type == "gmt2any") signum = +1 
        if (type == "any2gmt") signum = -1      
        # Convert:
        if (FinCenter != "GMT") {
            center = rulesFinCenter(FinCenter)
            center1 = as.character(center[,1])
            center2 = as.character(center[,2])      
            charchanges = center1[!is.na(center1)]              
            o = order(c(charchanges, charvec))
            nRC = length(charchanges)
            nME = length(charvec)
            center2 = center2[!is.na(center1)] 
            ishms = c(as.integer(center2), rep(NA, length = nME)) 
            x = (1:(nRC+nME))[!is.na(ishms[o])]
            xout = (1:(nRC+nME))[is.na(ishms[o])]
            y = ishms[o][!is.na(ishms[o])] 
            offSets = approx(x = x, y = y , xout, method = "constant")$y    
            dt = strptime(charvec, "%Y-%m-%d %H:%M:%S")             
            ans = format(dt + signum * offSets, format="%Y-%m-%d %H:%M:%S") }
        else {
            ans = charvec }
        # Return Value:
        ans }
    
    # Convert:    
    DEBUG = FALSE
    if (recFinCenter == "GMT" && useFinCenter == "GMT") {       
        if (DEBUG) print("if - 1:")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec); cat("\n") }
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) }  
             
    if (recFinCenter == "GMT" && useFinCenter != "GMT") {
        if (DEBUG) print("if - 2:") 
        charvec = formatFinCenter(charvec, useFinCenter, type = "gmt2any")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec); cat("\n") }
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) }    
                
    if (recFinCenter != "GMT" && useFinCenter == "GMT") {
        if (DEBUG) print("if - 3:")
        charvec = formatFinCenter(charvec, recFinCenter, type = "any2gmt")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec); cat("\n") }
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) }      
                
    if (recFinCenter == useFinCenter) {     
        if (DEBUG) print("if - 4:")
        if (trace) { 
            cat("\nOutput: ")
            print(useFinCenter)
            print(charvec); cat("\n") }
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        return(new("timeDate", 
            Data = lt,
            Dim = as.integer(Dim),
            format = iso.format ,
            FinCenter = useFinCenter)) }    
            
    if (recFinCenter != useFinCenter) {
        if (DEBUG) print("if - 5:")
        charvec = formatFinCenter(charvec, recFinCenter, type = "any2gmt")
        charvec = formatFinCenter(charvec, useFinCenter, type = "gmt2any")
        if (trace) { 
            cat("\nOutput: ") 
            print(useFinCenter); 
            print(charvec); cat("\n") }
        lt = strptime(charvec, iso.format)
        timeTest = sum(lt$hour) + sum(lt$min) + sum(lt$sec) 
        if (timeTest == 0) iso.format = "%Y-%m-%d"
        return(new("timeDate", 
            Data = lt, 
            Dim = as.integer(Dim),
            format = iso.format,
            FinCenter = useFinCenter)) }    
            
    # Return Value:
    invisible()         
}


# ------------------------------------------------------------------------------


timeCalendar = 
function(y = currentYear, m = 1:12, d = NULL, h = NULL, min = NULL, 
s = NULL, FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a 'timeDate' object from calendar atoms
    
    # Arguments:
    #   y - calendar years (e.g. 1997), defaults are 1960. 
    #   m - calendar months (1-12), defaults are 1. 
    #   d - calendar days (1-31), defaults are 1. 
    #   h - hours of the days (0-23), defaults are 0. 
    #   min - minutes of the days (0-59), defaults are 0. 
    #   s - seconds of the days (0-59), defaults are 0. 
    #   FinCenter - a character sting with the the location of the  
    #       financial center named as "continent/city"  
    
    # Value:
    #   Returns a 'timeDate' object corresponding to the "atomic" 
    #   inputs. For the default arguments the first day in each 
    #   month of the current year will be returned.
    
    # Details:
    #   Creates a 'timeDate' object from date as month, day, year and
    #   time of day as hours, and minutes [seconds, milliseconds]
    
    # Note:
    #   The 'zone' where the data were recorded is fixed to myFincenter!
    #   The argument list has ISO-8601 ordering!
    #   ms - Milliseconds is not supported.
    
    # Example:
    #   x = timeCalendar(y = 2000, h = rep(16,12)) 
    #   x = timeCalendar(m = c(3,4,5), d = c(12,15,7), y = c(1998,1997,2004)) 
    #   x = timeCalendar(h = c(9,14), min = c(15,23)) 
  
    # FUNCTION:
    
    # Trace:
    trace = FALSE
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Check Input:
    len = c(length(m), length(d), length(y), length(h), length(min), length(s))
    data.len = max(len)
    if (data.len < 1) 
        stop("No arguments defined!")
    if (any((data.len %% len[len > 0]) != 0))
        stop("Arguments have incompatible lengths")
    
    # Make All Arguments the Same Length:
    if (len[1] == 0) m = 1
    if (len[2] == 0) d = 1
    if (len[3] == 0) y = 1960
    if (len[4] == 0) h = 0
    if (len[5] == 0) min = 0
    if (len[6] == 0) s = 0
    
    # Presettings:
    m = rep(m, length = data.len)
    d = rep(d, length = data.len)
    y = rep(y, length = data.len)
    h = rep(h, length = data.len)
    min = rep(min, length = data.len)
    s = rep(s, length = data.len)
    
    # Date-Time Strings:
    # Note Format is always of type  "%Y%m%d%H%M%S"  !
    
    CCYYMMDD = as.integer(y*10000 + m*100 + d)
    chardate = as.character(CCYYMMDD)
    hhmmss = as.integer(1000000 + h*10000 + min*100 + s)
    chartime = substr(as.character(hhmmss), 2, 7)
    charvec = paste(as.vector(chardate), as.vector(chartime), sep = "") 
    
    # Return Value:  
    timeDate(charvec = charvec, format = "%Y%m%d%H%M%S", 
        zone = FinCenter, FinCenter = FinCenter) 
}


# ------------------------------------------------------------------------------


timeSequence = 
function(from = "2004-01-01", to = format(Sys.time(), "%Y-%m-%d"), 
by = "day", length.out = NULL, format = "", FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a regularly spaced 'timeDate' object
    
    # Arguments:
    #   from - starting date. Required.
    #   to - end date. Optional. If supplied must be after from.
    #   by - a character string, containing one of "sec", "min",
    #       "hour", "day", "week", "month" or "year".
    #       This can optionally be preceded by an integer and a
    #       space, or followed by "s". 
    #   length.out - length.out integer, optional. Desired length  
    #       of the sequence, if specified "to" will be ignored.
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city".  
    
    # Value:
    #   Returns a 'timeDate' object corresponding to the "sequence" 
    #   specification. 
    
    # Note:
    #   The 'zone' where the data were recorded is fixed to myFincenter!
    
    # Example:
    #   x = timeSequence("2004-01-28", "2004-02-04", by = "day")
    #   x = timeSequence("2004-01-31", "2005-01-31", by = "month")
    #   x = timeSequence("2004-01-28", by = "day", length.out = 10)
    #   x = timeSequence("2004-01-31", by = "month", length.out = 12))   
    #   x = timeSequence("2004-01-28 18:00:00", "2004-01-29 06:00:00", 
    #       format = "%Y-%m-%d %H:%M:%S", by = "hour")
    #   x = timeSequence("2004-01-28 18:00:00", 
    #       format = "%Y-%m-%d %H:%M:%S", by = "hour", length.out = 10)
        
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Convert Quarters:
    if (by == "quarters") by = "3 months"
    
    # Auto-detect Input Format:
    format.from = format.to = format
    if (is.character(from) & is.null(format)) {
        nchar.iso = mean(nchar(from))
        if (nchar.iso == 10) format.from = "%Y-%m-%d" 
        if (nchar.iso == 19) format.from = "%Y-%m-%d %H:%M:%S"
        if (nchar.iso ==  8) format.from = "%Y%m%d"
        if (nchar.iso == 14) format.from = "%Y%m%d%H%M%S"
        if (regexpr("/", from[1])[[1]] > 0) format.from = "%m/%d/%Y" }
    if (is.character(to) & is.null(format)) {
        nchar.iso = mean(nchar(to))
        if (nchar.iso == 10) format.to = "%Y-%m-%d" 
        if (nchar.iso == 19) format.to = "%Y-%m-%d %H:%M:%S"
        if (nchar.iso ==  8) format.to = "%Y%m%d"
        if (nchar.iso == 14) format.to = "%Y%m%d%H%M%S"
        if (regexpr("/", to[1])[[1]] > 0) format.to = "%m/%d/%Y" }
    format = format.from
    if (format != format.to)
        stop ("Args from and to must have the same format specification.")
    
    # Create Charvector:  
    from = strptime(as.character(from), format = format) 
    iso.format = "%Y-%m-%d %H:%M:%S"
    if (is.null(length.out)) {
        # The start "from" and end date "to" must be specified!
        to = strptime(as.character(to), format = format)
        charvec = format(seq.POSIXt(from = from, 
            to = to, by = by), iso.format) }
    else  {
        # The end date is missing and has to be specified
        charvec = format(seq.POSIXt(from = from, 
            by = by, length.out = length.out), iso.format) }
            
    # Create timeDate Object:  
    ans = timeDate(charvec = charvec, format = NULL, 
        zone = FinCenter, FinCenter = FinCenter) 
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


Sys.timeDate =
function(FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns system time as an object of class 'timeDate'
    
    # Arguments:
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city"   
    
    # Value:
    #   Returns the system time as an object of class 'timeDate'.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Get System Time:
    ans = timeDate(as.character(Sys.time()), zone = "GMT", 
        FinCenter = FinCenter)
        
    # Return Value:
    ans
    
}

 
################################################################################
# SPECIAL MONTHLY TIME DATE SEQUENCES:
#   We have implemented five functions to generate special monthly 
#   sequences. These are functions to compute the last day in a given 
#   month and year, to compute the dates in amonth that is a n-day 
#   ON OR AFTER a given date, to compute the dates in a month that 
#   is a n-day ON OR BEFORE a specified date, to compute the n-th 
#   ocurrances of a n-day for a specified year/month vectors, and 
#   finally to compute the last n-day for a specified year/month
#   value or vector.


timeLastDayInMonth = 
function(charvec, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last day in a given month and year
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the last day in a given month and year as a
    #   'timeDate' object.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Last day of month:
    last.day = c(31,28,31, 30,31,30, 31,31,30, 31,30,31)
    lt = strptime(charvec, format)
    y = 1900 + lt$year
    leap.year = (y%%4 == 0 & (y%%100 != 0 | y%%400 == 0))
    leap.day = as.integer(leap.year)*as.integer(lt$mon == 1)
    lt$mday = last.day[1 + lt$mon] + leap.day
    
    # Return Value:
    timeDate(lt, format = "%Y-%m-%d", zone = FinCenter, 
        FinCenter = FinCenter)
}

    
# ------------------------------------------------------------------------------


timeNdayOnOrAfter = 
function(charvec, nday = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes date in month that is a n-day ON OR AFTER 
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the date in month that is a n-day ON OR AFTER as
    #   a 'timeDate' object.
    
    # Details:
    #   nday = 1 is a Monday
    
    # Example: 
    #   What date has the first Monday on or after March 15, 1986?
    #   OnOrAfter("1986-03-15", 1)
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # timeDate:
    lt = strptime(charvec, format)
    
    # On or after:
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) + (nday-lt$wday)%%7)
    class(ct) = "POSIXct"
    
    # Return Value:
    timeDate(format(ct), format = format, zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeNdayOnOrBefore = 
function(charvec, nday = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes date in month that is a n-day ON OR BEFORE 

    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the date in month that is a n-day ON OR BEFORE
    #   as a 'timeDate' object.
    
    # Example: 
    #   What date has Friday on or before April 22, 1977?
    
    # FUNCTION: 
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # timeDate:
    lt = strptime(charvec, format)
    
    # On or after:
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) - (-(nday-lt$wday))%%7)
    class(ct) = "POSIXct"
    
    # Return Value:
    timeDate(format(ct), format = format, zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeNthNdayInMonth = 
function(charvec, nday = 1, nth = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes "nth" ocurrance of a "nday" (nth = 1,...,5) 
    #   in "year,month"
 
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   nth - an integer vector numbering the n-th occurence.
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the "nth" ocurrance of a "nday" (nth = 1,...,5) 
    #   in "year,month" as a 'timeDate' object.
    
    # Example: 
    #   What date is the second Monday in April 2004?
    
    # FUNCTION: 
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # timeDate:
    lt = strptime(charvec, format)
    
    # On or after:
    lt1 = lt
    lt1$mday = 1
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) + (nth-1)*7 + 
        (nday-lt1$wday)%%7)
    class(ct) = "POSIXct"

    # Return Value:
    timeDate(format(ct), format = format, zone = FinCenter, 
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeLastNdayInMonth = 
function(charvec, nday = 1, format = "%Y-%m-%d", FinCenter = "GMT")
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the last "nday" in "year/month"
    
    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character 
    #       vector.
    #   FinCenter - a character string with the the location of the  
    #       financial center named as "continent/city". 
    
    # Value:
    #   Returns the last "nday" in "year/month" as a 'timeDate' 
    #   object.
    
    # Example: 
    #   What date has the last Monday in May, 1996?
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    if (FinCenter == "") FinCenter = "GMT"
    
    # Last Day:
    last.day = c(31,28,31, 30,31,30, 31,31,30, 31,30,31)
    lt = strptime(charvec, format)
    y = 1900 + lt$year
    leap.year = (y%%4 == 0 & (y%%100 != 0 | y%%400 == 0))
    leap.day = as.integer(leap.year)*as.integer(lt$mon == 1)
    lt$mday = last.day[1 + lt$mon] + leap.day
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) - (-(nday-lt$wday))%%7)
    class(ct) = "POSIXct"

    # Return Value:
    timeDate(format(ct), format = format, zone = FinCenter,
        FinCenter = FinCenter)
}


################################################################################
# TESTS AND REPRESENTATION OF OBJECTS:
#   We have implemented four S3 methods to test and represent 'timeDate'
#   objects. The methods check if a given object is of class 'timeDate',
#   print 'timeDate' objects including 'FinCenter' and 'Data' Slot,
#   summarize details of a 'timeDate' object, and format 'timeDate' 
#   objects as ISO conform formatted character strings.


is.timeDate = 
function(object) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks if object is of class 'timeDate'
    
    # Arguments:
    #   object - a 'timeDate' object to be checked.
    
    # Value:
    #   Returns 'TRUE' or 'FALSE' depending on whether its
    #   argument is of 'timeDate' type or not.
 
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Object:
    ans = inherits(object, "timeDate")
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


print.timeDate =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints FinCenter and timeDate for a 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object to be printed.
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns a printed report on 'timeDate' objects.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Print:
    print(x@FinCenter)
    layout = paste("[", as.character(x@Data), "]", sep = "")
    
    # Return Value:
    print(layout, quote = FALSE, ...) 
}   
    

# ------------------------------------------------------------------------------


summary.timeDate = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summarizes details of a 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object to be summarized.
    
    # Value:
    #   Returns a summary report of the details of a 'timeDate'
    #   object.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Print:
    x = object
    cat("\nObject:       ", as.character(match.call())[2])
    cat("\nFirstRecord:  ", as.character(start(x)))
    cat("\nEndRecord:    ", as.character(end(x)))
    cat("\nObservations: ", length(as.character(td)))
    cat("\nFormat:       ", x@format)
    cat("\nFinCenter:    ", x@FinCenter)
    cat("\n")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


format.timeDate = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Formats 'timeDate' as ISO conform character string
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns an ISO conform formatted character string.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Format:
    # format.POSIXlt(x, format = "", usetz = FALSE, ...) 
    ans = format.POSIXlt(x@Data, ...)
    # print(x@FinCenter)    
    
    # Return Value:
    ans
}


################################################################################

