
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# S3 MEHOD:              MATHEMATICAL OPERATIONS:
#  [.timeDate             Extracts or replaces subsets from 'timeDate' Objects
#  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
#  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
#  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
#  diff.timeDate          Returns suitably lagged and iterated differences
#  difftimeDate           Returns a difference of two 'timeDate' objects
#  c.timeDate             Concatenates objects of class 'timeDate'
#  rep.timeDate           Replicates objects of class 'timeDate'
#  start.timeDate         Extracts the first object of a 'timeDate' vector
#  end.timeDate           Extracts the last object of a 'timeDate' vector
#  modify.timeDate        Sorts, Rounds or truncates a 'timeDate' vector
#  rev.timeDate           Reverts  a 'timeDate' vector object
# S3 MEHOD:              OBJECT TRANSFORMATION:
#  as.character.timeDate  Returns a 'timeDate' object as character string
#  as.data.frame.timeDate Returns a 'timeDate' object as data frame
#  as.POSIXct.timeDate    Returns a 'timeDate' object as POSIXct object
#  julian.timeDate        Returns Julian day counts since 1970-01-01
#  atoms.timeDate         Returns date/time atoms from a 'timeDate' object
#  months.timeDate        Extract months atom from a 'timeDate' object
################################################################################


################################################################################
# MATHEMATICAL OPERATIONS:
#   This is a collection of S3 methods for objects of class 'timeDate'.
#   Included are methods to extracts or replace subsets from 'timeDate' 
#   objects, to perform arithmetic "+" and "-" operations, to group 
#   'Ops' generic functions, to return suitably lagged and iterated 
#   differences, to return differences of two 'timeDate' objects, to
#   to xoncatenate objects, to replicate objects, to rounds objects,
#   to truncates objects, to extract the first or last object of a
#   vector, to ort the objects the elements of a vector, and to revert
#   'timeDate' vector objects.


"[.timeDate" =
function(x, ..., drop = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts or replaces subsets from 'timeDate' objects
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns a subset from a 'timeDate' object.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Subsets:
    val <- lapply(x@Data, "[", ..., drop = drop)
    attributes(val) <- attributes(x@Data) 
    
    # Return Value:
    new("timeDate", 
        Data = val, 
        Dim = length(as.character(val)),
        format = x@format,
        FinCenter = x@FinCenter)      
}   


# ------------------------------------------------------------------------------


"+.timeDate" =
function(e1, e2)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs arithmetic "+" operation on 'timeDate' objects.
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'numeric'
    
    # Value:
    #   Returns a 'timeDate' object "e2" seconds later than the
    #   'timeDate' object "e1". 
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    
    # Add and Convert back to FinCenter:
    ans = timeDate(e1GMT+e2, zone = "GMT", FinCenter = e1@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


"-.timeDate" = 
function(e1, e2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs arithmetic "-" operation on 'timeDate' objects
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' or of class 'numeric'
    
    # Value:
    #   Returns a 'difftime' object if both "e1" and "e2" are
    #   'timeDate' objects, or returns a 'timeDate' object "e2"
    #   seconds earlier than "e1".
    
    # Example:
    #   charvec = c("2004-01-01 16:00:00", "2004-01-01 18:00:00")
    #   x = timeDate(charvec, zone = "GMT", FinCenter = "Europe/Zurich")
    
    # FUNCTION:
     
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "timeDate")) test2 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # First Object:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    if (inherits(e2, "timeDate")) {
        e2 = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
        # Returns difftime:
        return(e1GMT-e2) }
    if (inherits(e2, "numeric")) {
        # Returns timeDate:
        return(timeDate(e1GMT-e2, zone = "GMT", FinCenter = e1@FinCenter)) }
        
    # Return Value:
    invisible()         
}


# ------------------------------------------------------------------------------


Ops.timeDate = 
function(e1, e2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Uses group 'Ops' generic functions for 'timeDate' objects

    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' 
    
    # Value:
    #   Returns the 'Ops' grouped object.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Logical Operators:
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for timeDate objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
        "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) 
        stop(paste(.Generic, "not defined for timeDate XXX objects"))   
        
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    e2GMT = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
    
    # Convert to Julian:
    if (inherits(e1GMT, "POSIXlt")) e1 <- as.POSIXct(e1GMT)
    if (inherits(e2GMT, "POSIXlt")) e2 <- as.POSIXct(e2GMT)
    
    # Return Value:
    NextMethod(.Generic)
}


# ------------------------------------------------------------------------------


diff.timeDate =
function (x, lag = 1, differences = 1, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns suitably lagged and iterated differences
    
    # Arguments:
    #   x - a 'timeDate' object.
    #   lag - an integer indicating which lag to use, by 
    #       default 1.
    #   differences - an integer indicating the order of the 
    #       difference, by default 1.
    #   ... - further arguments to be passed to or from methods.
  
    # Value:
    #   If 'x' is a vector of length 'n' and 'differences=1', then 
    #   the computed result is equal to the successive differences
    #   'x[(1+lag):n] - x[1:(n-lag)]'.
    #   If 'difference' is larger than one this algorithm is applied
    #   recursively to 'x'. Note that the returned value is a vector 
    #   which is shorter than 'x'.

    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Convert to GMT:
    xGMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
        
    # Return Value:
    diff.POSIXt(x = as.POSIXct(xGMT@Data), xGMT@Data, lag = lag, 
        differences = differences, ...) 
}



# ------------------------------------------------------------------------------


difftimeDate = 
function(time1, time2, 
units = c("auto", "secs", "mins", "hours", "days", "weeks")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Takes a difference of two 'timeDate' objects
    
    # Arguments:
    #   time1, time2 - 'timeDate' objects.
    #   units - a character string. The units in which the results 
    #       are desired, one of the following: "auto", "secs", 
    #       "mins", "hours", "days", "weeks"
    
    # Value:
    #   'difftimeDate' takes a difference of two 'timeDate' 
    #   objects and returns an object of class 'difftime' with
    #   an attribute indicating the units. 

    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Convert to GMT:
    time1GMT = timeDate(time1, zone = time1@FinCenter, 
        FinCenter = "GMT") 
    time2GMT = timeDate(time2, zone = time2@FinCenter, 
        FinCenter = "GMT") 

    # Return Value:
    difftime(time1GMT@Data, time2GMT@Data, tz = "GMT", units = units[1]) 
}


# ------------------------------------------------------------------------------


c.timeDate = 
function(..., recursive = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Concatenates objects of class 'timeDate'
    
    # Arguments:
    #   ... - objects to be concatenated.
    #   recursive - a logical. If 'recursive=TRUE', the function 
    #       recursively descends through lists combining all their 
    #       elements into a vector.
    
    # Value:
    #   Returns all arguments to be coerced to a 'timeDate' object  
    #   which is the type of the returned value.

    # Details:
    #   This is a generic function which combines its arguments.

    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
        
    # List all:
    z = list(...)
    
    # Convert to GMT character vectors:
    all = NULL
    for (i in 1:length(z)) {
        new = format(timeDate(z[[i]], zone = z[[i]]@FinCenter, 
            FinCenter = "GMT")@Data, "%Y-%m-%d %H:%M:%S")
        all = c(all, new) }
    
    # Convert to Financial Center of the first element:
    ans = timeDate(all, zone = "GMT", FinCenter = z[[1]]@FinCenter)
    
    # Return Value:
    ans
}
  
    
# ------------------------------------------------------------------------------


rep.timeDate =
function(x, times, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Replicates objects of class 'timeDate'
    
    # Arguments:
    #   x - a 'timeDate' object
    #   times - a non-negative integer.  A vector giving the number 
    #       of times to repeat each element if of length 'length(x)', 
    #       or to repeat the whole vector if of length 1.
    
    # Value:
    #   Returns a vector of repeated elements belonging to the same 
    #   class as 'x'.
    
    # FUNCTION:

    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Convert to GMT:
    xGMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
    
    # Repeats:  
    lt = rep.POSIXlt(xGMT@Data, times = times, ...)
    
    # Convert to timeDate:
    ans = timeDate(lt, zone="GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


start.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the first object of a 'timeDate' vector

    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns from 'x' the earliest entry as an object of class 
    #   'timeDate'.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # First element:
    # print(x@FinCenter)
    xGMT = timeDate(x, zone=x@FinCenter, FinCenter="GMT")@Data
    z = as.numeric(as.POSIXct(xGMT))
    order(z)[1]
    
    # Return Value:
    x[1]
}


# ------------------------------------------------------------------------------


end.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the last object of a 'timeDate' vector

    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns an object of class 'timeDate'.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Last element:
    # print(x@FinCenter)
    xGMT = timeDate(x, zone=x@FinCenter, FinCenter="GMT")@Data
    z = as.numeric(as.POSIXct(xGMT))
    n = order(z)[length(z)]
    
    # Return Value:
    x[n]
}


# ------------------------------------------------------------------------------


modify.timeDate =
function(x, method = c("sort", "round", "trunc"), units = c("secs", 
"mins", "hours", "days"))
{	# A function implemented by Diethelm Wuertz

    # Description:
    #   Sorts, rounds or truncates a 'timeDate' vector
    
	# Select:
	method = method[1]
	units = units[1]
	
	# Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
	# Internal Function:
	sort.timeDate = 
	function(x) {  
	    # Description:
	    #   Time-sorts the objects of a 'timeDate' vector   
	    # Arguments:
	    #   x - a 'timeDate' object to be sorted.
	    #   ... - arguments passed to other methods.    
	    # Value:
	    #   Returns a sorted object of the same class as 'x'.
	    # Sort elements:
	    xGMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")@Data
	    z = as.numeric(as.POSIXct(xGMT))
	    n = order(z)  
	    # Return Value:
	    x[n] }
	    
	# Internal Function
	round.timeDate = 
	function(x, units) {   
	    # Description:
	    #   Rounds objects of class 'timeDate'
	    # Arguments:
	    #   x - a 'timeDate' object
	    #   units - a character string, one of the units listed, 
	    #       by default "secs".
	    # Value:
	    #   Returns a rounded object of class 'timeDate'.
	    # Round:
	    xGMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
	    lt = round.POSIXt(xGMT@Data, units = units) 
	    ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)   
	    # Return Value:
	    ans }

	# Internal Function
	trunc.timeDate = 
	function(x, units) {
	    # Description:
	    #   Truncates objects of class 'timeDate'
	    # Arguments:
	    #   x - a 'timeDate' object
	    #   units - one of the units listed, by default "secs". 
	    # Value:
	    #   Returns a truncated object of class 'timeDate'.
	    # Truncate:
	    xGMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
	    lt = trunc.POSIXt(xGMT@Data, units = units) 
	    ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)  
	    # Return Value:
	    ans }
	    
	# Modify:
	ans = NA
	if (method == "sort")  return(sort.timeDate(x))
	if (method == "round") return(round.timeDate(x, units))
	if (method == "trunc") return(trunc.timeDate(x, units))
	
	# Return Value:
	ans  
}


# ------------------------------------------------------------------------------


rev.timeDate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Reverts  a 'timeDate' vector object.
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object in reversed order.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Revert elements:
    x@Data = rev(x@Data)
    
    # Return Value:
    x
}

    
################################################################################
# TRANSFORMATIONS MEHODS:
#   This is a collection of S3 methods for objects of class 'timeDate'.
#   Included are methods to transform 'timeDate' objects to character 
#   strings, to data frames, to POSIXct or POSIXlt objects, to Julian
#   counts, to extract date/time atoms from calendar dates, and to 
#   extract the months atom from a 'timeDate' object.


as.character.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as character string
    
    # Arguments:
    #   x - a 'timeDate' object
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns 'x' as a character vector. 

    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Format:
    ans = format.POSIXlt(x@Data)
    # print(x@FinCenter)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.data.frame.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as data frame
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a data frame.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Data Frame:
    ans = as.data.frame.POSIXlt(x@Data, ...)
    # print(x@FinCenter)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.POSIXct.timeDate =
function(x, tz = "")
{# A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as POSIXct object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   tz - a timezone specification to be used for the conversion.
    #       (Not needed when used for 'timeDate' conversions.)
    
    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # POSIXlt:
    ans = as.POSIXct.POSIXlt(x@Data)
    
    # Return Value: 
    ans
}


# ******************************************************************************

 
julian.timeDate = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts Julian time in days since 1970-01-01
    
    # Arguments:
    #   x - a 'timeDate' object
    #   units - a character string, one of the units listed, 
    #       by default "secs".
    
    # Value:
    #   Returns the number of days (possibly fractional) since
    #   the origin.
    
    # Details:
    #   The origin is "1970-01-01 00:00:00 GMT"

    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Fixed Units:
    if (!exists("myUnits")) units = "secs" else units = myUnits
    
    # POSIX:
    lt = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")@Data

    # Difftime:  
    origin = as.POSIXlt("1970-01-02", tz = "GMT") - 24 * 3600
    res = difftime(as.POSIXct(lt), origin, units = units[1])
    ans = structure(res, origin = origin)
        
    # Return Value:
    ans
}
	

# ------------------------------------------------------------------------------


atoms.timeDate = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts atoms from a 'timeDate' object.
    
    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       calendar "atoms".
    
    # Value:
    #   Returns a data.frame with the following calendar atoms:
    #   Y(ear), m(onth), d(ay), H(our), M(inutes), S(econds).
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # mdy:
    x = x@Data
    Y = x$year + 1900
    m = x$mon + 1
    d = x$mday
    H = x$hour
    M = x$min
    S = x$sec
    
    # Data Frame:
    ans = data.frame(Y = Y, m = m, d = d, H = H, M = M, S = S)
    # print(x@FinCenter)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


months.timeDate =
function(x, abbreviate = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts months atom from a timeDate object

    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       month "atom".
    
    # Value:
    #   Returns the month from a 'timeDate' object as an integer
    #   value or vector with elements ranging between 1 and 12,
    #   numbering the months from January to December.
    
    # FUNCTION:
    
    # Check Timezone:
    if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Month:
    ans = x@Data$mon+1
    
    # Return Value:
    ans
}
    

################################################################################

