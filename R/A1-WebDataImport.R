
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
# FUNCTIONS:            DESCRIPTION:
#  economagicImport      Downloads market data from EconoMagic's web site
#  yahooImport           Downloads market data from Yahoo's web site 
#  .yahooImport          ... the old download function 
#  keystatsImport        Downloads key statistics from Yahoo's web site  
#  fredImport            Downloads market data from St. Louis FED web site
################################################################################


economagicImport =
function (file = "tempfile", 
source = "http://www.economagic.com/em-cgi/data.exe/", 
query, frequency = c("quarterly", "monthly", "daily"), 
save = FALSE, colname = "VALUE", try = TRUE)
{   # A function implemented by Diethelm Wuertz
    
    # Notes:
    #   Note, only the first column is returned, the remaining are  
    #     usually percentual changes which can be calculated otherwise.
    #   Required Functions:
    #     fields() cuts a string in fields
    
    # Examples:
    #   USDEUR Foreign Exchange Rate:
    #   economagicImport(file = "USDEUR.CSV", query = "fedny/day-fxus2eu", 
    #       frequency = "daily", save = TRUE, colname = "USDEUR")
    #   USFEDFUNDS US FedFunds Rate:
    #   EconomagicImport("USFEDFUNDS.CSV", query = "fedstl/fedfunds+2", 
    #       frequency = "monthly", save = TRUE, colname = "USFEDFUNDS")
    #   USDGNP:
    #   economagicImport(file = "USGNP.CSV", query = "fedstl/gnp", 
    #       frequency = "quarterly", save = TRUE, colname = "USGNP")

    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(economagicImport(file = file, source = source, 
            query = query, frequency = frequency, 
            save = save, colname = colname, try = FALSE)) 
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) }
        else {
            return(z) }}    
    else {
        # Internal Function:
        fields = 
        function (x, rm.spaces = TRUE, ignore.quotes = TRUE, sep = ",") {
            # Remove White Spaces:
            if(rm.spaces) { 
                p = nchar(x); result = NULL
                while (p > 0) {
                    p = regexpr("\ ", x); x1 =substring(x, 1, p-1)
                    x2 = substring(x, p+1); x =paste(x1 ,x2, sep="")}}   
            # Ignore Quotation Marks:
            if(ignore.quotes) { 
                p = nchar(x); result = NULL
                while (p > 0) {
                    p = regexpr('\"', x); x1 =substring(x, 1, p-1)
                    x2 = substring(x, p+1); x =paste(x1, x2, sep="")}}   
            # Retrieve Individual Fields:
            sep = paste("\\", sep, sep="")
            p = nchar(x); result = NULL
            while (p > 0) {
                p = regexpr(sep, x); add =substring(x, 1, p-1)
                x = substring(x, p+1)
                if (p > 0) result = c(result, add)
                else result = c(result, x)}
            # Return Value:
            result }
        
        # Settings:
        if (length(frequency) == 4) stop("Define frequency value")
        if (frequency == "quarterly" || frequency == "monthly") n =2
        if (frequency == "daily") n =3
        if (n == 0) stop("no valid frequency value")
       
        # Download the file:
        download.file(url = paste(source, query, sep = ""), destfile = file)
    
        # Extract all the data records:
        lines = grep("font color=white", scan(file, what = "", sep = "\n", 
            quiet = TRUE))
        
        # Build the data table z:
        z = scan(file, what = "", sep = "\n", quiet = TRUE)[lines]
        z = gsub("font", "          ", x=z)         # remove irrelevant strings
        z = gsub("<", " ", x=z)                     # remove irrelevant strings
        z = gsub(">", " ", x=z)                     # remove irrelevant strings
        z = gsub("color=white........", " ", x=z)   # remove irrelevant strings
        for (i in 1:15) { z =gsub("  ", " ", x=z)}  # remove double blank spaces
        for (i in 1:n) { z =sub(" ", "", x=z)}      # link date together
        z = sub(" $", "", perl=TRUE, x=z)           # remove trailing spaces
        n.rows =length(z)                           # count records
        z = as.numeric(fields(z, rm.spaces = FALSE, sep = " "))
        n.fields =length(z)     
        z = matrix(z, ncol = n.fields/n.rows)       # create matrix
        
        # Create the dates in ISO-8601 format:
        # For quarterly data multiplay quarters by 3 to get monthly base
        if (frequency == "quarterly") z[,1] =100*(z[,1]%/%100)+3*z[,1]%%100
        z = data.frame(cbind(z[,1], z[,2]))
        znames = as.character(1:(length(names(z)) - 1))
        names(z) = c("DATE", colname)   
        
        # Save to file:
        if (save) {
            write.table(z, file, quote = FALSE, sep = ",", row.names = FALSE) }
        else {
            unlink(file) }
        
        # Return Value:
        return(z)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


yahooImport = 
function (file = "tempfile", source = "http://chart.yahoo.com/table.csv?", 
query, save = FALSE, sep = ";", swap = 20, try = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   IBM SHARES, test 19/20 century change 01-12-1999 -- 31-01-2000:
    #   yahooImport("IBM.CSV", 
    #     query = "s=IBM&a=11&b=1&c=1999&d=0&q=31&f=2000&z=IBM&x=.csv", 
    #   save = TRUE)

    # Notes:
    #   Requires: fields() cuts a string in fields
    #   Yahoo Token Description:           
    #   s     Selected Ticker-Symbol
    #   a     First Quote starts with Month (mm): 0-11, Jan-Dec
    #   b     First Quote starts with Day (dd)
    #   c     First Quote starts with Year: as CCYY
    #   d     Last Quote ends with Month (mm): 0-11, Jan-Dec
    #   e     Last Quote ends with Day (dd)
    #   f     Last Quote ends with Year (yy): as CCYY
    #   z     Selected Ticker-Symbol [optional]

    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(yahooImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) }
        else {
            return(z) }}    
    else {
        # Download the file:
        download.file(url = paste(source, query, sep = ""), destfile = file)
         
        # Read from file and revert time order:
        x = rev(scan(file, quiet = TRUE, what = "", skip = 1))
        
        # Extract only date lines including dates:
		x = strsplit(x[regexpr("-...-..,", x) > 0], ",")
		
		# Create a matrix from the list:
		x = matrix(unlist(x), byrow = TRUE, nrow = length(x))
		
		# Transfer to numeric data.frame:
		z = matrix(as.numeric(x[, -1]), ncol = dim(x)[2]-1)
		
		# Add row (by date) and column (by instrument) names:
		rownames(z) <- as.character(sdate(fjulian(x[,1], order = "dmy", 
			swap = swap)))
		colnames(z) <- scan(file = file, n = dim(x)[2], quiet = TRUE, 
			what = "", sep = ",")[-1]
			
        # Save Download ?
		if (save) {
		    # Header:
		    write.table(t(c("%Y%m%d", colnames(z))), file, quote = FALSE, 
		    	row.names = FALSE, col.names = FALSE, sep = sep)
			# Data:
			write.table(z, file, quote = FALSE, append = TRUE, 
				col.names = FALSE, sep = sep) }
		else {
		    unlink(file) } 
		
        # Return Value:
        return(as.data.frame(z))
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
# The old yahooImport function:


.yahooImport = 
function (file = "tempfile", source = "http://chart.yahoo.com/table.csv?", 
query, save = FALSE, try = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   IBM SHARES, test 19/20 century change 01-12-1999 -- 31-01-2000:
    #   yahooImport("IBM.CSV", 
    #     query = "s=IBM&a=11&b=1&c=1999&d=0&q=31&f=2000&z=IBM&x=.csv", 
    #   save=TRUE)

    # Notes:
    #   Requires: fields() cuts a string in fields
    #   Yahoo Token Description:           
    #   s     Selected Ticker-Symbol
    #   a     First Quote starts with Month (mm): 0-11, Jan-Dec
    #   b     First Quote starts with Day (dd)
    #   c     First Quote starts with Year: as CCYY
    #   d     Last Quote ends with Month (mm): 0-11, Jan-Dec
    #   e     Last Quote ends with Day (dd)
    #   f     Last Quote ends with Year (yy): as CCYY
    #   z     Selected Ticker-Symbol

    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(.yahooImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) }
        else {
            return(z) }}    
    else {
        # Internal Function:
        fields = 
        function (x, rm.spaces = TRUE, ignore.quotes = TRUE, sep = ",") {
            options(warn=-1)
            # Remove White Spaces:
            if(rm.spaces) { 
                p = nchar(x); result = NULL
                while (p > 0) {
                    p = regexpr("\ ", x); x1 = substring(x, 1, p-1)
                    x2 = substring(x, p+1); x = paste(x1 ,x2, sep = "")}}   
            # Ignore Quotation Marks:
            if(ignore.quotes) { 
                p = nchar(x); result = NULL
                while (p > 0) {
                    p = regexpr('\"', x); x1 = substring(x, 1, p-1)
                    x2 = substring(x, p+1); x = paste(x1, x2, sep = "")}}   
            # Retrieve Individual Fields:
            sep = paste("\\", sep, sep="")
            p = nchar(x); result = NULL
            while (p > 0) {
                p = regexpr(sep,x); add = substring(x, 1, p-1)
                x = substring(x, p+1)
                if (p > 0) result = c(result, add)
                else result = c(result, x)}
            # Return Value:
            result }
            
        # Download the file:
        download.file(url = paste(source, query, sep = ""), destfile = file)
         
        # Get the names from the first line and cut it in fields,
        # Use the function fields() from fBasics:
        znames = fields(scan(file = file, n = 1, quiet = TRUE, 
            what = "", sep = "\n"))
        
        # Get the data records, starting in the second line:
        z = scan(file, quiet = TRUE, what = "", skip = 1, sep = ",")
        
        # Transform the data to matrix form:
        z = matrix(data = z, byrow = TRUE, ncol = length(znames))
        
        # Remove the last it's a comment !?
        z = z[-dim(z)[1], ]
        
        # Write the date in ISO-8601 format as CCYYMMDD,
        mm = 1:12
        names(mm) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        r = matrix(fields(z[, 1], sep = "-"), byrow = FALSE, ncol = 3)
        r[, 2] = mm[r[, 2]]
        r = matrix(as.numeric(r), byrow = FALSE, ncol = 3)
        # Add Century: Sorry works only up to 2010:
        z[,1 ] = (1950 + 50*(sign(10-r[, 3])) + r[, 3])*10000 + 
            r[, 2]*100 + r[, 1]
        
        # Build the data table z:
        z = data.frame(z)
        names(z) = znames
        for (i in 1:length(znames)) z[, i] = rev(z[, i])
        
        # Write table to file:
        if (save) {
            write.table(z, file, quote = FALSE, sep = ",", row.names = FALSE) }
        else {
            unlink(file) }
        
        # Return Value:
        return(z)
    }
    
    # Return Value:
    invisible()
}
    

# ------------------------------------------------------------------------------

  	
keystatsImport = 
function(file = "tempfile", source = "http://finance.yahoo.com/q/ks?s=", 
query, save = FALSE, try = TRUE) 
{	# A function implemented by Diethelm Wuertz

	# Description 
	#	Downloads Key Statistics on shares from Yahoo's Internet site
	# 
	# Example:
	#	keystatsImport(query = "YHOO")
	
	# FUNCTION:
	
	# Download:
	if (try) {
		# First try if the Internet can be accessed:
        z = try(keystatsImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) }
        else {
            return(z) } }
    else {                 
	    offset = 2
	    url = paste(source, query, sep = "")
	    download.file(url, file)
	    x = scan(file, what = "", sep = ">")
	    keynames = c(
	        "Market Cap ", 
	        "Enterprise Value ",
	        "Trailing P/E ",
	        "Forward P/E ",
	        "PEG Ratio ",
	        "Price/Sales ",
	        "Price/Book ",
	        "Enterprise Value/Revenue ",
	        "Enterprise Value/EBITDA ",
	        "Annual Dividend:",
	        "Dividend Yield:",
	        "Beta:",
	        "52-Week Change:",
	        "52-Week High ",
	        "52-Week Low " )
	    stats = as.character(Sys.Date())
	    for (s in keynames) {
		    grepped = paste(sub("</td", "", x[grep(s, x) + offset]))
			stats = c(stats, grepped)}
		for (i in 1:length(keynames))
			keynames[i] = substr(keynames[i], 1, nchar(keynames[i])-1)
		keynames = c("Date", keynames)       
	    # Return Value:
	    data.frame(cbind(Keyname = keynames, Statistic = stats)) }
}
    
    
# ------------------------------------------------------------------------------
  

fredImport = 
function(file = "tempfile", 
source = "http://research.stlouisfed.org/fred2/series/", 
query, frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
{   # A function implemented by Diethelm Wuertz

	# Description:
	#	Downloads Monthly Market Data, Indices and Benchmarks from 
	#	St. Louis FED, "research.stlouisfed.org".
	
	# Value:
	#	An One Column data frame with row names denoting the dates
	#	given in the POSIX format "%Y%m%d". The column lists the
	#   downloaded data records.
		
	# Examples:
	#   fredImport(query = "DPRIME")
    
	# Notes:
	#	This function is written for one-column daily data sets.
	#   Some example data sets include:  
	#     DEXUSEU   U.S. / Euro Foreign Exchange Rate 
	#     DEXSZUS	Switzerland / U.S. Foreign Exchange Rate 
	#	  DGS1      1-Year Treasury Constant Maturity Rate 
	#	  DPRIME	Bank Prime Loan Rate
	#	   

	# FUNCTION:
	
	# Check:
	if (frequency != "daily")
		stop("Only daily dat records are supported!")
	
	# Download:
	if (try) {
		# Try for Internet Connection:
        z = try(fredImport(file = file, source = source, query = query, 
            save = save, try = FALSE))
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) }
        else {
            return(z) } }
	else { 
		# Download File:
		queryFile = paste(query, "/downloaddata/", query, ".txt", sep = "")
		download.file(url = paste(source, queryFile, sep = ""), file) 
		# Scan the file:
		
		x = scan(file, what = "", sep = "\n")
		# Extract dates ^19XX and ^20XX:
		x = x[regexpr("^[12][90]", x) > 0]
		x = x[regexpr(" .$", x) < 0]
		
		# Transform to one-column matrix:
		z = matrix(as.numeric(substr(x, 11, 999)), byrow = TRUE, ncol = 1)
		
		# Add column name:
		colnames(z) <- query
		rownames(z) <- paste(substr(x, 1, 4), substr(x, 6, 7), 
			substr(x, 9, 10), sep = "")
		
		# Save Download ?
		if (save) {
		    write.table(paste("%Y%m%d", query, sep = sep), file, 
		    	quote = FALSE, row.names = FALSE, col.names = FALSE)
			write.table(z, file, quote = FALSE, append = TRUE, 
				col.names = FALSE, sep = sep) }
		else {
		    unlink(file) } 
		       
		# Return Value:
		return(as.data.frame(z) )
	}
	
	# Return Value:
	invisible()
}


# ------------------------------------------------------------------------------

