
#
# fBasics Functions Addon:
#
#   1 Chron Package Addon
#   2 Estimates densities using smoothing spline ANOVA models
#   3 Computes Bootstrapped Mean
#   4 Imports data from comma delimited CSV files
#   5 Downloads Monthly Market Data from "www.forecasts.org"
#   6 Time/Date Functions
#
# Author:
#	Diethelm Wuertz, GPL
#


################################################################################
# 1 Chron Package Addon


print.dates =
function (x, digits = NULL, quote = FALSE, prefix = "", simplify, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	yyyy-m-d Fix for chron package and thus allows for ISO8601 
	#	"CCYY-MM-DD" standard format.	
	
	# Details:
	# 	Default input format: "m/d/y"
	# 	Dates yields the wrong print out - try ...
	# 	  dates("5/15/03",   out.format="yyyy-m-d")   # yields: 2003-May-15
	# 	  dates("5/15/2003", out.format="yyyy-m-d")   # yields: 2003-May-15
	#   Note, that the correct result should be: 2003-05-15
	#   This function patches the bug.		
	
	# FUNCTION:    
	
	# Requirements:
	require(chron)
	
	# Print Dates:
	if (!as.logical(length(x))) {
        cat("dates(0)\n")
        return(invisible(x)) }
    if (missing(simplify) && is.null(simplify <- getOption("chron.simplify"))) 
        simplify <- FALSE 	    
    # Patch - A quick and dirty hack ...
    save.month.abb <<- month.abb
    m = regexpr("mon", attributes(x)$format) < 0
    if (m) month.abb <<- c(paste("0", 1:9, sep=""), "10", "11", "12") 
    formatted = as.character(format.dates(x, simplify = simplify))
	print(formatted, quote = quote) 
    month.abb <<- save.month.abb 	    
    
    # Return Value:
    invisible(x) 
}
	

# ------------------------------------------------------------------------------

    
print.chron = 
function (x, digits = NULL, quote = FALSE, prefix = "", sep = " ", 
enclosed = c("(", ")"), simplify, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	yyyy-m-d Fix for chron package and thus allows for ISO8601 
	#	"CCYY-MM-DD hh:mm:ss" standard format.	
	
	# Details:
	# 	Dates yields the wrong print out - try ...
	# 	  chron("5/15/03", "15:30:00", out.format=c("yyyy-m-d", "h:m:s")) 
	#     chron("5/15/2003", "15:30:00", out.format=c("yyyy-m-d", "h:m:s"))
	#   This yields: (2003-May-15 15:30:00)
	#   Note, that the result should be: (2003-05-15 15:30:00)
	#   This function patches the bug.	
	
	# FUNCTION:	    
	
	# Requirements:
	require(chron)
	
	# Print Chron:
	if (!as.logical(length(x))) {
        cat("chron(0)\n")
        return(invisible(x))}
    if (missing(simplify) && is.null(simplify <- getOption("chron.simplify"))) 
        simplify <- FALSE	    
    # Patch - A quick and dirty hack ...
    save.month.abb <<- month.abb
    m = regexpr("mon", attributes(x)$format) < 0
    if (m[1]) month.abb <<- c(paste("0", 1:9, sep=""), "10", "11", "12") 
    formatted = as.character(format.chron(x, sep = sep, enclosed = enclosed, 
    	simplify = simplify))
	print(formatted, quote = quote) 
    month.abb <<- save.month.abb 	    
    # Return Value:
    invisible(x) }

    
# ------------------------------------------------------------------------------


seq.chron = 
function(from, to, by = "days", length.out = NULL, k.by = 1, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	SPlus like "seq.dates" function	
	
	# Details:
	#	Like "seq.dates", adding by = "quarters" arguments and
	#	SPlus like argument list. For Splus compatibility.
	
	# Notes:
	# 	Very preliminary status.
	
	# FUNCTION:
	
	# Requirements:
	require(chron)
	
	# Add-Ons to Chron:	
	options(warn = -1) # Don't warn if the last column of the matrix cycles!
	if (is.numeric(by)) {
		ans = seq.dates(from = from, to = to, by = "days") 
		index = matrix(1:length(ans), byrow = TRUE, ncol = by)[,1]
		ans = ans[index] }
	else {
		if (by == "quarters") {
			ans = seq.dates(from = from, to = to, by = "months") 
			index = matrix(1:length(ans), byrow = TRUE, ncol = 3)[,1]
			ans = ans[index] }
		else {
			ans = seq.dates(from = from, to = to, by = by) }}       
    if (k.by > 1) {
    	index = matrix(1:length(ans), byrow = TRUE, ncol = k.by)[,1]
		ans = ans[index] }	
	# Return Value:
	ans 
}


################################################################################
# 2 Estimate probability densities using smoothing spline ANOVA models
#	with cubic spline, linear spline, or thin-plate spline marginals for 
#	numerical variables, "ssdFit" Furthermore, write simple function  
#	wrappers "dssd", "pssd", "qssd" and "rssd" for the probability density,  
#	probabilities, quantiles and random variates. 
#	Hint: use the functions from R's "gss" Package. 


dssd = 
function(x, object) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#  Evaluate density using smoothing spline ANOVA model 
	
	# FUNCTION:
	
	# Requirements:
	require(gss)
	
	# Return Value:
	dssden(object = object, x = x) 
}	


# ------------------------------------------------------------------------------

 
pssd = 
function(q, object) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#  	Evaluate probability using smoothing spline ANOVA model 
	
	# FUNCTION:
	
	# Requirements:
	require(gss)
	
	# Return Value:
	pssden(object = object, q = q) 
}


# ------------------------------------------------------------------------------


qssd = 
function(p, object) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	# 	Evaluate quantiles using smoothing spline ANOVA model 	
	
	# FUNCTION:
	
	# Requirements:
	require(gss)
	
	# Return Value:
	qssden(object = object, p = p) 
}


# ------------------------------------------------------------------------------


rssd = 
function(n, object) 
{   # A function implemented by Diethelm Wuertz

	# Description:
	# 	Generate random deviates using smoothing spline ANOVA model 
	
	# FUNCTION:
	
	# Requirements:
	require(gss)
	
	# Return Value:
	qssden(object = object, p = runif(n)) 
}


# ------------------------------------------------------------------------------


ssdFit = 
function(x, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	# 	Estimate probability densities using smoothing spline ANOVA 
	# 	models with cubic spline, linear spline, or thin-plate spline 
	#	marginals for numerical variables. x is a numeric vector.	
	
	# FUNCTION:
	
	# Requirements:
	require(gss)
	
	# Return Value:
	ssden(~x, ...)	
}


################################################################################
# 3 Computes Bootstrapped Mean
#   A very fast implementation of the basic nonparametric bootstrap for 
#   obtaining confidence limits for the population mean without assuming 
#   normality.       


bootMean =
function(x, B = 1000, ci = 0.95, na.rm = TRUE, reps = FALSE)
{   # A function implemented by Diethelm Wuertz
	
	# Description:
    #   A very fast implementation of the basic nonparametric 
    #   bootstrap for obtaining confidence limits for the population 
    #   mean without assuming normality.       
    
    # Arguments:
    #   B - number of bootstrap resamples, by default 1000.
    #   ci - specifies the confidence level (0-1) for interval 
    #       estimation of the population mean. 
    #   na.rm - a logical flag, should NAs be removed?
    #   reps - set to TRUE to have bootMean return the vector 
    #       of bootstrapped means as the reps attribute of 
    #       the returned object .
    
    # Notes:
    #   The function calls "smean.cl.boot" from the "Hisc" package
    #   Requirements: require(Hmisc)       
    
    # FUNCTION:       
    
    # Requirements:
    sink("@sink@") # Skip Loading Comments ...
    library(Design, warn.conflicts = FALSE)
    library(Hmisc, warn.conflicts = FALSE)
    sink()
    unlink("@sink@") 
           
    # Return Value:
    smean.cl.boot(x = x, conf.int = ci, B = B, na.rm = na.rm, reps = reps)
}


################################################################################
# 4 Imports data from comma delimited CSV files


data.csv = csvImport =
function (..., package = .packages(), sep = ",")
{	
	# Description:
	#	Imports data from comma delimited CSV files
	
	# Arguments:
	#	package - a name or character vector giving the packages to 
	#	  look into for data sets.  By default, all packages in the 
	#	  search path are used, then the `data' directory (if present) 
	#	  of the current working directory.
	#	sep - the field separator character. Values on each line of 
	#	  the file are separated by this delimeter, by default a comma 
	#	 for "fLibraries" files.
	#	... - the filename (excluding the .csv extension), a character 
	#	string.
	
	# Value:
	#	A dataframe for a multi-column data file, a vector for a 
	#	single-column data file.
	
	# Notes:
	#	This function is a synonyme for R's data() reading *.csv files
	#	  with an arbitrary file separator. The only difference to the
	#	  original code of data() ist, that we have added an additional
	#	  argument sep.csv with default value ",".
	#	Be aware , multi-columns tables are returned as dataframes, 
	#	  single-column tables are returned as vector.
	
	# FUNCTION:
	
	# Settings:
	list = character(0)
	lib.loc = NULL 
	verbose = getOption("verbose")
	sep.csv = ","
	row.names=NULL 
	
	# Internal Function:
	sQuote <- function(s) paste("`", s, "'", sep = "")
	names <- c(as.character(substitute(list(...))[-1]), list)
	if (!missing(package)) 
		if (is.name(y <- substitute(package))) 
			package <- as.character(y)
	found <- FALSE
	fsep <- .Platform$file.sep
	paths <- .find.package(package, lib.loc, verbose = verbose)
	if (is.null(lib.loc)) 
		paths <- c(.path.package(package, TRUE), getwd(), paths)
	paths <- unique(paths[file.exists(paths)])
	nodata <- !file.exists(file.path(paths, "data"))
	if (any(nodata)) {
		if (!missing(package) && (length(package) > 0)) {
			packagesWithNoData <- package[package %in% sapply(paths[nodata], 
			basename)]
		if (length(packagesWithNoData) > 1) {
			warning(paste("packages", paste(sQuote(packagesWithNoData), 
			collapse = ", "), "contain no datasets")) }
		else if (length(packagesWithNoData) == 1) {
			warning(paste("package", sQuote(packagesWithNoData), 
				"contains no datasets")) } }
		paths <- paths[!nodata] }
	if (length(names) == 0) {
		db <- matrix(character(0), nr = 0, nc = 4)
		noindex <- character(0)
		for (path in paths) {
			INDEX <- file.path(path, "data", "00Index")
		if (file.exists(INDEX)) {
			entries <- read.00Index(INDEX)
			if (NROW(entries) > 0) {
				db <- rbind(db, cbind(basename(path), dirname(path), 
			entries)) } }
		else {
			if (length(list.files(file.path(path, "data"))) > 0) 
			noindex <- c(noindex, basename(path)) } }
		colnames(db) <- c("Package", "LibPath", "Item", "Title")
		if (length(noindex) > 0) {
			if (!missing(package) && (length(package) > 0)) {
				packagesWithNoIndex <- package[package %in% noindex]
			if (length(packagesWithNoIndex) > 1) {
				warning(paste("packages", paste(sQuote(packagesWithNoIndex), 
				collapse = ", "), "contain datasets but no index")) }
			else if (length(packagesWithNoIndex) == 1) 
				warning(paste("package", sQuote(packagesWithNoIndex), 
				"contains datasets but no index")) } }
		footer <- if (missing(package)) 
		paste("Use `data(package = ", ".packages(all.available = TRUE))'\n", 
			"to list the data sets in all ", "*available* packages.", 
			sep = "")
		else NULL
		y <- list(type = "data", header = NULL, results = db, 
			footer = footer)
			class(y) <- "packageIQR"
		return(y) }
	paths <- file.path(paths, "data")
	for (name in names) {
		files <- NULL
		for (p in paths) {
			if (file.exists(file.path(p, "Rdata.zip"))) {
				if (file.exists(fp <- file.path(p, "filelist"))) 
				files <- c(files, file.path(p, scan(fp, what = "", 
					quiet = TRUE)))
			else warning(paste("`filelist' is missing for dir", p)) }
			else {
			files <- c(files, list.files(p, full = TRUE)) } }
		files <- files[grep(name, files)]
		found <- FALSE
		if (length(files) > 0) {
			subpre <- paste(".*", fsep, sep = "")
			for (file in files) {
			if (verbose) 
			cat("name=", name, ":  file= ...", fsep, sub(subpre, 
				"", file), "::      ", sep = "")
			if (found) 
			break
			found <- TRUE
			ext <- sub(".*\\.", "", file)
			if (sub(subpre, "", file) != paste(name, ".", 
			ext, sep = "")) 
			found <- FALSE
			else {
			zfile <- zip.file.extract(file, "Rdata.zip")
			# next four lines added:
			if (ext == "csv" || ext == "CSV") {
				# handling data.frames:
				result <- read.table(zfile, header = TRUE, 
					sep = sep.csv,  row.names=row.names)
				# handling if there is only one column: 
				if(length(names) == 1) result <- result[,]}
			switch(ext, 
				R = , 
					r = source(zfile, chdir = TRUE), 
				RData = , 
				rdata = , 
					rda = load(zfile, envir = .GlobalEnv), 
				TXT = , 
				txt = , 
					tab = assign(name, read.table(zfile, header = TRUE), 
						env = .GlobalEnv), 
				CSV = , 
					csv = assign(name, result, env = .GlobalEnv), 
				found <- FALSE)			
			if (zfile != file) 
				unlink(zfile) }
			if (verbose) 
			cat(if (!found) 
			 "*NOT* ", "found\n") } }
		if (!found) 
		warning(paste("Data set", sQuote(name), "not found")) }
	
	# Return Value:
	invisible(name)
}


################################################################################
# 5 Downloads Monthly Market Data, Indices and Benchmarks from the 
#	Financial Forecast Center, "www.forecasts.org".


forecastsImport = 
function(file = "tempfile", source = "http://www.forecasts.org/data/data/", 
query, save = FALSE, try = TRUE) 
{   # A function implemented by Diethelm Wuertz

	# Description:
	#	Downloads Monthly Market Data, Indices and Benchmarks from the 
	#	Financial Forecast Center, "www.forecasts.org".
	
	# Value:
	#	An One Column data frame with row names denoting the dates
	#	given in the POSIX format "%Y%m%d".
		
	# Examples:
	#   forecastsImport(query = "GOLD")
    #	forecastsImport(query = "MDISCRT")
    #	forecastsImport(query = "EXJPUS")
    #	forecastsImport(query = "GS3M")
    #	forecastsImport(query = "FEDFUNDS")
    
	# Notes:
	#	This function is not written for daily data sets.
	#   Some example data sets include:  
	#   Indices:
	#	  djiaM     sp500M   sp100M     nysecompM  nasdcompM  djcompM  
	#	  djtransM  djutilM  spmc400M   spsc600M   r1000M     r2000M    
	#	  r3000M    w5000M   valuM	 
	#	  nik225M  daxM      hangsengM  ftse100M   tse300M    mtM
	#   Ohter:  
	#	  MDISCRT	   
	#	  EXJPUS	    
	#	  GS3M

	# FUNCTION:
	
	# Download:
	if (try) {
		# Try for Internet Connection:
        z = try(forecastsImport(file = file, source = source, query = query, 
            save = save, try = FALSE))
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) }
        else {
            return(z) } }
	else { 
		# File:
		queryFile = paste(query, ".htm", sep = "")
		# Construct URL:
		url = paste(source, queryFile, sep = "")
		# Download file:
		download.file(url, file) 
		# Scan the file:
		x = scan(file, what = "", sep = "\n")
		# Extract dates ^19XX and ^20XX:
		x = x[regexpr("^[12][90]", x) > 0]
		# Write back to file:
		write(x, file)	
		# Read as data frame:
		x = read.table(file)
		# Two types of date strings are used %Y-%m-%d and %Y.%m
		# transform to %Y%m and paste the 28th to the format string:
		x[, 1] = substr(gsub("-", ".", as.vector(x[, 1])), 1, 7)
		x = data.frame(x[, 2], row.names = 
			as.character(10000*as.numeric(x[, 1]) + 28))
		# Add column name:
		colnames(x) <- query
		# Save Download ?
		if (save) {
		    write.table(paste("%Y%m%d;", query, sep = ""), file, 
		    	quote = FALSE, row.names = FALSE, col.names = FALSE)
			write.table(x, file, quote = FALSE, append = TRUE, 
				col.names = FALSE, sep=";") }
		else {
		    unlink(file) }    
		# Return Value:
		x  }
}


################################################################################
# 6 Time/Date Functions
      
    
is.weekday = 
function(x) 
{  	# A function implemented by Diethelm Wuertz

	# Description:
	#	Test if a date is a weekday day or not
	
	# Arguments:
	#	x - an object of class "timeDate"
	
	# Value:
	#	returns a logical or a vector of logicals
	
	# FUNCTION:
	
	# Return Value:
	wday = (x@Data)$wday
	return(!(wday == 0 | wday == 6)) 
}


# ------------------------------------------------------------------------------

    
is.weekend = 
function(x) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Test if a date is a weekend day or not
	
	# Arguments:
	#	x - an object of class "timeDate"
	
	# Value:
	#	returns a logical or a vector of logicals
	
	# FUNCTION:
	
	# Return Value:
	return(!is.weekday(x)) 
}   


# ------------------------------------------------------------------------------

    
is.bizday = 
function(x, holidays) 
{ 	# A function implemented by Diethelm Wuertz

	# Description:
	#	Test if a date is a business day or not
	
	# Arguments:
	#	x - an object of class "timeDate"
	#   holidays - a holiday calendar
	
	# Value:
	#	returns a logical or a vector of logicals
	
	# FUNCTION:
	
	# Test:
    if (x@FinCenter != holidays@FinCenter) stop("Different FinCenters")
    x = is.weekday(modify(x, "trunc", "days"))
    
    # Return Value:
    return(x[!(as.character(x) %in% as.character(holidays))]) 
}     	


# ------------------------------------------------------------------------------

    
holiday.ZURICH = 
function(y = currentYear) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Returns a holiday Calendar for Zurich in Switzerland

	# Details:
    # 	Inspect the holiday database in "data/holiday.db.R"
    # 	... You can add there additional holidays!
    #   	NewYearsDay         Jan, 1st
    #   	GoodFriday          2 days before Easter
    #   	EasterMonday        1 day after Easter
    #   	LaborDay            May, 1st  
    #   	PentecostMonday     50 days after Easter
    #   	ChristmasDay        Dec, 25 
    #   	BoxingDay           Dec, 26  
    #   	CHBerchtoldsDay     Jan, 2nd
    #   	CHSechselaeuten     3rd Monday in April 
    #                       	1 week later if it coincides with Easter Monday
    #   	CHAscension         39 days after Easter
    #   	CHConfederationDay  Aug, 1st
    #   	CHKnabenschiessen   2nd Saturday to Monday in Sep
    
    # FUNCTION:
    
    # Calendar:
	years = y
	holidays = NULL
	# Iterate Years:
	for (y in years ) { 
		holidays = c(holidays, NewYearsDay(y))
	    holidays = c(holidays, GoodFriday(y))   
	    holidays = c(holidays, EasterMonday(y)) 
	    holidays = c(holidays, LaborDay(y))
	    holidays = c(holidays, PentecostMonday(y))  
	    holidays = c(holidays, ChristmasDay(y)) 
	    holidays = c(holidays, BoxingDay(y)) 
	    holidays = c(holidays, CHBerchtoldsDay(y))
	    holidays = c(holidays, CHSechselaeuten(y))
	    holidays = c(holidays, CHAscension(y))
	    holidays = c(holidays, CHConfederationDay(y))
	    holidays = c(holidays, CHKnabenschiessen(y)) }
	
	# Sort and Convert to 'timeDate':
	holidays = as.character(sort(holidays))
	ans = timeDate(holidays, format = "%Y%m%d", FinCenter = "GMT")

	# Remove Remaining Weekend Dates:
	ans = ans[!( (ans@Data)$wday == 0 | (ans@Data)$wday == 6 )]

	# Set Financial Center:
	ans@FinCenter = "Europe/Zurich"

	# Return Value:
	ans 
}


# ------------------------------------------------------------------------------

    
summary.timeSeries = 
function(x) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	S3 Summary method for objects of class "timeDate"
	
	# Arguments
	#	x - an object of class "timeDate"
	
	# FUNCTION: 

    # Series Name:
    cat("\nTime Series:        ")
    cat("\n Name:              ", substitute(x))    
    # Data Matrix:
    Dim = dim(x@Data)
    cat("\nData Matrix:        ")
    cat("\n Dimension:         ", Dim)
    cat("\n Column Names:      ", colnames(x@Data) )
    firstName = rownames(x@Data)[1]
    lastName = rownames(x@Data)[Dim[1]]
    cat("\n Row Names:         ", firstName, " ... ", lastName)
    # Date/Time Positions:
    positions = seriesPositions(x)
    cat("\nPositions:          ")
    cat("\n Start:             ", as.character(start(positions)))
    cat("\n End:               ", as.character(end(positions)))
    # Other Attributes:
    cat("\nAttributes:         ")
    cat("\n Format:            ", x@format)
    cat("\n FinCenter:         ", x@FinCenter)
    cat("\n Units:             ", x@units)
    cat("\n Title:             ", x@title)
    cat("\n Documentation:     ", x@documentation)
    cat("\n") 
}  


################################################################################

