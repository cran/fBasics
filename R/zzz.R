
#*******************************************************************************
# fBasics - A SOFTWARE COLLECTION FOR FINANCIAL ENGINEERS
# PART I: Markets, Basic Statistics, Date and Time Management
#
# collected by Diethelm Wuertz
# Version 0.9
#*******************************************************************************


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
 
	
# Default Parameters:
	
	xmpBasics = function(prompt = "") {invisible(prompt)}
	myFinCenter =  "Zurich"
	currentYear = as.POSIXlt(Sys.time())$year + 1900
	myUnits = "days"
	
	
.First.lib =  
function(lib, pkg)
{   # A function implemented by Diethelm Wuertz
    
    # Package:
    cat("\nfBasics:    Markets, Basic Statistics, Date and Time")
        
    # Requires:
    require(methods)
    
    # Set a timezone if none found in environment variables or options()
    # ... as suggested by Dirk Eddelbuettel, thanks Dirk.
    if (Sys.getenv("TZ") == "") {
		if (is.null(getOption("TZ"))) {
			cat("No timezone information found, using default of GMT\n")
          	Sys.putenv("TZ" = "GMT") }
       	else {
          	cat("No timezone information found, applying option() value of",
             	getOption("TZ"), "\n")
       		Sys.putenv("TZ" = getOption("TZ")) } }

    # Load dll:
    library.dynam("fBasics", pkg, lib)
}

