
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
# MA 02111-1307 USA

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
#	  iCalendar Calendaring and Scheduling protocols. (RFC 2445, 2446, 
#     and 2447). It parses iCal components and provides a C API for 
#     manipulating the component properties, parameters, and subcomponents.
#   Olsen's VTIMEZONE: These data files are released under the GNU 
#	  General Public License, in keeping with the license options of 
#     libical. 
# for the holiday database:
#   holiday information collected from the internet and governmental 
#	sources obtained from a few dozens of websites


################################################################################
# FUNCTION:		   		DESCRIPTION:
#  xmpfBasics            Popups the example menu
################################################################################


xmpfBasics = 
function() 
{ 	# A function implemented by Diethelm WUertz

	# Description:
	#	Popups the example menu
	
	# FUNCTION:
	
	# Popup:
	path = paste(.Library,"/fBasics", sep = "") 
	entries = read.00Index (file.path(path, "demo", "00Index"))    
	example = select.list(entries[,1])
	selected = 0
	for (i in 1:length(entries[,1])) {
		if (example == entries[i,1]) selected = i}
	if (example == "") {
    	cat("\nNo demo selected\n")}
   	else {
     	cat("\nLibrary: ", "fBasics", "\nExample: ", 
       		entries[selected,1], "\nTitle:   ", entries[selected,2], "\n")
        source(paste(path, "/demo/", example, ".R", sep = ""), 
            echo = TRUE, verbose = FALSE)}
    if (TRUE) cat("\n") 
    
    # Return Value:
    invisible()
}
 

# ******************************************************************************
# R - Modifications and Problems:


modify =
function(x, method, units) 
{
	UseMethod("modify")	
}


# ------------------------------------------------------------------------------


modify.default =
function(x, method = c("sort", "round", "trunc"), units = NULL )
{	
	# Modify:
	ans = NA
	if (method[1] == "sort") return(sort(x))
	if (method[1] == "round") return(round(x))
	if (method[1] == "trunc") return(trunc(x))
	
	# Return Value:
	ans
}	


# ------------------------------------------------------------------------------


atoms = 
function(x, ...) 
{
	UseMethod("atoms")
}


# ------------------------------------------------------------------------------


atoms.default = 
function(x, ...) 
{
	invisible(x)
}


# ------------------------------------------------------------------------------

