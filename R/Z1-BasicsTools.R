
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
# FUNCTION:             DESCRIPTION:
#  xmpBasics             Sets prompt
#  xmpfBasics            Popups the example menu
# FUNCTION:             DESCRIPTION:
#  unirootNA             One Dimensional Root (Zero) Finding
################################################################################


xmpBasics = 
function(prompt = "") 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Sets prompt
    
    # FUNCTION:
    
    # Return Value:
    invisible(prompt)
}

    
# ------------------------------------------------------------------------------


xmpfBasics = 
function() 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Popups the example menu
    
    # FUNCTION:
    
    # Popup:
    path = paste(.Library,"/fBasics", sep = "") 
    entries = .read.fBasics.00Index (file.path(path, "demoIndex"))    
    example = select.list(entries[,1])
    selected = 0
    for (i in 1:length(entries[,1])) {
        if (example == entries[i,1]) selected = i
    }
    if (example == "") {
        cat("\nNo demo selected\n")
    } else {
        cat("\nLibrary: ", "fBasics", "\nExample: ", 
            entries[selected, 1], "\nTitle:   ", entries[selected, 2], "\n")
        source(paste(path, "/demo/", example, ".R", sep = ""))
    }
    if (TRUE) {
        cat("\n") 
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.read.fBasics.00Index = 
function (file) 
{
    if (is.character(file)) {
        if (file == "") {
            file <- stdin()
        } else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    }
    if (!inherits(file, "connection")) 
        stop(paste("argument", 
            sQuote("file"), "must be a character string or connection"))
    y <- matrix("", nr = 0, nc = 2)
    x <- paste(readLines(file), collapse = "\n")
    for (chunk in unlist(strsplit(x, "\n[       \n]*\n"))) {
        entries <- try({
            if (regexpr("(   |  )", chunk) == -1) 
                NULL
            else {
                chunk <- gsub("\n[      ]+", "  ", chunk)
                x <- strsplit(unlist(strsplit(chunk, "\n")), "[    ]")
                cbind(unlist(lapply(x, "[[", 1)), unlist(lapply(x, 
                    function(t) {
                        paste(t[-c(1, which(nchar(t) == 0))], collapse = " ")
                    })))
            }
        })
        if (!inherits(entries, "try-error") && NCOL(entries) == 2) 
            y <- rbind(y, entries)
    }
    colnames(y) <- c("Item", "Description")
    y
}


# ******************************************************************************
# R - Modifications and Problems:


unirootNA = 
function(f, interval, lower = min(interval), upper = max(interval), 
tol = .Machine$double.eps^0.25, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Searches the interval from lower to upper for a 
    #   root (i.e., zero) of the function f with respect 
    #   to its first argument. 
    
    # Arguments:
    #   see 'uniroot'
    
    # Value:
    #   Returns the x value of f where the root is located. If
    #   now root exists NA will be returned. In the last case
    #   the function doesn't terminate with an error like in the
    #   case of the standard function uniroot.

    # Details:
    #   R:
    #   uniroot(f, interval, lower = min(interval), upper = max(interval),
    #       tol = .Machine$double.eps^0.25, 
    #       maxiter = 1000, ...)
    #   uniroot(f, interval, lower = min(interval), upper = max(interval), 
    #       tol = .Machine$double.eps^.25, 
    #       keep.xy = F, f.lower = NA,  f.upper = NA, ...) 

    # Example:
    #   unirootNA(sin, c(1, 2)); unirootNA(sin, c(-1, 1))
        
    # There is no Root:  
    if (is.null(args(f))) {  
        if (f(lower) * f(upper) >=0) return(NA)  
    } else {
        if (f(lower, ...) * f(upper, ...) >= 0) return(NA)
    } 
      
    # There is a Root:  
    ans = uniroot(f = f, interval = interval, lower = lower, 
        upper = upper, tol = tol, ...)
    
    # Return Value:
    ans$root
}  


################################################################################

 