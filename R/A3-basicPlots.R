
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
# FUNCTION:            DESCRIPTION:    
#  splusLikePlot        Sets parameters that plots look more Splus like
#  tsPlot               Returns a time series plot
#  histPlot             Returns a histogram plot
#  densityPlot          Returns a kernel density estimate plot
# FUNCTION:            DESCRIPTION 3D PLOTS:
#  circlesPlot          Returns a scatterplot of circles indexing a 3rd variable
#  perspPlot            Returns a perspective plot in 2 dimensions
# FUNCTION:            DESCRIPTION PLOT TOOLS:
#  characterTable       Shows a table of character's numerical equivalents 
#  plotcharacterTable   Shows a table of plot characters and symbols
#  colorTable           Shows a table of plot color codes
################################################################################

 
splusLikePlot = 
function(scale = 0.8)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets parameters that plots look more Splus like
    
    # Arguments:
    #   cex.axis - The magnification to be used for axis annotation
    #       relative to the current.
    #   cex.lab - The magnification to be used for x and y labels relative
    #       to the current.
    #   cex.main - The magnification to be used for main titles relative
    #       to the current.
    #   cex.sub - The magnification to be used for sub-titles relative to
    #       the current.
    
    # Note: 
    #   * Scales plotting text and symbols relative to the default
    #     so that plots look more SplusLike.    
    #   * Further parameters will be added in the future.

    # FUNCTION::fBasics
    
    # Set par:
    par(cex.axis = scale, cex.lab = scale, cex.main = scale, cex.sub = scale)
    
    # Return value:
    invisible()
}


# ------------------------------------------------------------------------------


tsPlot = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns time series graphs in a common plot
  
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    plot(x, ylab = "", ...)
         
    # Return Value:
    invisible(x)
}
     
   
# ------------------------------------------------------------------------------


histPlot = 
function(x, col = "steelblue4", border = "white", main = x@units, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns histogram plots for each column of a 
    #   timeSeries object
    
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Transform 'timeSeries':
    units = x@units
    DIM = dim(x@Data)[2]
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
	    Values = as.vector(x@Data[, i])
        result = hist(x = Values, col = col, border = border, 
            breaks = "FD", main = main[i], ...)  
        ans[[i]] = result  
    }
    names(ans) = units
    
    # Return Value:
    invisible(ans)
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, col = "steelblue4", main = x@units, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a 
    #   timeSeries object

    # FUNCTION:
    
    # Transform 'timeSeries':
    units = x@units
    DIM = dim(x@Data)[2]
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
	    Values = as.vector(x@Data[, i])
	    Density = density(Values, ...)
        plot(x = Density, col = col, type = "l", main = main[i], ...)  
        ans[[i]] = Density  
    }
    names(ans) = units
    
    # Return Value:
    invisible(ans)
}


# ******************************************************************************


circlesPlot = 
function(x, y, size = 1, ...)
{   # A function implemented by GKS
    
    # Description:  
    #   Creates a scatterplot with circle size indexing a 
    #   third variable.
    
    # Example:
    #   circlesPlot(x = rnorm(50), y = rnorm(50), size = abs(rnorm(50)))

    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Circle Plot:
    plot(x, y, type = "n", ...)
    symbols(x, y, add = TRUE, circles = sqrt(size), inches = 0.25, ...)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


perspPlot = 
function(x, y, z, theta = -40, phi = 30, col = "steelblue4", ps = 9, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a perspecvtive plot
    
    # Notes:
    #   A synonyme call for function 'persp'
    
    # FUNCTION:   
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Perspective Plot:
    if (class(version) == "Sversion") {
        # we assume SPlus:
        ans = persp(x = x, y = y, z = z, ...) 
    } else {
        # R:
        par(ps = ps)
        if (!exists("ticktype")) ticktype = "detailed"
        if (!exists("expand")) expand = 0.6
        if (!exists("r")) r = 500
        ans = persp(x = x, y = y, z = z, theta = theta, phi = phi, 
            col = col, ticktype = ticktype, expand = expand, ...) 
    }
        
    # Return Value:
    invisible(ans)
}

                        
# ******************************************************************************


characterTable = 
function(font = 1, cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Prints numeric equivalents to all latin characters.

    # Notes:
    #   The printed version doesn't allways corresponds to the 
    #   screen display. The character on line "xy" and column 
    #   "z" of the table has code "xyz". These codes can be 
    #   used as any other characters. 
    #     e.g. title("\347\340 et \340")
    #   Note on S:
    #   As the command line window of Splus can't print special 
    #   characters 
    #     cat("\347\340 et \340") 
    #   will not print the special characters, at least under 
    #   4.5 and under 2000.
    
    # Author:
    #   Source from Pierre Joyet, pierre.joyet@bluewin.ch

    # Example:
    #   for (i in 1:20) characterTable(font = i)

    # FUNCTION:
    
    # Table:
    v = 40:377
    v = v[v %% 100 < 80 & v %% 10 < 8]
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(-1:7, seq(4, 33, length = 9), type = "n", axes = FALSE, 
        xlab = "", ylab = "", cex = cex, main = "Table of Characters")
    k = 1
    for(i in 4:31)
        for(j in 0:7) {
            text(j, 35 - i, eval(parse(text = paste("\"\\", v[k], "\"",
                    sep = ""))), font = font, cex = cex)
            k = k + 1 }
    
    text(0:7, rep(33, 7), as.character(0:7), font = 3, cex = cex)
    text(rep(-1, 28), 31:4, as.character(c(4:7, 10:17, 20:27, 
        30:37)), font = 3, cex = cex)
    
    # Return Value:
    invisible(font)
}


# ------------------------------------------------------------------------------


colorTable = 
function(cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a table of plot colors.
    
    # Author:
    #   Unknown, code found on the internet.
    
    # Example:
    #   colorTable()

    # FUNCTION:
    
    # Plot:
    plot(0, 0, xlim = c(-1, 10), ylim = c(0, 10), type = 'n', axes = FALSE, 
        xlab = '', ylab = '', cex = cex, main = "Table of Color Codes")
    j = -1
    for(i in 0:99) {
        if(i %% 10 == 0) {j = j+1; k = 10}
        k = k-1
        points(j, k, pch = 15, col = i, cex = 2)
        text(j+0.45, k, i, cex = cex)}
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


plotcharacterTable = 
function(font = par('font'), cex = 0.7) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Shows a table of plot characters.
    
    # Example:
    #   plotcharacterTable()
    
    # Author:
    #   Unknown, code found on the internet.

    # FUNCTION:
    
    # Table:
    plot(0, 0, xlim = c(-1, 11), ylim = c(0, 26), type = 'n', 
        axes = FALSE, xlab = '', ylab = '', 
        main = "Table of Plot Characters")
    j = -1
    for(i in 0:255) {
        if(i %% 25 == 0) {j = j+1; k = 26}
        k = k-1
        points(j, k, pch = i, font = font, cex = cex, col = 2)
        text(j+0.50, k, i, cex = cex) 
    }
    
    # Return Value:
    invisible(font)
}


################################################################################

