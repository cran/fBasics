
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
# FUNCTION:             DESCRIPTION:    
#  splusLikePlot         Sets parameters that plots look more Splus like
#  tsPlot                Returns a time series plot
#  histPlot              Returns a histogram plot
#  densityPlot           Returns a kernel density estimate plot
#  logpdfPlot            Returns a pdf plot on logarithmic scale(s)
#  qqgaussPlot           Returns a Gaussian Quantile-Quantile plot
#  scalinglawPlot        Evaluates and displays a scaling law behavior
# FUNCTION:             DESCRIPTION 3D PLOTS:
#  circlesPlot           Returns a scatterplot of circles indexing a 3rd variable
#  perspPlot             Returns a perspective plot in 2 dimensions
# FUNCTION:             DESCRIPTION PLOT TOOLS:
#  characterTable        Shows a table of character's numerical equivalents 
#  plotcharacterTable    Shows a table of plot characters and symbols
#  colorTable            Shows a table of plot color codes
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
function(x, type = "l", labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a time series plot
    
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Labels:
    if (labels) {
        xlab = "t"
        ylab = "x"
        main = paste ("Series: ", substitute(x))
        col = "steelblue4" }
    else {
        xlab = ""
        ylab = ""
        main = ""}
            
    # Plot:
    if (is.null(dim(x))) {
        if (labels) {
            ts.plot(x = x, type = type, col = col, 
                xlab = xlab, ylab = ylab, main = main, ...) 
            grid() }
        else {
            ts.plot(x = x, type = type,
                xlab = xlab, ylab = ylab, main = main, ...) }}
    else { 
        if (labels) {
            ts.plot(x = x,
                xlab = xlab, ylab = ylab, main = main, ...) 
            grid() }
        else {
            ts.plot(x = x, ...) } }
            
    # Return Value:
    invisible(x)
}
     
   
# ------------------------------------------------------------------------------


histPlot = 
function(x, col = "steelblue4", border = "white", ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a histogram plot
    
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Histogram Plot:
    ans = hist(x = x, col = col, border = border, ...) 
    
    # Return Value:
    ans
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a kernel density estimate plot

    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    doplot = TRUE
    
    # Plot:
    if (doplot) {
        return(plot(density(x), ...)) }
    else {
        return(density(x, ...)) }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


logpdfPlot = 
function(x, n = 50, doplot = TRUE, type = c("lin-log", "log-log"), ...)
{   # A function implemented by D. Wuertz
    
    # Description:
    #   Returns a pdf plot on a lin-log scale in
    #   comparisin to a Gaussian density plot
    #   and return the break-midpoints and the
    #   counts obtained from the histogram of
    #   the empirical data.
    
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Select Type:
    type = type[1]
    
    # Internal Function Log-LogPlot:
    loglogpdfPlot = function(x, n = 50, doplot = TRUE, ...) {
        # Internal FUNCTION:
        .hist = function(x, cells="FD", include.lowest=FALSE) { result = 
            hist(x, breaks=cells, include.lowest=include.lowest, plot=FALSE)
            prob.counts = result$counts/sum(result$counts) /
                diff(result$breaks)[1]
            list(breaks=result$breaks, counts=prob.counts) }
        # Histogram Count & Breaks:
        histogram = .hist(x, cells="fd", include.lowest=FALSE)
            yh = histogram$counts; xh = histogram$breaks
            xh = xh[1:(length(xh)-1)] + diff(xh)/2
            xh = xh[yh > 0]; yh = yh[yh > 0]
            yh1 = yh[xh < 0]; xh1 = abs(xh[xh < 0])
            yh2 = yh[xh > 0]; xh2 = xh[xh > 0]
            plot(log(xh1), log(yh1), type="p", ...) 
            par(err=-1); points(log(xh2), log(yh2), col=2) 
        # Compare with a Gaussian plot:
        xg = seq(from=min(xh1[1], xh[2]), 
            to=max(xh1[length(xh1)], xh2[length(xh2)]), length=n)
        xg = xg[xg > 0]
            yg = log(dnorm(xg, mean(x), sqrt(var(x))))
        par(err=-1); lines(log(xg), yg, col=3)
        # Return Value:
        invisible(list(breaks = c(xh1, xh2), counts = c(yh1, yh2), 
            fbreaks=c(-rev(xg), xg), fcounts=c(-rev(yg), yg))) }
        
    # Internal FUNCTION:
    .hist = function(x, cells="FD", include.lowest=FALSE) { 
        result = hist(x, breaks = cells, include.lowest = include.lowest, 
            plot = FALSE)
        prob.counts = result$counts/sum(result$counts)/diff(result$breaks)[1]
        list(breaks = result$breaks, counts = prob.counts) }

    # Lin-Log Plot:
    if (type == "lin-log") {
        # Histogram Count & Break-Midpoints:
        histogram = .hist(x, cells = "FD", include.lowest = FALSE)
            yh = histogram$counts
            xh = histogram$breaks
            xh = xh[1:(length(xh)-1)] + diff(xh)/2
            xh = xh[yh>0]
            yh = log(yh[yh > 0])
            if (doplot) {
                par(err=-1)
                plot(xh, yh, type = "p", ...)} 
        # Compare with a Gaussian Plot:
        xg = seq(from = xh[1], to = xh[length(xh)], length = n)
            yg = log(dnorm(xg, mean(x), sqrt(var(x))))
            if (doplot) { 
                par(err=-1)
                lines(xg, yg, col=2)}
        # Result:
        result = invisible(list(breaks = xh, counts = yh, 
            fbreaks = xg, fcounts = yg))}
    
    # Log-Log Plot:
    if (type == "log-log") {
        result = loglogpdfPlot(x = x, n = n, doplot = doplot, ...) }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


qqgaussPlot = 
function(x, span = 5, ...)
{   # A function implemented by D. Wuertz
    
    # Description:
    #   Returns a Quantile-Quantile plot.

    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Standardized qqnorm():
    y = (x-mean(x)) / sqrt(var(x))
    lim = c(-span, span)
    qqnorm(y[abs(y) < span], xlim = lim, ylim = lim, ...)
    
    # Return Value:
    invisible(qqline(y, col = 2))
}


# ------------------------------------------------------------------------------


scalinglawPlot =
function(x, span = ceiling(log(length(x)/252)/log(2)), doplot = TRUE, ...)
{   # A function implemented by D. Wuertz
  
    # Description:
    #   Investigates the scaling law.
    #   The input "x" requires log-returns.

    # FUNCTION: 
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    logtimesteps = span
    xmean = mean(x)
    
    # x have to be logarithmic returns
    y = (x-xmean)
    logprices = cumsum(y)
    
    # Scaling Power Low:
    scale = function (nx, logprices) {
        sum(abs(diff(logprices, lag = (2^nx))))}
        
    nx = 0:logtimesteps; x = nx*log(2)
    y = log(apply(matrix(nx), 1, scale, logprices))
    fit = lsfit(x, y)$coefficients
    # Robust Fit:       
    # fit = l1fit(x, y)$coefficients
    alfa = 1.0/fit[2]
    if (doplot) { 
        plot(x, y, xlab = "log-time", ylab = "log-volatility", ...)
        title(main = "Scaling Law Plot", ...)
        grid()
        abline(fit[1], fit[2], col=2)
        abline(fit[1], 0.5, col=3) }
    
    # Return Value:
    list(exponent = as.numeric(alfa), fit = fit)
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
    
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Perspective Plot:
    par(ps = ps)
    if (!exists("ticktype")) ticktype = "detailed"
    if (!exists("expand")) expand = 0.6
    if (!exists("r")) r = 500
    
    # Return Value:
    persp(x = x, y = y, z = z, theta = theta, phi = phi, 
        col = col, ticktype = ticktype, expand = expand, ...) 
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
    #   for ( i in 1:20) characterTable(font = i)

    # FUNCTION:
    
    # Settings:
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
    invisible()
}


# ------------------------------------------------------------------------------


colorTable = 
function(cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a table of plot colors.
    
    # Author:
    #   Unknown:
    
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
    #   Unknown.

    # FUNCTION:
    
    # Compute:
    plot(0, 0, xlim = c(-1, 11), ylim = c(0, 26), type = 'n', 
        axes = FALSE, xlab = '', ylab = '', main = "Table of Plot Characters")
    j = -1
    for(i in 0:255) {
        if(i %% 25 == 0) {j = j+1; k = 26}
        k = k-1
        points(j, k, pch = i, font = font, cex = cex, col = 2)
        text(j+0.50, k, i, cex = cex) }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------

