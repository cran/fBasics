
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
# You should have received A copy of the GNU Library General 
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
# DEMO:                         DESCRIPTION   
#  xmpDWChapter012.R
#  xmpDWChapter013.R
#  xmpDWChapter014.R
#  xmpDWChapter015.R
#  xmpZWChapter01.R
################################################################################


test.demoFile12 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fBasics\\Demo\\xmpDWChapter012.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoFile13 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fBasics\\Demo\\xmpDWChapter013.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}

# ------------------------------------------------------------------------------


test.demoFile14 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fBasics\\Demo\\xmpDWChapter014.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoFile15 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fBasics\\Demo\\xmpDWChapter015.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoZWChapter01 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fBasics\\Demo\\xmpZWChapter01.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    # WARNING - NOT YET UPDATED TO R 2.4.0, THIS MAY RESULT IN ERRORS
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runitDemo.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

