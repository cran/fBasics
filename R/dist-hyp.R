
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


################################################################################
# FUNCTION:             DESCRIPTION:
#  dhyp                  Returns density for hyperbolic DF
#  phyp                  Returns probability for hyperbolic DF
#  qhyp                  Returns quantiles for hyperbolic DF
#  rhyp                  Returns random variates for hyperbolic DF
################################################################################


dhyp <-
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1, log = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns Hyperbolic Density Function PDF

    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:

    # Parameters:
    if (length(alpha) == 4) {
       mu <- alpha[4]
       delta <- alpha[3]
       beta <- alpha[2]
       alpha <- alpha[1]
    } 
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Density:
    ans <- if (pm == 1)
               .dhyp1(x, alpha, beta, delta, mu)
           else if (pm == 2)
               .dhyp2(x, alpha, beta, delta, mu)
           else if (pm == 3)
               .dhyp3(x, alpha, beta, delta, mu)
           else if (pm == 4)
               .dhyp4(x, alpha, beta, delta, mu)
           else
               stop("argument 'pm' should be one of the numbers 1, 2, 3, or 4")
    
    # Return value:
    if (log)
        log(ans)
    else
        ans
}


# ------------------------------------------------------------------------------


phyp <-
function(q, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Return cumulative probability of Hyperbolic PDF

    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:

    # Parameters:
    if (length(alpha) == 4) {
       mu <- alpha[4]
       delta <- alpha[3]
       beta <- alpha[2]
       alpha <- alpha[1]
    } 
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Return Value:
    if (pm == 1)
        .phyp1(q, alpha, beta, delta, mu, ...)
    else if (pm == 2)
        .phyp2(q, alpha, beta, delta, mu, ...)
    else if (pm == 3)
        .phyp3(q, alpha, beta, delta, mu, ...)
    else if (pm == 4)
        .phyp4(q, alpha, beta, delta, mu, ...)
    else
        stop("argument 'pm' should be one of the numbers 1, 2, 3, or 4")
}


# ------------------------------------------------------------------------------


qhyp <-
function(p, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quantiles of Hyperbolic PDF

    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # Note:
    #   This procedure will not run under Splus.

    # FUNCTION:

    # Parameters:
    if (length(alpha) == 4) {
       mu <- alpha[4]
       delta <- alpha[3]
       beta <- alpha[2]
       alpha <- alpha[1]
    } 
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Return Value:
    if (pm == 1)
        .qhyp1(p, alpha, beta, delta, mu, ...)
    else if (pm == 2)
        .qhyp2(p, alpha, beta, delta, mu, ...)
    else if (pm == 3)
        .qhyp3(p, alpha, beta, delta, mu, ...)
    else if (pm == 4)
        .qhyp4(p, alpha, beta, delta, mu, ...)
    else
        stop("argument 'pm' should be one of the numbers 1, 2, 3, or 4")
}


# ------------------------------------------------------------------------------


rhyp <-
function(n, alpha = 1, beta = 0, delta = 1, mu = 0, pm = 1)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns random deviates of Hyperbolic PDF

    # Arguments:
    #   n - number of random deviates to be generated
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # Notes:
    #   I have removed my original Fortran program and replaced it by
    #   the dhyperb() function from the HyperbolicDist Package, written
    #   by David Scott, Ai-Wei Lee, Jennifer Tso, Richard Trendall.
    #   License: GPL

    # FUNCTION:

    # Parameters:
    if (length(alpha) == 4) {
       mu <- alpha[4]
       delta <- alpha[3]
       beta <- alpha[2]
       alpha <- alpha[1]
    } 
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Result:
    if (pm == 1)
        .rhyp1(n, alpha, beta, delta, mu)
    else if (pm == 2)
        .rhyp2(n, alpha, beta, delta, mu)
    else if (pm == 3)
        .rhyp3(n, alpha, beta, delta, mu)
    else if (pm == 4) 
        .rhyp4(n, alpha, beta, delta, mu)
    else
        stop("argument 'pm' should be one of the numbers 1, 2, 3, or 4")
}

################################################################################
