#' Retrieves all information about the position as of a date
#' 
#' NOTE This should get much more complicated from here, particularly when it's conditional on symbol, etc.
#' @param Portfolio string identifying a portfolio object containing transactions
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Date timestamp as of which to have the most recent position
#' @returnType 
#' @return All data elements related to position in a row of an xts object
#' @export
getPos <- function(Portfolio, Symbol, Date)
{ # @author Peter Carl
    Portfolio<-getPortfolio(Portfolio)    
    # FUNCTION
    PosData = Portfolio$symbols[[Symbol]]$txn
    toDate = paste('::', Date, sep="")
    # It may not make sense to return realized P&L with the position information, so only position and 
    # position average cost are returned.
    Pos = last(PosData[toDate,c('Pos.Qty','Pos.Avg.Cost')])
    return(Pos)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
