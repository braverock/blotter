`getPos` <-
function(Portfolio, Symbol, Date)
{ # @author Peter Carl

    # DESCRIPTION:
    # Retrieves all information about the position as of a date

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Date: timestamp as of which to have the most recent position

    # Outputs
    # All data elements related to position in a row of an xts object
    # This should get much more complicated from here, particularly when it's conditional on symbol, etc.
    
    # FUNCTION
    PosData = Portfolio[[Symbol]]$txn
    toDate = paste('::', Date, sep="")
    # It may not make sense to return realized P&L with the position information, so only position and 
    # position average cost are returned.
    Pos = tail(PosData[toDate,c('Pos.Qty','Pos.Avg.Cost')], n=1)
    return(Pos)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
