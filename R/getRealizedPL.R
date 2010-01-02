`getRealizedPL` <-
function(Portfolio, Symbol, Date)
{ # @author Peter Carl

    # DESCRIPTION:
    # Retrieves realized PL for a period

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Date: date for which to get realized PL.  Use xts subsetting for best 
    # results, e.g., '1980-01-01' for a whole day, '1980-01' for a month

    # Outputs
    # Realized PL calculated for the transaction

    # FUNCTION
    PosData = Portfolio[[Symbol]]$txn
    RealizedPL = sum(as.numeric(PosData[Date, 'Realized.PL', drop=FALSE]))
    return(RealizedPL)
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
