`updatePortf` <-
function(Portfolio, StartDate, EndDate)
{ # @author Peter Carl

    # DESCRIPTION
    # Function goes through each symbol and calculates the PL for each day 
    # prices are available

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # StartDate: Date from which to calculate equity account
    # EndDate: Date to stop calculating equity account
    # These dates must appear in the price stream

    # Outputs
    # Regular time series of position information and PL

    # FUNCTION
    symbols = names(Portfolio)
    for(symbol in symbols){
      Portfolio = updatePosPL(Portfolio, symbol, StartDate, EndDate, Cl(get(symbol)))
  }
  return(Portfolio)
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
