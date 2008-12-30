`getBySymbol` <-
function(Portfolio, Attribute, Date=NULL)
{ # @author Peter Carl

    # DESCRIPTION:
    # Retrieves calculated attributes for each position in the portfolio
    # from the posPL table.  Assembles into a symbol-by-time table useful
    # for graphing or calculations

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Item: column name to be assembled for each symbol, any of:
    #   'Pos.Qty', 'Pos.Value', 'Txn.Value', 'Realized.PL', 'Unrealized.PL',
    #    or 'Trading.PL'

    # Outputs
    # regular xts object of values by symbol

    # FUNCTION
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    table = xts(NULL, order.by=Date) 
      ## Need a reference time index
    symbols=names(Portfolio)
    for (i in 1:length(symbols)) 
        table = merge(table, Portfolio[[i]]$posPL[Date,Attribute,drop=FALSE])
    colnames(table) = symbols
    return(table)

## TODO: append summary information in last columns based on Attribute requested
# e.g., 'Pos.Value' would append Net.Value, Gross.Value, Long.Value, Short.Value
# using calcPortfAttr(p,'Net.Value')
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
