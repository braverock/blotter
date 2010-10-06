.getBySymbol <- function(Portfolio, Attribute, Dates=NULL, Symbols=NULL, native=FALSE)
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
    if(is.null(Dates) | is.na(Dates)) # if no date is specified, get all available dates
        Dates = time(Portfolio$symbols[[1]]$posPL)
    # else  Dates = time(Portfolio$symbols[[1]]$posPL[Dates])
    if(!is.null(attr(Portfolio,'currency')) & native==FALSE) {
        p.ccy.str<-attr(Portfolio,'currency')
        namePosPL = paste("posPL", p.ccy.str, sep=".")
    } else {
        print("Returning position values in native currency values")
        namePosPL = "posPL"
        # Alternatively, we could just use posPL without ccy extension
    }

    table = NULL 
      ## Need a reference time index
    if(is.null(Symbols))
        symbols=names(Portfolio$symbols)
    else
        symbols = Symbols
    
    for (symbol in symbols) {
        tmp_col = Portfolio$symbols[[symbol]][[namePosPL]][Dates,Attribute,drop=FALSE]
        if(is.null(table)) table = tmp_col
        else table = merge(table, tmp_col)
    }
    colnames(table) = symbols
    class(table)<-class(xts())
    return(table)
### TODO: NA fill like getByPortfolio

## TODO: append summary information in last columns based on Attribute requested
# e.g., 'Pos.Value' would append Net.Value, Gross.Value, Long.Value, Short.Value
# using calcPortfAttr(p,'Net.Value')
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
