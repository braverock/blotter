.getByPortf <- function(Account, Attribute, Dates=NULL)
{ # @author Peter Carl
    
    # DESCRIPTION:
    # Retrieves calculated attributes for each portfolio in the account
    # from the portfolio summary table.  Assembles into a portfolio-by-time table 

    # Inputs
    # Account: an Account object containing Portfolio summaries
    # Attribute: column name to be assembled for each symbol, any of:
    # 'Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees',
    # 'Realized.PL', 'Unrealized.PL', or 'Trading.PL'

    # Outputs
    # regular xts object of values by portfolio

    # FUNCTION
    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Account[[2]])

    table = NULL 
    i=1
    portfolios=names(Account)[-1]
    for (portfolio in portfolios) {
        tmp_col= Account[[portfolio]][Dates,Attribute,drop=FALSE]
        colnames(tmp_col)<-portfolio
        if(is.null(table)) table = tmp_col
        else table = cbind(table, tmp_col)
    }
    return(table)
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
