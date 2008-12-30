`getByPortf` <-
function(Account, Attribute, Date=NULL)
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
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Account[[2]])
    else
        Date = time(Account[[2]][Date])
    table = xts(NULL, order.by=Date) 
      ## Need a reference time index
    portfolios=names(Account)[-1]
    for (i in 1:length(portfolios)) 
        table = merge(table, Account[[i+1]][Date,Attribute,drop=FALSE])
    colnames(table) = portfolios
    return(table)
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
