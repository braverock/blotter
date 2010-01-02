`calcAcctAttr` <-
function(Account, Attribute, Date=NULL)
{ # @author Peter Carl

    # DESCRIPTION
    # 

    # Inputs
    # Account: an Account object containing Portfolio summaries
    # Attribute: column name to be assembled for each symbol, any of:
    # 'Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees',
    # 'Realized.PL', 'Unrealized.PL', or 'Trading.PL'

    portfolios = names(Account)[-1]
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Account[[2]])
    else
        Date = time(Account[[2]][Date])
    table = xts(NULL, order.by=Date) ## Reference time index
    table = getByPortf(Account, Attribute, Date)
    result = xts(apply(table, FUN='sum', MARGIN=1), Date)
    colnames(result) = Attribute
    return(result)
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
