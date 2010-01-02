`calcPortfSummary` <-
function(Portfolio, Date=NULL)
{ # @ author Peter Carl

    # DESCRIPTION
    # Create portfolio summary with the following columns
    # Long.Value, Short.Value, Net.Value, Trading.PL

    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    TradingPL = calcPortfAttr(Portfolio, 'Trading.PL', Date)
    RealizedPL = calcPortfAttr(Portfolio, 'Realized.PL', Date)
    UnrealizedPL = calcPortfAttr(Portfolio, 'Unrealized.PL', Date)
    # UnrealizedPL = TradingPL - RealizedPL
    TxnFees = calcPortfAttr(Portfolio, 'Txn.Fees', Date)
    NetValue = calcPortfAttr(Portfolio, 'Net.Value', Date)
    GrossValue = calcPortfAttr(Portfolio, 'Gross.Value', Date)
    LongValue = calcPortfAttr(Portfolio, 'Long.Value', Date)
    ShortValue = calcPortfAttr(Portfolio, 'Short.Value', Date)

    summary=merge(LongValue, ShortValue, NetValue, GrossValue, TxnFees, RealizedPL, UnrealizedPL, TradingPL)
#     colnames(summary)=c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees','Realized.PL', 'Unrealized.PL', 'Trading.PL')
# Needed?
    return(summary)
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
