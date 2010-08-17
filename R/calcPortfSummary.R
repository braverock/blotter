calcPortfSummary <- function(Portfolio, Dates=NULL)
{ # @ author Peter Carl
    if(!inherits(Portfolio,"portfolio")) stop("Portfolio passed is not a portfolio object.")
    # DESCRIPTION
    # Create portfolio summary with the following columns
    # Long.Value, Short.Value, Net.Value, Trading.PL

    if(is.null(Dates) || is.na(Dates)) # if no date is specified, get all available dates
        Dates = time(Portfolio[[1]]$posPL )
#    else Dates = time(Portfolio[[1]]$posPL[Dates])
    
    GrossTradingPL = calcPortfAttr(Portfolio, 'Gross.Trading.PL', Dates)
    NetTradingPL = calcPortfAttr(Portfolio, 'Net.Trading.PL', Dates)
    RealizedPL = calcPortfAttr(Portfolio, 'Realized.PL', Dates)
    UnrealizedPL = calcPortfAttr(Portfolio, 'Unrealized.PL', Dates)
    # UnrealizedPL = TradingPL - RealizedPL
    TxnFees = calcPortfAttr(Portfolio, 'Txn.Fees', Dates)
    NetValue = calcPortfAttr(Portfolio, 'Net.Value', Dates)
    GrossValue = calcPortfAttr(Portfolio, 'Gross.Value', Dates)
    LongValue = calcPortfAttr(Portfolio, 'Long.Value', Dates)
    ShortValue = calcPortfAttr(Portfolio, 'Short.Value', Dates)

    summary=merge(LongValue, ShortValue, NetValue, GrossValue, RealizedPL, UnrealizedPL, GrossTradingPL, TxnFees, NetTradingPL)
#     colnames(summary)=c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Realized.PL', 'Unrealized.PL', 'Gross.Trading.PL','Txn.Fees','Net.Trading.PL')
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
