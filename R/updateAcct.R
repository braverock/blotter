`updateAcct` <-
function(Account, Dates) 
{ # @author Peter Carl

    # DESCRIPTION
    # Constructs the equity account calculations from the portfolio data and
    # corresponding close prices. 

    # Inputs
    # Prices: close prices in an xts OHLC object with a columnname == "Close"
    # StartDate: Date from which to calculate equity account
    # EndDate: Date to stop calculating equity account
    # These dates must appear in the price stream

    # Outputs
    # Account object.

    # NOTES:
    # Realized.PL is net of transaction fees.  To support 

    # FUNCTION
    Portfolios = names(Account)[-1]
    Portfolio = get(Portfolios[1])
    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Portfolio[[1]]$posPL)
    else
        Dates = time(Portfolio[[1]]$posPL[Dates])

    # For each date, calculate realized and unrealized P&L
    for(d in 1:length(Dates)){ # d is a date slot counter
    # I shouldn't have to do this but I lose the class for the element when I do
    # for(date in Dates)
        # Append the portfolio summary data to the portfolio slot
        for(i in 1:length(Portfolios)){
            Portfolio = get(Portfolios[i])
            row = as.xts(calcPortfSummary(Portfolio, Dates[d]), order.by=as.Date(Dates[d])) ## comes back as zoo?
            Account[[i+1]] = rbind(Account[[i+1]],row)
        }

        # Now aggregate the portfolio information into the TOTAL slot
        TxnFees = as.numeric(calcAcctAttr(Account, 'Txn.Fees', Dates[d]))
        RealizedPL = as.numeric(calcAcctAttr(Account, 'Realized.PL', Dates[d]))
        UnrealizedPL = as.numeric(calcAcctAttr(Account, 'Unrealized.PL', Dates[d]))
        TradingPL = as.numeric(calcAcctAttr(Account, 'Trading.PL', Dates[d]))
        row = as.xts(t(c(0, 0, TxnFees, RealizedPL, UnrealizedPL, 0, TradingPL, 0, 0, 0)), order.by=as.Date(Dates[d]))
        colnames(row) = c('Additions', 'Withdrawals', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Int.Income', 'Trading.PL', 'Advisory.Fees', 'Net.Performance', 'End.Eq')
        Account[['TOTAL']] <- rbind(Account[['TOTAL']], row)
    # This function does not calculate End.Eq 
    }
    return(Account) 
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
