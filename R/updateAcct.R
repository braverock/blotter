#' Constructs the equity account calculations from the portfolio data and
#' corresponding close prices.
#'
#' Inputs
#' Prices: close prices in an xts OHLC object with a columnname == "Close"
#' Dates: Dates from which to calculate equity account
#'
#' NOTES:
#' Realized.PL is net of transaction fees.  To support 
#' 
#' @param name 
#' @param Dates 
#' @export
updateAcct <- function(name='default', Dates=NULL) 
{ # @author Peter Carl

    Account<-try(get(paste("account",name,sep='.'), envir=.blotter))
    if(inherits(Account,"try-error"))
        stop(paste("Account",name," not found, use initAcct() to create a new account"))
    

    # FUNCTION
    Portfolios = names(Account)[-1]
    Portfolio = getPortfolio(Portfolios[1])
    # TODO FIXME this so that it finds the date range in *any*/all portfolios, not just the first
    if(is.null(Dates)) 
        #[[1]] here is the first instrument in the portfolio
        Dates = time(Portfolio[[1]]$posPL ) # if no date is specified, get all available dates
#    if(!is.timeBased(Dates) ){
        # Dates is an xts range, turn it into a list of Dates
    else Dates = time(Portfolio[[1]]$posPL[Dates])
#    } 
    # For each date, calculate realized and unrealized P&L
    for(d in 1:length(Dates)){ # d is a date slot counter
    # I shouldn't have to do this but I lose the class for the element when I do
    # for(date in Dates)
        # Append the portfolio summary data to the portfolio slot
        for(i in 1:length(Portfolios)){
            Portfolio = getPortfolio(Portfolios[i])
            row = calcPortfSummary(Portfolio, Dates[d])
            Account[[i+1]] = rbind(Account[[i+1]],row)
        }

        # Now aggregate the portfolio information into the TOTAL slot
        TxnFees = as.numeric(calcAcctAttr(Account, Attribute = 'Txn.Fees', Date = Dates[d]))
        RealizedPL = as.numeric(calcAcctAttr(Account, 'Realized.PL', Dates[d]))
        UnrealizedPL = as.numeric(calcAcctAttr(Account, 'Unrealized.PL', Dates[d]))
        TradingPL = as.numeric(calcAcctAttr(Account, 'Trading.PL', Dates[d]))
        row = xts(t(c(0, 0, TxnFees, RealizedPL, UnrealizedPL, 0, TradingPL, 0, 0, 0)), order.by=Dates[d])
        colnames(row) = c('Additions', 'Withdrawals', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Int.Income', 'Trading.PL', 'Advisory.Fees', 'Net.Performance', 'End.Eq')
        Account[['TOTAL']] <- rbind(Account[['TOTAL']], row)
    # This function does not calculate End.Eq 
    }
    assign(paste("account",name,sep='.'),Account, envir=.blotter) 
    return(name) #not sure this is a good idea
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
