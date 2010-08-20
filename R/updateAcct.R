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

    Account<-getAccount(name)

    # FUNCTION
    Portfolios = names(Account)[-1]
    Portfolio = getPortfolio(Portfolios[1])
    # TODO FIXME this so that it finds the date range in *any*/all portfolios, not just the first
    if(is.null(Dates)) 
        #[[1]] here is the first instrument in the portfolio
        Dates = time(Portfolio$symbols[[1]]$posPL ) # if no date is specified, get all available dates
#    if(!is.timeBased(Dates) ){
        # Dates is an xts range, turn it into a list of Dates
#    else Dates = time(Portfolio$symbols[[1]]$posPL[Dates])
#    } 
    
    #TODO FIXME do two loops, one over portfolios to update each portfolio 
    # and one over accounts, to aggregate, probably via matrix addition
    
    # For each date, calculate realized and unrealized P&L
    for(d in 1:length(Dates)){ # d is a date slot counter
    # I shouldn't have to do this but I lose the class for the element when I do
    # for(date in Dates)


        # Append the portfolio summary data to the portfolio slot
        for(pname in Portfolios){
            Portfolio = getPortfolio(pname)

            CurrentDate = Dates[d]
            PrevDate = time(Portfolio$symbols[[1]]$posPL[Portfolio$symbols[[1]]$posPL[CurrentDate,which.i=TRUE]-1 ] ) # which.i is new in [.xts
            if (length(PrevDate)==0) next() #no price data, keep looking
            PrevDateWidth = xts:::.parseISO8601(PrevDate)
            PrevDateLast = PrevDateWidth$last.time
            CurrentSpan = paste(PrevDateLast, CurrentDate, sep="::")
            
            rows = calcPortfSummary(Portfolio, CurrentSpan)
            rows = na.omit(rows)
            Account[[pname]] = rbind(Account[[pname]],rows)
        }
        if(is.null(CurrentSpan)) next()
        # Now aggregate the portfolio information into the TOTAL slot
        TxnFees = calcAcctAttr(Account, Attribute = 'Txn.Fees', CurrentSpan)
        Additions = xts(rep(0,length(index(TxnFees))),order.by=index(TxnFees))
        Withdrawals = Additions
        IntIncome = Additions
        AdvisoryFees = Additions
        NetPerformance = Additions
        EndEq = Additions
        RealizedPL = calcAcctAttr(Account, 'Realized.PL', CurrentSpan)
        UnrealizedPL = calcAcctAttr(Account, 'Unrealized.PL', CurrentSpan)
        GrossTradingPL = calcAcctAttr(Account, 'Gross.Trading.PL', CurrentSpan)
        NetTradingPL = calcAcctAttr(Account, 'Net.Trading.PL', CurrentSpan)
        rows = cbind(Additions, Withdrawals, RealizedPL, UnrealizedPL, IntIncome, GrossTradingPL, TxnFees, NetTradingPL, AdvisoryFees, NetPerformance, EndEq)
        colnames(rows) = c('Additions', 'Withdrawals', 'Realized.PL', 'Unrealized.PL', 'Int.Income', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL', 'Advisory.Fees', 'Net.Performance', 'End.Eq')
        Account[['TOTAL']] <- rbind(Account[['TOTAL']], rows)
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
