`updatePosPL` <-
function(Portfolio, Symbol, StartDate, EndDate, Prices=Cl(get(Symbol)))
{ # @author Peter Carl

    # DESCRIPTION
    # Calculates position PL from the position data and
    # corresponding close price data. 

    # Inputs
    # Portfolio: a portfolio object structured with initPortf()
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Prices: close prices in an xts object with a columnname == "Close"
    # StartDate: Date from which to calculate equity account
    # EndDate: Date to stop calculating equity account
    ## These dates must appear in the price stream

    # Outputs
    # Regular time series of position information and PL

    # FUNCTION
    PosAvgCost = 0
    PosQty = 0

    StartDateRow = grep(StartDate, time(Prices))
    EndDateRow = grep(EndDate, time(Prices))

    # For each date, calculate realized and unrealized P&L
    for(i in StartDateRow:EndDateRow){ ##
        # Get the current date and close price
        CurrentDate = time(Prices)[i]
        if(i>1) # if it isn't the first price in the time series
            PrevDate = time(Prices)[i-1]
        else
            PrevDate = NA

        TxnValue = getTxnValue(Portfolio, Symbol, CurrentDate)
        TxnFees = getTxnFees(Portfolio, Symbol, CurrentDate)
        PosQty = getPosQty(Portfolio, Symbol, CurrentDate)
        ClosePrice = as.numeric(Prices[i, grep("Close", colnames(Prices))]) #not necessary
        PosValue = PosQty * ClosePrice # function?

        if(is.na(PrevDate))
            PrevPosQty = 0
        else
            PrevPosQty = getPosQty(Portfolio, Symbol, PrevDate) 

        if(PrevPosQty==0)
            PrevClosePrice = 0
        else
            PrevClosePrice = as.numeric(Prices[i-1,grep("Close", colnames(Prices))]) # not necessary
        PrevPosValue = PrevPosQty * PrevClosePrice

        TradingPL = PosValue - PrevPosValue - TxnValue #

        RealizedPL = getRealizedPL(Portfolio, Symbol, CurrentDate)
        UnrealizedPL = TradingPL - RealizedPL #

        NewPeriod = as.xts(t(c(PosQty, PosValue, TxnValue, TxnFees, RealizedPL, UnrealizedPL, TradingPL)), order.by=as.Date(CurrentDate))
        colnames(NewPeriod) = c('Pos.Qty', 'Pos.Value', 'Txn.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Trading.PL')
        Portfolio[[Symbol]]$posPL <- rbind(Portfolio[[Symbol]]$posPL, NewPeriod) 
    }
    return(Portfolio)
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
