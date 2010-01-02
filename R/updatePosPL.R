`updatePosPL` <-
function(Portfolio, Symbol, Dates, Prices=Cl(get(Symbol)))
{ # @author Peter Carl

    # DESCRIPTION
    # Calculates position PL from the position data and
    # corresponding close price data. 

    # Inputs
    # Portfolio: a portfolio object structured with initPortf()
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Prices: close prices in an xts object with a columnname == "Close"
    # Dates: xts subset of dates, e.g., "2007-01::2008-04-15"
    ## These dates must appear in the price stream

    # Outputs
    # Regular time series of position information and PL

    # FUNCTION
    PosAvgCost = 0
    PosQty = 0

    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Prices)
    else # could test to see if it's a list of dates, which would pass through
        Dates = time(Prices[Dates,])

    # For each date, calculate realized and unrealized P&L
    for(i in 1:length(Dates)){ ##
        # Get the current date and close price
        CurrentDate = Dates[i]
#          if(i>1) # if it isn't the first price in the time series
            PrevDate = time(Prices[grep(CurrentDate,time(Prices))-1])
#          else
          if(length(PrevDate)==0)
             PrevDate = NA

        TxnValue = getTxnValue(Portfolio, Symbol, CurrentDate)
        TxnFees = getTxnFees(Portfolio, Symbol, CurrentDate)
        PosQty = getPosQty(Portfolio, Symbol, CurrentDate)
        ClosePrice = as.numeric(Prices[CurrentDate, grep("Close", colnames(Prices))]) #not necessary
        PosValue = calcPosValue(PosQty,ClosePrice)

        if(is.na(PrevDate))
            PrevPosQty = 0
        else
            PrevPosQty = getPosQty(Portfolio, Symbol, PrevDate) 

        if(PrevPosQty==0)
            PrevClosePrice = 0
        else
            PrevClosePrice = as.numeric(Prices[PrevDate, grep("Close", colnames(Prices))]) # not necessary

        PrevPosValue = calcPosValue(PrevPosQty,PrevClosePrice)
        TradingPL = calcTradingPL(PosValue, PrevPosValue, TxnValue)
        RealizedPL = getRealizedPL(Portfolio, Symbol, CurrentDate)
        UnrealizedPL = TradingPL - RealizedPL # @todo: calcUnrealizedPL(TradingPL, RealizedPL)

        NewPeriod = as.xts(t(c(PosQty, PosValue, TxnValue, TxnFees, RealizedPL, UnrealizedPL, TradingPL)), order.by=as.POSIXct(CurrentDate))
        colnames(NewPeriod) = c('Pos.Qty', 'Pos.Value', 'Txn.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Trading.PL')
        Portfolio[[Symbol]]$posPL <- rbind(Portfolio[[Symbol]]$posPL, NewPeriod) 
    }
    return(Portfolio)
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
