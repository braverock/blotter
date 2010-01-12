#' Calculates position PL from the position data and corresponding close price data. 
#' 
#' @param Portfolio a portfolio name to a portfolio structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Dates xts subset of dates, e.g., "2007-01::2008-04-15". These dates must appear in the price stream
#' @param Prices close prices in an xts object with a columnname == "Close"
#' @return Regular time series of position information and PL 
#' @author Peter Carl
#' @export
updatePosPL <- function(Portfolio, Symbol, Dates, Prices=Cl(get(Symbol)))
{ # @author Peter Carl

    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter,inherits=TRUE)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",name," not found, use initPortf() to create a new account"))
    
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

        ConMult = 1 ## @TODO: Change this to look up the value from instrument
        PrevConMult = 1 ## @TODO: Change this to look up the value from instrument?

        TxnValue = getTxnValue(pname, Symbol, CurrentDate)
        TxnFees = getTxnFees(pname, Symbol, CurrentDate)
        PosQty = getPosQty(pname, Symbol, CurrentDate)
        ClosePrice = as.numeric(Prices[CurrentDate, grep("Close", colnames(Prices))]) #not necessary
        PosValue = calcPosValue(PosQty, ClosePrice, ConMult)

        if(is.na(PrevDate))
            PrevPosQty = 0
        else
            PrevPosQty = getPosQty(pname, Symbol, PrevDate) 

        if(PrevPosQty==0)
            PrevClosePrice = 0
        else
            PrevClosePrice = as.numeric(Prices[PrevDate, grep("Close", colnames(Prices))]) # not necessary

        PrevPosValue = calcPosValue(PrevPosQty, PrevClosePrice, ConMult) ### @TODO: PrevConMult?
        TradingPL = calcTradingPL(PosValue, PrevPosValue, TxnValue)
        RealizedPL = getRealizedPL(pname, Symbol, CurrentDate)
        UnrealizedPL = TradingPL - RealizedPL # TODO: calcUnrealizedPL(TradingPL, RealizedPL)

        NewPeriod = as.xts(t(c(PosQty, ConMult, PosValue, TxnValue, TxnFees, RealizedPL, UnrealizedPL, TradingPL)), order.by=as.POSIXct(CurrentDate))
        colnames(NewPeriod) = c('Pos.Qty', 'Con.Mult', 'Pos.Value', 'Txn.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Trading.PL')
        Portfolio[[Symbol]]$posPL <- rbind(Portfolio[[Symbol]]$posPL, NewPeriod) 
    }
    # return(Portfolio)
    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)
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
