#' Calculates position PL from the position data and corresponding close price data. 
#' 
#' @param Portfolio a portfolio name to a portfolio structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Dates xts subset of dates, e.g., "2007-01::2008-04-15". These dates must appear in the price stream
#' @param Prices close prices in an xts object with a columnname == "Close"
#' @param ConMult if necessary, numeric contract multiplier, not needed if instrument is defined. 
#' @return Regular time series of position information and PL 
#' @author Peter Carl
#' @export
updatePosPL <- function(Portfolio, Symbol, Dates=NULL, Prices=NULL, ConMult=NULL, ...)
{ # @author Peter Carl

    pname<-Portfolio
    Portfolio<-getPortfolio(pname) #TODO add Dates
    
    # FUNCTION
    PosAvgCost = 0
    PosQty = 0
    
    if(is.null(Prices)){
        Prices=Cl(get(Symbol))
    } 
    

#     freq = periodicity(Prices)
#     switch(freq$scale,
#             seconds = { tformat="%Y-%m-%d %H:%M:%S" },
#             minute = { tformat="%Y-%m-%d %H:%M" },
#             hourly = { tformat="%Y-%m-%d %H" },
#             daily = { tformat="%Y-%m-%d" },
#             {tformat="%Y-%m-%d"}
#     )

    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Prices)
    else 
        Dates = time(Prices[Dates])

    #TODO if ConMuilt is a time series, this won't work right
    if(is.null(ConMult) | !hasArg(ConMult)){
        tmp_instr<-try(getInstrument(Symbol))
        if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
            warning(paste("Instrument",Symbol," not found, using contract multiplier of 1"))
            ConMult<-1
        } else {
            ConMult<-tmp_instr$multiplier
        }  
    }
    PrevConMult = 1 ## @TODO: Change this to look up the value from instrument?
    CcyMult =1 ## @TODO: Change this to look up the value from instrument?
    PrevCcyMult =1 ## @TODO: Change this to look up the value from instrument?
    
    # For each date, calculate realized and unrealized P&L
    for(i in 1:length(Dates)){ ##
        # Get the current date and close price
        CurrentDate = Dates[i]
        PrevDate = time(Prices[Prices[CurrentDate,which.i=TRUE]-1]) # which.i is new in [.xts
        if (length(PrevDate)==0) next() #no price data, keep looking
        # NOTE the line above iterates to the next Date in the Dates collection, 
        # this can be the case as with contract rolls, or missing data.  price data may not cover the entire period
        PrevDateWidth = xts:::.parseISO8601(PrevDate)
        PrevDateLast = PrevDateWidth$last.time
        PriorPrevDate = time(Prices[Prices[CurrentDate,which.i=TRUE]-1])
        PriorPrevDateWidth = xts:::.parseISO8601(PriorPrevDate)
        PriorPrevDateLast = PriorPrevDateWidth$last.time
        CurrentSpan = paste(PrevDateLast, CurrentDate, sep="::")
        PrevSpan = paste(PriorPrevDateLast, PrevDate, sep="::")
        if(length(PrevDate)==0)
             PrevDate = NA
        
        #TODO write a single getTxn and use the values instead of these lines
        TxnValue = getTxnValue(pname, Symbol, CurrentSpan)
        TxnFees = getTxnFees(pname, Symbol, CurrentSpan)
        PosQty = getPosQty(pname, Symbol, as.character(CurrentDate))
        
        ClosePrice = as.numeric(last(Prices[CurrentDate, grep("Close", colnames(Prices))])) #not necessary
        PosValue = calcPosValue(PosQty, ClosePrice, ConMult)

        if(is.na(PrevDate))
            PrevPosQty = 0
        else
            PrevPosQty = getPosQty(pname, Symbol, as.character(PrevDate)) 

        if(PrevPosQty==0)
            PrevClosePrice = 0
        else
            PrevClosePrice = as.numeric(Cl(Prices)[as.character(PrevDate)])

        PrevPosValue = calcPosValue(PrevPosQty, PrevClosePrice, ConMult) ### @TODO: PrevConMult?
        TradingPL = calcTradingPL(PosValue, PrevPosValue, TxnValue)
        RealizedPL = getRealizedPL(pname, Symbol, CurrentSpan)
        UnrealizedPL = TradingPL - RealizedPL # TODO: calcUnrealizedPL(TradingPL, RealizedPL)

        NewPeriod = as.xts(t(c(PosQty, ConMult, CcyMult, PosValue, TxnValue, TxnFees, RealizedPL, UnrealizedPL, TradingPL)), order.by=as.POSIXct(CurrentDate)) #, format=tformat
        colnames(NewPeriod) = c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Txn.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Trading.PL')
        Portfolio[[Symbol]]$posPL <- rbind(Portfolio[[Symbol]]$posPL, NewPeriod) 
    }
    # return(Portfolio)
    assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
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
