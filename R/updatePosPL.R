#' Calculates position PL from the position data and corresponding close price data. 
#' 
#' @param Portfolio a portfolio name to a portfolio structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Dates xts subset of dates, e.g., "2007-01::2008-04-15". These dates must appear in the price stream
#' @param Prices periodic prices in an xts object with a columnname compatible with \code{getPrice}
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
        Prices=getPrice(get(Symbol, envir=as.environment(.GlobalEnv)))
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
    else if(!is.timeBased(Dates)) Dates = time(Prices[Dates])

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

    CcyMult = NA 
    FXrate = NA
    invert=FALSE
    if(!is.null(attr(Portfolio,'currency'))) {
        p.ccy.str<-attr(Portfolio,'currency')
        if (tmp_instr$currency==p.ccy.str) {
            CcyMult<-1
        } else {
            port_currency<-try(getInstrument(p.ccy.str))
            if(inherits(port_currency,"try-error") | !is.instrument(port_currency)){
                warning("Currency",p.ccy.str," not found, using currency multiplier of 1")
                CcyMult<-1
            } else {
                FXrate.str<-paste(tmp_instr$currency,p.ccy.str,sep='')
                FXrate<-try(get(FXrate.str))
                if(inherits(FXrate,"try-error")){
                    FXrate.str<-paste(p.ccy.str,tmp_instr$currency,sep='')
                    FXrate<-try(get(FXrate.str))
                    if(inherits(FXrate,"try-error")){ 
                        warning("Exchange Rate",FXrate.str," not found for symbol,',Symbol,' using currency multiplier of 1")
                        CcyMult<-1
                    } else {
                        invert=TRUE
                    }
                }
            }
        }
    } else {
        message("no currency set on portfolio, using currency multiplier of 1")
        CcyMult =1
    }
    
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
        
        ClosePrice = as.numeric(last(getPrice(Prices[CurrentDate,]))) #not necessary
        PosValue = calcPosValue(PosQty, ClosePrice, ConMult)

        if(is.na(PrevDate))
            PrevPosQty = 0
        else
            PrevPosQty = getPosQty(pname, Symbol, as.character(PrevDate)) 

        if(PrevPosQty==0)
            PrevClosePrice = 0
        else
            PrevClosePrice = as.numeric(getPrice(Prices)[as.character(PrevDate)])

        PrevPosValue = calcPosValue(PrevPosQty, PrevClosePrice, ConMult) ### @TODO: PrevConMult?
        GrossTradingPL = PosValue - PrevPosValue - TxnValue
        NetTradingPL = GrossTradingPL + TxnFees # Fees are assumed to have negative values

        UnrealizedPL = PosQty*(ClosePrice-getPosAvgCost(Portfolio=pname, Symbol, CurrentDate))*ConMult

        RealizedPL = round(GrossTradingPL - UnrealizedPL,2)
        #$unrealized_gl    = $end_return['calc_position'] * ($end_return['last_price'] - $end_return['average_cost']);

        NewPeriod = as.xts(t(c(PosQty, ConMult, CcyMult, PosValue, TxnValue, RealizedPL, UnrealizedPL, GrossTradingPL, TxnFees, NetTradingPL)), order.by=CurrentDate) #, format=tformat
        colnames(NewPeriod) =  c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Txn.Value',  'Realized.PL', 'Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
        Portfolio[[Symbol]]$posPL <- rbind(Portfolio[[Symbol]]$posPL, NewPeriod) 
    }
    
    # now do the currency conversions for the whole date range
    startDate = xts:::.parseISO8601(first(Dates))$first.time-1
    endDate   = xts:::.parseISO8601(last(Dates))$last.time
    dateRange = paste(startDate,endDate,sep='::')
    TmpPeriods<-Portfolio[[Symbol]]$posPL[dateRange]
    if(is.na(CcyMult) & !is.na(FXrate)) {
        if(inherits(FXrate,'xts')){
            CcyMult <- FXrate[dateRange]
            CcyMult <- na.locf(merge(CcyMult,index(TmpPeriods)))
            CcyMult <- CcyMult[index(TmpPeriods)]
        } else {
            CcyMult<-as.numeric(FXrate)
        }
    } else {
        CcyMult<-1
    }
    if(isTRUE(invert)){
        # portfolio and instrument have different currencies, and FXrate was in the wrong direction
        CcyMult<-1/CcyMult
    }
    #multiply the correct columns, (probably one at a time?)    
    columns<-c('Pos.Value', 'Txn.Value',  'Realized.PL', 'Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
    for (column in columns){
        TmpPeriods[,column]<-TmpPeriods[,column]*CcyMult
    }
    TmpPeriods[,'Ccy.Mult']<-CcyMult
    #stick it in posPL.ccy
    Portfolio[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-rbind(Portfolio[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]],TmpPeriods)
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
