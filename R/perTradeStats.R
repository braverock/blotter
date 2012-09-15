#' calculate flat to flat per-trade statistics
#'
#' One 'trade' is defined as the entire time the symbol is not flat.
#' It may contain many transactions.  From the initial transaction that
#' moves the position away from zero to the last transaction that flattens the
#' position is all one 'trade' for the purposes of this function.
#' 
#' This is sometimes referred to as 'flat to flat' analysis.
#' 
#' Note that a trade that is open at the end of the measured period will
#' be marked to the timestamp of the end of the series.  
#' If that trade is later closed, the stats for it will likely change. 
#' This is 'mark to market' for the open position, and corresponds to
#' most trade accounting systems and risk systems in including the open
#' position in reporting.
#'  
#' @param Portfolio string identifying the portfolio
#' @param Symbol string identifying the symbol to examin trades for. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param includeOpenTrade whether to process only finished trades, or the last trade if it is still open, default TRUE
#' @param \dots any other passthrough parameters
#' @author Brian G. Peterson, Jan Humme
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6)
#' @return 
#' A \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Start}{the \code{POSIXct} timestamp of the start of the trade}
#'      \item{End}{the \code{POSIXct} timestamp of the end of the trade, when flat}
#'      \item{Init.Pos}{the initial position on opening the trade}
#'      \item{Max.Pos}{the maximum (largest) position held during the open trade}
#'      \item{Num.Txns}{ the number of transactions included in this trade}
#'      \item{Max.Notional.Cost}{ the largest notional investment cost of this trade}
#'      \item{Net.Trading.PL}{ net trading P&L in the currency of \code{Symbol}}
#'      \item{MAE}{ Maximum Adverse Excursion (MAE), in the currency of \code{Symbol}}
#'      \item{MFE}{ Maximum Favorable Excursion (MFE), in the currency of \code{Symbol}}
#'      \item{Pct.Net.Trading.PL}{ net trading P&L in percent of invested \code{Symbol} price gained or lost}
#'      \item{Pct.MAE}{ Maximum Adverse Excursion (MAE), in percent}
#'      \item{Pct.MFE}{ Maximum Favorable Excursion (MFE), in percent}
#'      \item{tick.Net.Trading.PL}{  net trading P&L in ticks}
#'      \item{tick.MAE}{ Maximum Adverse Excursion (MAE) in ticks}
#'      \item{tick.MFE}{ Maximum Favorable Excursion (MFE) in ticks} 
#' }
#' @seealso \code{\link{chart.ME}} for a chart of MAE and MFE derived from this function, 
#' and \code{\link{tradeStats}} for a summary view of the performance
#' @export
perTradeStats <- function(Portfolio, Symbol, includeOpenTrade=TRUE, ...) {
    portf <- getPortfolio(Portfolio)
    
    if(missing(Symbol)) Symbol <- names(portf$symbols)[[1]]
    
    posPL <- portf$symbols[[Symbol]]$posPL
    
    instr <- getInstrument(Symbol)
    tick_value <- instr$multiplier*instr$tick_size
    
    trades <- list()
    
    # identify start and end for each trade, where end means flat position
    trades$Start <- index(posPL[which(posPL$Pos.Value!=0 & lag(posPL$Pos.Value)==0),])
    trades$End <- index(posPL[which(posPL$Pos.Value==0 & lag(posPL$Pos.Value)!=0),])
    
    # if the last trade is still open, adjust depending on whether wants open trades or not
    if(length(trades$Start)>length(trades$End))
    {
        if(includeOpenTrade)
            trades$End <- c(trades$End,last(index(posPL)))
        else
            trades$Start <- head(trades$Start, -1)
    }
    
    # calculate information about each trade
    for(i in 1:length(trades$End))
    {
        timespan <- paste(format(trades$Start[[i]], "%Y-%m-%d %H:%M:%OS6"),
                format(trades$End[[i]], "%Y-%m-%d %H:%M:%OS6"), sep="::")
        
        trade <- posPL[timespan]        

        # close and open may occur in at same index timestamp, must be corrected
        if(first(trade)$Pos.Qty==0) trade <- tail(trade, -1)
        if(last(trade)$Pos.Qty!=0) trade <- head(trade, -1)

        # add cost basis column
        trade$Pos.Cost.Basis <- cumsum(trade$Txn.Value)
        #add running posPL column
        trade$PosPL <- trade$Pos.Value-trade$Pos.Cost.Basis
        
        #position sizes
        trades$Init.Pos[i] <- first(trade$Pos.Qty)
        trades$Max.Pos[i] <- first(trade[which(abs(trade$Pos.Qty)==max(abs(trade$Pos.Qty))),]$Pos.Qty)

        #count number of transactions
        trades$Num.Txns[i]<-length(which(trade$Txn.Value!=0))
        
        # investment
        trades$Max.Notional.Cost[i] <- first(trade[which(abs(trade$Pos.Qty)==max(abs(trade$Pos.Qty))),]$Pos.Cost.Basis)
        
        # cash P&L
        trades$Net.Trading.PL[i] <- last(trade)$PosPL
        trades$MAE[i] <- min(0,trade$PosPL)
        trades$MFE[i] <- max(0,trade$PosPL)
        
        # percentage P&L
        trade$Pct.PL <- trade$PosPL/abs(trade$Pos.Cost.Basis) #broken for last timestamp
        trade$Pct.PL[length(trade$Pct.PL)]<-last(trade)$PosPL/abs(trades$Max.Notional.Cost[i])
        
        trades$Pct.Net.Trading.PL[i] <- last(trade$Pct.PL)
        trades$Pct.MAE[i] <- min(0,trade$Pct.PL)
        trades$Pct.MFE[i] <- max(0,trade$Pct.PL)
        
        # tick P&L
        #Net.Trading.PL/position/tick value=ticks
        trade$tick.PL <- trade$PosPL/abs(trade$Pos.Qty)/tick_value #broken for last observation
        trade$tick.PL[length(trade$tick.PL)] <- last(trade$PosPL)/abs(trades$Max.Pos[i])/tick_value
        
        trades$tick.Net.Trading.PL[i] <- last(trade$tick.PL)
        trades$tick.MAE[i] <- min(0,trade$tick.PL)
        trades$tick.MFE[i] <- max(0,trade$tick.PL)
    }

    return(as.data.frame(trades))
}

#' quantiles of per-trade stats
#' @param Portfolio string identifying the portfolio
#' @param Symbol string identifying the symbol to examin trades for. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param \dots any other passthrough parameters
#' @param scale string specifying 'cash', or 'percent' for percentage of investment, or 'tick'
#' @param probs vector of probabilities for \code{quantile}
#' @author Brian G. Peterson
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6)
#' @export 
tradeQuantiles <- function(Portfolio, Symbol, ..., scale=c('cash','percent','tick'),probs=c(.5,.75,.9,.95,.99,1)) 
{
    trades <- perTradeStats(Portfolio, Symbol, ...)
    
    post <- trades[trades$Net.Trading.PL>0,]
    negt <- trades[trades$Net.Trading.PL<0,]

    ret<-NULL
    for (sc in scale){
        switch(sc,
                cash = {
                    posq <- quantile(post$Net.Trading.PL,probs=probs)
                    names(posq)<-paste('posPL',names(posq))
                    
                    negq <- -1*quantile(abs(negt$Net.Trading.PL),probs=probs)
                    names(negq)<-paste('negPL',names(negq))
                    
                    posMFEq <-quantile(post$MFE,probs=probs)
                    names(posMFEq) <- paste('posMFE',names(posMFEq))
                    posMAEq <--1*quantile(abs(post$MAE),probs=probs)
                    names(posMAEq) <- paste('posMAE',names(posMAEq))
                    
                    negMFEq <-quantile(negt$MFE,probs=probs)
                    names(negMFEq) <- paste('negMFE',names(negMFEq))
                    negMAEq <--1*quantile(abs(negt$MAE),probs=probs)
                    names(negMAEq) <- paste('negMAE',names(negMAEq))
                    
                    ret<-c(ret,posq,negq,posMFEq,posMAEq,negMFEq,negMAEq)
                },
                percent = {
                    posq <- quantile(post$Pct.Net.Trading.PL,probs=probs)
                    names(posq)<-paste('posPctPL',names(posq))
                    
                    negq <- -1*quantile(abs(negt$Pct.Net.Trading.PL),probs=probs)
                    names(negq)<-paste('negPctPL',names(negq))
                    
                    posMFEq <-quantile(post$Pct.MFE,probs=probs)
                    names(posMFEq) <- paste('posPctMFE',names(posMFEq))
                    posMAEq <--1*quantile(abs(post$Pct.MAE),probs=probs)
                    names(posMAEq) <- paste('posPctMAE',names(posMAEq))
                    
                    negMFEq <-quantile(negt$Pct.MFE,probs=probs)
                    names(negMFEq) <- paste('negPctMFE',names(negMFEq))
                    negMAEq <--1*quantile(abs(negt$Pct.MAE),probs=probs)
                    names(negMAEq) <- paste('negPctMAE',names(negMAEq))
                    
                    ret<-c(ret,posq,negq,posMFEq,posMAEq,negMFEq,negMAEq)                },
                tick = {
                    posq <- quantile(post$tick.Net.Trading.PL,probs=probs)
                    names(posq)<-paste('posTickPL',names(posq))
                    
                    negq <- -1*quantile(abs(negt$tick.Net.Trading.PL),probs=probs)
                    names(negq)<-paste('negTickPL',names(negq))
                    
                    posMFEq <-quantile(post$tick.MFE,probs=probs)
                    names(posMFEq) <- paste('posTickMFE',names(posMFEq))
                    posMAEq <--1*quantile(abs(post$tick.MAE),probs=probs)
                    names(posMAEq) <- paste('posTickMAE',names(posMAEq))
                    
                    negMFEq <-quantile(negt$tick.MFE,probs=probs)
                    names(negMFEq) <- paste('negTickMFE',names(negMFEq))
                    negMAEq <--1*quantile(abs(negt$tick.MAE),probs=probs)
                    names(negMAEq) <- paste('negTickMAE',names(negMAEq))
                    
                    ret<-c(ret,posq,negq,posMFEq,posMAEq,negMFEq,negMAEq)
                }
        ) #end scale switch   
    } #end for loop
    
    #return a single column for now, could be multiple column if we looped on Symbols
    ret<-t(t(ret))
    colnames(ret)<-Symbol
    ret
}

# to algorithmically set stops, the classic answer is to calculate quantiles.
# i'm not sure if this belongs in tradeStats, perhaps?  
# perhaps include MFE and MAE stats in tradeStats, plus some quantile information
# for MAE of the 90% and 95% quantiles of profitable trades?

################tradeQuantiles('bbands','IBM')###############################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
