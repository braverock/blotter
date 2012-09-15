#' Chart Maximum Adverse/Forward Excursion
#'
#' Produces a scatterplot with one point per trade, with x-axis: absolute 
#' value of Drawdown (Adverse), or Run Up (Favourable), 
#' and y-axis: absolute value of Net Profit or Loss
#'
#' @param Portfolio string identifying the portfolio to chart
#' @param Symbol string identifying the symbol to chart. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param type string specifying MAE (Adverse) or MFE (Favourable) chart type
#' @param scale string specifying 'cash', or 'percent' for percentage of investment, or 'tick'
#' @param \dots any other passthrough parameters, in particular includeOpenTrades (see perTradeStats())
#' @author Jan Humme
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6), section 3.5
#' @seealso \code{\link{perTradeStats}} for the calculations used by this chart, 
#' and \code{\link{tradeStats}} for a summary view of the performance
#' @export
chart.ME <- function(Portfolio, Symbol, type=c('MAE','MFE'), scale=c('cash','percent','tick'), ...)
{   # @author Jan Humme

    trades <- perTradeStats(Portfolio, Symbol, ...)
    
    #multiply Pcct numbers for prettier charting
    trades$Pct.Net.Trading.PL <- 100 * trades$Pct.Net.Trading.PL
    trades$Pct.MAE <- 100 * trades$Pct.MAE
    trades$Pct.MFE <- 100 * trades$Pct.MFE
    
    profitable <- (trades$Net.Trading.PL > 0)

    switch(scale,
        cash = {
            .ylab <- 'Profit/Loss (cash)'
            if(type == 'MAE')
            {
                .cols <- c('MAE','Net.Trading.PL')
                .xlab <- 'Drawdown (cash)'
                .main <- 'Maximum Adverse Excursion (MAE)'
            }
            else    # type == 'MFE'
            {
                .cols <- c('MFE','Net.Trading.PL')
                .xlab <- 'Run Up (cash)'
                .main <- 'Maximum Favourable Excursion (MFE)'
            }
        },
        percent = {
            .ylab <- 'Profit/Loss (%)'
            if(type == 'MAE')
            {
                .cols <- c('Pct.MAE','Pct.Net.Trading.PL')
                .xlab <- 'Drawdown (%)'
                .main <- 'Maximum Adverse Excursion (MAE)'
            }
            else    # type == 'MFE'
            {
                .cols <- c('Pct.MFE','Pct.Net.Trading.PL')
                .xlab <- 'Run Up (%)'
                .main <- 'Maximum Favourable Excursion (MFE)'
            }
        },
        tick = {
            .ylab <- 'Profit/Loss (ticks)'
            if(type == 'MAE')
            {
                .cols <- c('tick.MAE','tick.Net.Trading.PL')
                .xlab <- 'Drawdown (ticks)'
                .main <- 'Maximum Adverse Excursion (MAE)'
            }
            else    # type == 'MFE'
            {
                .cols <- c('tick.MFE','tick.Net.Trading.PL')
                .xlab <- 'Run Up (ticks)'
                .main <- 'Maximum Favourable Excursion (MFE)'
            }
        }
    )

    plot(abs(trades[, .cols]), type='n', xlab=.xlab, ylab=.ylab, main=.main)

    grid()

    points(abs(trades[ profitable, .cols]), pch=24, col='green', bg='green', cex=0.6)
    points(abs(trades[!profitable, .cols]), pch=25, col='red', bg='red', cex=0.6)

    abline(a=0, b=1, lty='dashed', col='darkgrey')

    legend(
            x='bottomright', inset=0.1,
            legend=c('Profitable Trade','Losing Trade'),
            pch=c(24,25),
            col=c('green','red'),
            pt.bg=c('green','red')
    )
}

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
        if(includeOpenTrades)
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

# to algorithmically set stops, the classic answer is to calculate quantiles.
# i'm not sure if this belongs in tradeStats, perhaps?  
# perhaps include MFE and MAE stats in tradeStats, plus some quantile information
# for MAE of the 90% and 95% quantiles of profitable trades?

###############################################################################
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
