#' Chart Maximum Adverse/Forward Excursion
#'
#' Produces a scatterplot with one point per trade, with x-axis: absolute 
#' value of Drawdown (Adverse), or RunUp (Favourable), 
#' and y-axis: absolute value of Net Profit or Loss
#'
#' @param Portfolio string identifying the portfolio to chart
#' @param Symbol string identifying the symbol to chart. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param type string specifying MAE (Adverse) or MFE (Favourable) chart type
#' @param scale string specifying 'cash', or 'percent' for percentage of investment
#' @param \dots any other passthrough parameters
#' @author Jan Humme
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6), section 3.5
#' @export
chart.ME <- function(Portfolio, Symbol, type=c('MAE', 'MFE'), scale=c('cash', 'percent'),...)
{   # @author Jan Humme

    trades <- perTradeStats(Portfolio,Symbol)
    

    #multiply Pcct numbers for prettier charting
    trades$Pct.Net.Trading.PL <- 100 * trades$Pct.Net.Trading.PL
    trades$Pct.Drawdown       <- 100 * trades$Pct.Drawdown
    trades$Pct.RunUp          <- 100 * trades$Pct.RunUp
    
    profitable <- (trades$Net.Trading.PL > 0)

    if(type == 'MAE')
    {
        if(scale == 'cash')
        {
            plot(abs(trades[, c('Drawdown','Net.Trading.PL')]), type='n',
                    xlab='Cash Drawdown', ylab='Cash Profit (Loss)',
                    main='Maximum Adverse Excursion (MAE)')

            points(abs(trades[ profitable, c('Drawdown','Net.Trading.PL')]), pch=24, col='green', bg='green', cex=0.6)
            points(abs(trades[!profitable, c('Drawdown','Net.Trading.PL')]), pch=25, col='red', bg='red', cex=0.6)
        }
        else    # scale == 'percent'
        {
            plot(abs(trades[, c('Pct.Drawdown','Pct.Net.Trading.PL')]), type='n',
                    xlab='Drawdown (%)', ylab='Profit (Loss) in %',
                    main='Maximum Adverse Excursion (MAE) in %')

            points(abs(trades[ profitable, c('Pct.Drawdown','Pct.Net.Trading.PL')]), pch=24, col='green', bg='green', cex=0.6)
            points(abs(trades[!profitable, c('Pct.Drawdown','Pct.Net.Trading.PL')]), pch=25, col='red', bg='red', cex=0.6)
        }
    }
    else    # type == 'MFE'
    {
        if(scale == 'cash')
        {
            plot(abs(trades[, c('RunUp','Net.Trading.PL')]), type='n',
                    xlab='Cash RunUp', ylab='Cash Profit (Loss)',
                    main='Maximum Favourable Excursion (MFE)')
    
            points(abs(trades[ profitable, c('RunUp','Net.Trading.PL')]), pch=24, col='green', bg='green', cex=0.6)
            points(abs(trades[!profitable, c('RunUp','Net.Trading.PL')]), pch=25, col='red', bg='red', cex=0.6)
        }
        else    # scale == 'percent'
        {
            plot(abs(trades[, c('Pct.RunUp','Pct.Net.Trading.PL')]), type='n',
                    xlab='RunUp (%)', ylab='Profit (Loss) in %',
                    main='Maximum Favourable Excursion (MFE) in %')
    
            points(abs(trades[ profitable, c('Pct.RunUp','Pct.Net.Trading.PL')]), pch=24, col='green', bg='green', cex=0.6)
            points(abs(trades[!profitable, c('Pct.RunUp','Pct.Net.Trading.PL')]), pch=25, col='red', bg='red', cex=0.6)
        }
    }

    abline(a=0, b=1, lty='dashed', col='darkgrey')

    grid()

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
#'  
#' @param Portfolio string identifying the portfolio to chart
#' @param Symbol string identifying the symbol to chart. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param \dots any other passthrough parameters
#' @author Brian G. Peterson, Jan Hume
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6)
#' @export
perTradeStats <- function(Portfolio, Symbol,...) {
    portf <- getPortfolio(Portfolio)
    
    if(missing(Symbol)) Symbol <- names(portf$symbols)[[1]]
    
    posPL <- portf$symbols[[Symbol]]$posPL
    
    trades <- list()
    
    # identify start and end for each trade, where end means flat position
    trades$Start <- index(posPL[which(posPL$Pos.Value!=0 & lag(posPL$Pos.Value)==0),])
    trades$End <- index(posPL[which(posPL$Pos.Value==0 & lag(posPL$Pos.Value)!=0),])
    
    # discard open last trade, if any
    # trades$Start <- trades$Start[1:length(trades$End)]
    #TODO FIXME if trade is still open at end of series, set end of open trade to the end of the series instead
    if(length(trades$Start)>length(trades$End)){
        trades$End <- c(trades$End,last(index(posPL)))
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
        
        # investment
        trades$Txn.Value[i] <- abs(first(trade$Txn.Value))
        #TODO FIXME this is wrong for trades that level in/out, I think we need na.locf.
        
        #position sizes
        trades$Max.Pos[i] <- trade[which(abs(trade$Pos.Qty)==max(abs(trade$Pos.Qty))),]$Pos.Qty
        trades$Init.Pos[i] <- first(trade)$Pos.Qty
        
        #count number of transactions
        
        # cash-wise
        trades$Net.Trading.PL[i] <- sum(trade$Net.Trading.PL)
        trades$Drawdown[i] <- min(0,cumsum(trade$Net.Trading.PL))
        trades$RunUp[i] <- max(0,cumsum(trade$Net.Trading.PL))
    }
    
    # percentage-wise
    trades$Pct.Net.Trading.PL <- trades$Net.Trading.PL / trades$Txn.Value
    trades$Pct.Drawdown <- trades$Drawdown / trades$Txn.Value
    trades$Pct.RunUp <- trades$RunUp / trades$Txn.Value

    #TODO add tick stats

    trades <- as.data.frame(trades)
    return(trades)
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
