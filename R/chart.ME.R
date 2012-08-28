#' Chart Maximum Adverse/Forward Excursion
#'
#' Produces a scatterplot with one point per trade, with x-axis: absolute value of Drawdown (Adverse), or RunUp (Favourable), and y-axis: absolute value of Net Profit or Loss
#'
#' After Jaekle & Tomasini: Trading Systems - A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6), paragraph 3.5
#'
#' @param Portfolio string identifying the portfolio to chart
#' @param Symbol string identifying the symbol to chart. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param type string specifying MAE (Adverse) or MFE (Favourable) chart type
#' @param scale string specifying 'cash', or 'percent' for percentage of investment
#' @export
chart.ME <- function(portfolio, symbol, type=c('MAE', 'MFE'), scale=c('cash', 'percent'))
{   # @author Jan Humme

    require(xts)

    portf <- getPortfolio(portfolio)

    if(missing(symbol)) symbol <- names(portf$symbols)[[1]]
    
    posPL <- portf$symbols[[symbol]]$posPL

    trades <- list()

    # identify start and end for each trade, where end means flat position
    trades$Start <- index(posPL[which(posPL$Pos.Value!=0 & lag(posPL$Pos.Value)==0),])
    trades$End <- index(posPL[which(posPL$Pos.Value==0 & lag(posPL$Pos.Value)!=0),])

    # discard open last trade, if any
    trades$Start <- trades$Start[1:length(trades$End)]

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

        # cash-wise
        trades$Net.Trading.PL[i] <- sum(trade$Net.Trading.PL)
        trades$Drawdown[i] <- min(0,cumsum(trade$Net.Trading.PL))
        trades$RunUp[i] <- max(0,cumsum(trade$Net.Trading.PL))
    }

    # percentage-wise
    trades$Pct.Net.Trading.PL <- 100 * trades$Net.Trading.PL / trades$Txn.Value
    trades$Pct.Drawdown <- 100 * trades$Drawdown / trades$Txn.Value
    trades$Pct.RunUp <- 100 * trades$RunUp / trades$Txn.Value

    trades <- as.data.frame(trades)

    profitable <- (trades$Net.Trading.PL > 0)

    if(type == 'MAE')
    {
        if(scale == 'cash')
        {
            plot(abs(trades[, c('Drawdown','Net.Trading.PL')]), type='n',
                    xlab='Drawdown ($)', ylab='Profit (Loss) in $',
                    main='Maximum Adverse Excursion (MAE) in $')

            points(abs(trades[ profitable, c('Drawdown','Net.Trading.PL')]), pch=2, col='green')
            points(abs(trades[!profitable, c('Drawdown','Net.Trading.PL')]), pch=25, col='red')
        }
        else    # scale == 'percent'
        {
            plot(abs(trades[, c('Pct.Drawdown','Pct.Net.Trading.PL')]), type='n',
                    xlab='Drawdown (%)', ylab='Profit (Loss) in %',
                    main='Maximum Adverse Excursion (MAE) in %')

            points(abs(trades[ profitable, c('Pct.Drawdown','Pct.Net.Trading.PL')]), pch=2, col='green')
            points(abs(trades[!profitable, c('Pct.Drawdown','Pct.Net.Trading.PL')]), pch=25, col='red')
        }
    }
    else    # type == 'MFE'
    {
        if(scale == 'cash')
        {
            plot(abs(trades[, c('RunUp','Net.Trading.PL')]), type='n',
                    xlab='RunUp ($)', ylab='Profit (Loss) in $',
                    main='Maximum Favourable Excursion (MFE) in $')
    
            points(abs(trades[ profitable, c('RunUp','Net.Trading.PL')]), pch=2, col='green')
            points(abs(trades[!profitable, c('RunUp','Net.Trading.PL')]), pch=25, col='red')
        }
        else    # scale == 'percent'
        {
            plot(abs(trades[, c('Pct.RunUp','Pct.Net.Trading.PL')]), type='n',
                    xlab='RunUp (%)', ylab='Profit (Loss) in %',
                    main='Maximum Favourable Excursion (MFE) in %')
    
            points(abs(trades[ profitable, c('Pct.RunUp','Pct.Net.Trading.PL')]), pch=2, col='green')
            points(abs(trades[!profitable, c('Pct.RunUp','Pct.Net.Trading.PL')]), pch=25, col='red')
        }
    }

    grid()

    legend(
            x='right', inset=0.1,
            legend=c('Profitable Trade','Losing Trade'),
            pch=c(2,6),
            col=c('green','red')
    )
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Posn.R 855 2011-11-28 23:41:46Z bodanker $
#
###############################################################################
