#' Chart Maximum Adverse/Favorable Excursion
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
#' @param legend.loc string specifying the position of legend. If missing, "bottomright" will be used
#' @param trim.ax if FALSE, axes will not be trimmed, else the trim quantile will be used for trimming axes, useful for data with outliers either a single 0-1 floating point number or a c(x,y) option is supported
#' @author Jan Humme
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6), section 3.5
#' @seealso \code{\link{perTradeStats}} for the calculations used by this chart, 
#' and \code{\link{tradeStats}} for a summary view of the performance
#' @export
chart.ME <- function(Portfolio, Symbol, type=c('MAE','MFE'), scale=c('cash','percent','tick'), ..., legend.loc, trim.ax=FALSE)
{   # @author Jan Humme
    
    #edits
    if(missing(legend.loc)) legend.loc <- "bottomright"
    
    type=type[1]
    scale=scale[1] # can only take the first if the user doesn't specify
    
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

    if(!trim.ax){
      .xlim<-NULL
      .ylim<-NULL
    } else {
      if(length(trim_ax)==1) trim_ax[2] <- trim_ax
      .xlim <- c(0,abs(quantile(trades[,.cols[1]],probs=trim_ax[1])))
      .ylim <- c(0,abs(quantile(trades[,.cols[2]],probs=trim_ax[2])))
    }
    
    .main<-paste(Symbol,.main)
    
    plot(abs(trades[, .cols]), type='n', xlab=.xlab, ylab=.ylab, xlim=.xlim, ylim=.ylim, main=.main)

    grid()

    points(abs(trades[ profitable, .cols]), pch=24, col='green', bg='green', cex=0.6)
    points(abs(trades[!profitable, .cols]), pch=25, col='red', bg='red', cex=0.6)

    abline(a=0, b=1, lty='dashed', col='darkgrey')

    legend(
            x=legend.loc, inset=0.1,
            legend=c('Profitable Trade','Losing Trade'),
            pch=c(24,25),
            col=c('green','red'),
            pt.bg=c('green','red')
    )
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
