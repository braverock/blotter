#' Chart trades against market data, position through time, and cumulative P\&L
#'
#' Produces a three-panel chart of time series charts that contains prices and transactions in the top panel, the resulting position in the second, and a cumulative profit-loss line chart in the third.
#' @note Expect changes to this function, since the underlying charts are experimental functions in quantmod.
#'
#' @param Portfolio string identifying the portfolio to chart
#' @param Symbol string identifying the symbol to chart. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param Dates xts ISO 8601 style subsetting
#' @param \dots any other passthru parameters to \code{\link[quantmod]{chart_Series}}
#' @export
chart.Posn <- function(Portfolio, Symbol, Dates = NULL, ...)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-getPortfolio(pname)
    if (missing(Symbol)) Symbol <- names(Portfolio$symbols)[[1]]
    # FUNCTION

    require(quantmod)
    Prices=get(Symbol)
    if(!is.OHLC(Prices)) Prices=getPrice(Prices, ...=...)
    freq = periodicity(Prices)
    switch(freq$scale,
            seconds = { mult=1 },
            minute = { mult=60 },
            hourly = { mult=3600 },
            daily = { mult=86400 },
            {mult=86400}
    )
    if(!isTRUE(freq$frequency*mult == round(freq$frequency,0)*mult)) { 
        # if the equality
        n=round((freq$frequency/mult),0)*mult
    } else { n=mult }
    
    tzero = xts(0,order.by=index(Prices[1,]))
#    Prices=align.time(Prices,n) 
    if(is.null(Dates)) Dates<-paste(first(index(Prices)),last(index(Prices)),sep='::')
    
    #scope the data by Dates
    Portfolio$symbols[[Symbol]]$txn<-Portfolio$symbols[[Symbol]]$txn[Dates]
    Portfolio$symbols[[Symbol]]$posPL<-Portfolio$symbols[[Symbol]]$posPL[Dates]
    
	Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
	
	Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades>0)]
#    Buys = align.time(rbind(Buys,tzero),n)[-1]
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades<0)]
#    Sells = align.time(rbind(Sells,tzero),n)[-1]
    #Position = Portfolio$symbols[[Symbol]]$posPL$Pos.Qty # use $txn instead, and make it match the prices index
    Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
    Positionfill = na.locf(merge(Position,index(Prices)))
    CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
    if(length(CumPL)>1)
        CumPL = na.locf(merge(CumPL,index(Prices)))
    else 
        CumPL = NULL
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty<0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty>0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # 
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    # scope the Price data by Dates
    if(!is.null(Dates)) Prices=Prices[Dates]
    
    chart_Series(Prices, name=Symbol, TA=NULL, ...)
    if(!is.null(nrow(Buys)) && nrow(Buys) >=1 ) (add_TA(Buys,pch=2,type='p',col='green', on=1));
    if(!is.null(nrow(Sells)) && nrow(Sells) >= 1) (add_TA(Sells,pch=6,type='p',col='red', on=1));
    if(nrow(Position)>=1) {
        (add_TA(Positionfill,type='l',col='blue', lwd=2))   
        (add_TA(Position,type='p',col='blue', lwd=2, on=2))   
    }
    if(!is.null(CumPL))  (add_TA(CumPL, col='darkgreen', lwd=2))
    plot(current.chob())
}

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
