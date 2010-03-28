#' @export
chart.Posn <- function(Portfolio, Symbol = NULL, Dates = NULL, ...)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-getPortfolio(pname)

    # DESCRIPTION
    # Charts the transaction series of a symbol against prices

    # Inputs
    # Portfolio: a portfolio object structured with initPortf()
    # Symbol: an instrument identifier for a symbol included in the portfolio,
    #   e.g., IBM
    # Dates: dates to return the calculation over formatted as xts range

    # Outputs
    # Timeseries object with weights by date in rows and symbolname in columns

    # FUNCTION

    require(quantmod)
    Prices=get(Symbol)
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
    
    Trades = Portfolio[[Symbol]]$txn$Txn.Price*Portfolio[[Symbol]]$txn$Txn.Qty
    Buys = Portfolio[[Symbol]]$txn$Txn.Price[which(Trades>0)]
#    Buys = align.time(rbind(Buys,tzero),n)[-1]
    Sells = Portfolio[[Symbol]]$txn$Txn.Price[which(Trades<0)]
#    Sells = align.time(rbind(Sells,tzero),n)[-1]
    #Position = Portfolio[[Symbol]]$posPL$Pos.Qty # use $txn instead, and make it match the prices index
    Position = Portfolio[[Symbol]]$txn$Pos.Qty
    Position = na.locf(merge(Position,index(Prices)))
    CumPL = cumsum(Portfolio[[Symbol]]$posPL$Net.Trading.PL)
    if(length(CumPL)>1)
        CumPL = na.locf(merge(CumPL,index(Prices)))
    else 
        CumPL = NULL
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio[[Symbol]]$txn$Txn.Price * (Portfolio[[Symbol]]$txn$Txn.Qty<0) * (Portfolio[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio[[Symbol]]$txn$Txn.Price * (Portfolio[[Symbol]]$txn$Txn.Qty>0) * (Portfolio[[Symbol]]$txn$Pos.Qty==0)
    # 
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    # scope the date, this is heavy-handed, but should work
    if(!is.null(Dates)) Prices=Prices[Dates]
    
    chart_Series(Prices, TA=NULL,...)
    if(nrow(Buys)>=1) plot(add_TA(Buys,pch=2,type='p',col='green', on=1));
    if(nrow(Sells)>=1) plot(add_TA(Sells,pch=6,type='p',col='red', on=1));
    if(nrow(Position)>=1) plot(add_TA(Position,type='b',col='blue', lwd=2));
    if(!is.null(CumPL))  plot(add_TA(CumPL, col='darkgreen', lwd=2))
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