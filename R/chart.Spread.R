#' @export
chart.Spread <- function(Account, Portfolio, Spread=NULL, Symbols = NULL, Dates = NULL, ...)
{ # @author Peter Carl
    
    pname<-Portfolio
    Portfolio<-getPortfolio(pname,Dates)

    pacctdata<-getPortfAcct(Account,Portfolio=pname, Dates=NULL)
    
    tmp_instr<-getInstrument(Spread)
    if(!inherits(tmp_instr,"spread")) stop (paste("Instrument",Spread," is not a spread, please use the primary_id of a spread."))
    
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
    Prices=get(Spread)
    #buys and sells will be done on the first positive ratio instrument in a spread
    Symbol<-tmp_instr$memberlist$members[which(tmp_instr$memberlist$memberratio>0)][1]
    
    
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
    Prices=align.time(Prices,n) 
    tzero = xts(0,order.by=index(Prices[1,]))
    Trades = Portfolio[[Symbol]]$txn$Txn.Price*Portfolio[[Symbol]]$txn$Txn.Qty
    Buys = Portfolio[[Symbol]]$txn$Txn.Price[which(Trades>0)]
    Buys = align.time(rbind(Buys,tzero),n)[-1]
    #because this is a spread, we need to use the price of the spread at the time of the synthetic 'buy'
    Buys = Prices[index(Buys)]
    Sells = Portfolio[[Symbol]]$txn$Txn.Price[which(Trades<0)]
    Sells = align.time(rbind(Sells,tzero),n)[-1]
    #because this is a spread, we need to use the price of the spread at the time of the synthetic 'sell'
    Sells = Prices[index(Sells)]
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio[[Symbol]]$txn$Txn.Price * (Portfolio[[Symbol]]$txn$Txn.Qty<0) * (Portfolio[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio[[Symbol]]$txn$Txn.Price * (Portfolio[[Symbol]]$txn$Txn.Qty>0) * (Portfolio[[Symbol]]$txn$Pos.Qty==0)
    # 
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    chartSeries(Prices, TA=NULL,...)
    plot(addTA(Buys,pch=2,type='p',col='green', on=1));
    plot(addTA(Sells,pch=6,type='p',col='red', on=1));

    #Position = Portfolio[[Symbol]]$posPL$Pos.Qty # use $txn instead, and make it match the prices index
    i=1
    for(Symbol in tmp_instr$memberlist$members){
        Position = Portfolio[[Symbol]]$txn$Pos.Qty
        Position = na.locf(merge(Position,index(Prices)))
        plot(addTA(Position,type='b',col=i, lwd=1, on=2));
        i=i+1
    }

    #FIXME right now we don't separate trading PL by *Spread*, just by *Portfolios*, so this isn't quite right in the general case
    #TODO change this to use a two-line shaded graphic as soon as Jeff provides the infrastructure
    TradingPL = pacctdata$Trading.PL
    if(length(TradingPL)>1) TradingPL = na.locf(merge(TradingPL,index(Prices)))
    else TradingPL = NULL
    if(!is.null(TradingPL))  plot(addTA(TradingPL, col='darkgreen', lwd=2))
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
