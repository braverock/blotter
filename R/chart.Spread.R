#' Charts the transaction series, positions, and P&L of a spread against prices
#' @param Account string identifying the account
#' @param Portfolio string identifying the portfolio to chart
#' @param Symbols string identifying the underlying symbols to chart for positions
#' @param Spread identifier of a spread instrument
#' @param Dates date range, currently not used
#' @param \dots any other passthru parameters (typically parameters to \code{chart_Series})
#' @export
chart.Spread <- function(Account, Portfolio, Spread=NULL, Symbols = NULL, Dates = NULL, ...)
{ # @author Peter Carl
    
    pname<-Portfolio
    Portfolio<-getPortfolio(pname,Dates)

    pacctdata<-.getPortfAcct(Account,Portfolio=pname, Dates=NULL)
    
    tmp_instr<-getInstrument(Spread)
    if(!inherits(tmp_instr,"spread")) stop (paste("Instrument",Spread," is not a spread, please use the primary_id of a spread."))
    

    require(quantmod)
    Prices=get(Spread)
    #buys and sells will be done on the first positive ratio instrument in a spread
    Symbol<-as.character(tmp_instr$memberlist$members[which(tmp_instr$memberlist$memberratio>0)][1])
    
    
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
    Prices=align.time(Prices,n) 
    Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Price*Portfolio$symbols[[Symbol]]$txn$Txn.Qty
    Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades>0)]
    Buys = align.time(rbind(Buys,tzero),n)[-1]
    #because this is a spread, we need to use the price of the spread at the time of the synthetic 'buy'
    Buys = Prices[unique(Hmisc::find.matches(index(Buys),index(Prices))[[1]])]
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades<0)]
    Sells = align.time(rbind(Sells,tzero),n)[-1]
    #because this is a spread, we need to use the price of the spread at the time of the synthetic 'sell'
    Sells = Prices[unique(Hmisc::find.matches(index(Sells),index(Prices))[[1]])]
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty<0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty>0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # 
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    chartSeries(Prices, TA=NULL,theme='white',...)
    plot(addTA(Buys,pch=2,type='p',col='green', on=1));
    plot(addTA(Sells,pch=6,type='p',col='red', on=1));

    #Position = Portfolio$symbols[[Symbol]]$posPL$Pos.Qty # use $txn instead, and make it match the prices index
    i=1
    for(Symbol in tmp_instr$memberlist$members){
        Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
        Position = na.locf(merge(Position,index(Prices)))
        plot(addTA(Position,type='b',col=i, lwd=1));
        i=i+1
    }

    #FIXME right now we don't separate trading PL by *Spread*, just by *Portfolios*, so this isn't quite right in the general case
    #TODO change this to use a two-line shaded graphic as soon as Jeff provides the infrastructure
    UnrealizedPL = pacctdata$Unrealized.PL

    if(length(UnrealizedPL)>1) UnrealizedPL = na.locf(merge(UnrealizedPL,index(Prices)))
    else UnrealizedPL = NULL
    if(!is.null(UnrealizedPL))  plot(addTA(UnrealizedPL, col='darkgreen', lwd=2))

    RealizedPL = pacctdata$Realized.PL
    CumPL=cumsum(RealizedPL+UnrealizedPL)
    if(length(CumPL)>1) CumPL = na.locf(merge(CumPL,index(Prices)))
    else CumPL = NULL
    if(!is.null(CumPL))  plot(addTA(CumPL, col='green', lwd=2))
    
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
