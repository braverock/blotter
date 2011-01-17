#' Chart trades against market data, position through time, and cumulative P\&L
#'
#' Produces a three-panel chart of time series charts that contains prices and transactions in the top panel, the resulting position in the second, and a cumulative profit-loss line chart in the third.
#'
#' @param theoPort string identifying the theoretical portfolio to chart
#' @param actualPort string identifying the actual portfolio to chart
#' @param Symbol string identifying the symbol to chart
#' @param Dates currently not used
#' @param \dots any other passthru parameters to \code{\link[quantmod]{chart_Series}}
#' @seealso \code{\link{chart.Posn}}
#' @export
#' @note Expect changes to this function, since the underlying charts are experimental functions in quantmod.
chart.Reconcile <- function(theoPort, actualPort, Symbol, Dates = NULL, ...)
{ # @author Peter Carl, Brian G. Peterson
    pname<-theoPort
    aname<-actualPort
    Portfolio<-getPortfolio(pname)
    Actual<-getPortfolio(aname)
    
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

    Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Value
	ATrades = Actual$symbols[[Symbol]]$txn$Txn.Value
    
	Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades>0)]
    ABuys = Actual$symbols[[Symbol]]$txn$Txn.Price[which(Trades>0)]
    
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades<0)]
    ASells = Actual$symbols[[Symbol]]$txn$Txn.Price[which(Trades<0)]
    
    Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
    Positionfill = na.locf(merge(Position,index(Prices)))
    CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)

    ActPos = Actual$symbols[[Symbol]]$txn$Pos.Qty
    ActPosfill = na.locf(merge(ActPos,index(Prices)))
    ActCumPL = cumsum(Actual$symbols[[Symbol]]$posPL$Net.Trading.PL)
    
    if(length(CumPL)>1){
        CumPL = na.locf(merge(CumPL,index(Prices)))
        ActCumPL = na.locf(merge(ActCumPL,index(Prices)))
        PLdifference=ActCumPL-CumPL
    } else CumPL = NULL
    
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty<0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty>0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # 
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    # scope the date, this is heavy-handed, but should work
    if(!is.null(Dates)) Prices=Prices[Dates]
    
    chart_Series(Prices, name=Symbol, TA=NULL,...)
    if(nrow(Buys)>=1) {
        (add_TA(Buys,pch=2,type='p',col='lightgreen', on=1))    
        (add_TA(ABuys,pch=2,type='p',col='green', on=1))    
    }
    if(nrow(Sells)>=1){
        (add_TA(Sells,pch=6,type='p',col='lightred', on=1))    
        (add_TA(ASells,pch=6,type='p',col='red', on=1))    
    } 
    if(nrow(Position)>=1) {
        (add_TA(Positionfill,type='l',col='lightblue', lwd=2))   
        (add_TA(Position,type='p',col='lightblue', lwd=2, on=2))   
        (add_TA(ActPosfill,type='l',col='blue', lwd=2,on=2))   
        (add_TA(ActPos,type='p',col='blue', lwd=2, on=2))   
    }
    
    
    if(!is.null(CumPL)) {
        (add_TA(CumPL, col='darkgreen', lwd=2))
        if(!is.null(PLdifference)){
            (add_TA(PLdifference, col='darkgreen', lwd=2))
        }    
    } 
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
# $Id: chart.Posn.R 531 2011-01-14 16:42:23Z llevenson $
#
###############################################################################
