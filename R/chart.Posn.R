`chart.Posn` <- 
function(Portfolio, Symbol = NULL, Dates = NULL, ...)
{ # @author Peter Carl

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
    # @TODO: check that Portfolio is a Portfolio object
    # @TODO: add date scoping
    Prices=get(Symbol)

    Buys = Portfolio[[Symbol]]$txn$Txn.Price*(Portfolio[[Symbol]]$txn$Txn.Qty>0)
    Sells = Portfolio[[Symbol]]$txn$Txn.Price*(Portfolio[[Symbol]]$txn$Txn.Qty<0)
    Position = Portfolio[[Symbol]]$posPL$Pos.Qty
    CumPL = cumsum(Portfolio[[Symbol]]$posPL$Trading.PL)
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
    plot(addTA(Position,type='h',col='blue', lwd=2));
    plot(addTA(CumPL, col='darkgreen', lwd=2))
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: $
#
###############################################################################