#' Chart trades against market data, position through time, and cumulative P\&L
#'
#' Produces a three or four-panel or chart of time series charts that contains prices and transactions in the top panel, 
#' the resulting position in the second, a cumulative profit-loss line chart in the third.  
#' 
#' The theoretical trades, positions, and P&L are plotted first, in the 'light' versions of the colors, and then the actual values are overplotted in the main color.  
#' If they agree completely, the theoretical values will not be visible.  Differences will make themselves visible by misalignment of the symbols or lines. 
#' 
#' The fourth panel is the difference in P&L between the theoretical and actual values, and could be considered 'slippage', which could be positive or negative.  
#' It is calculated by subtracting the theoretical P&L from the actual P&L.  
#' If parameter \code{PLdiff} is 'cumulative', then this panel will display the cumsum of difference between the theoretical and actual portfolios.
#' If parameter \code{PLdiff} is 'episodic' it will display the differences in P&L    
#'
#' The \code{data} parameter allows the user to either \code{\link{View}} or \code{\link{return}} the data calculated inside the chart.  Default FALSE (only chart)
#' 
#' @param theoPort string identifying the theoretical portfolio to chart
#' @param actualPort string identifying the actual portfolio to chart
#' @param Symbol string identifying the symbol to chart
#' @param Dates xts ISO 8601 style subsetting 
#' @param \dots any other passthru parameters to \code{\link[quantmod]{chart_Series}}
#' @param PLdiff one of 'cumulative' or 'episodic', see Details.
#' @param data what to do with the calculated data, see Details
#' @seealso \code{\link{chart.Posn}}
#' @export
#' @note Expect changes to this function, since the underlying charts are experimental functions in quantmod.
chart.Reconcile <- function(theoPort, actualPort, Symbol, Dates = NULL, ..., PLdiff=c('cumulative', 'episodic'),data=c(FALSE,'View','return'))
{ # @author Peter Carl, Brian G. Peterson
    pname     <- theoPort
    aname     <- actualPort
    Portfolio <- getPortfolio(pname)
    Actual    <- getPortfolio(aname)
    
    PLdiff <- PLdiff[1]
    data   <- data[1]

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
    #uses the first column of Prices, hopefully unecessary, as getPrice should only have one col

    Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Value
	ATrades = Actual$symbols[[Symbol]]$txn$Txn.Value
    
	Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades>0)]
    ABuys = Actual$symbols[[Symbol]]$txn$Txn.Price[which(ATrades>0)]
    
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades<0)]
    ASells = Actual$symbols[[Symbol]]$txn$Txn.Price[which(ATrades<0)]
    
    Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
    Positionfill = na.locf(merge(Position,index(Prices)))
    CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)

    ActPos = Actual$symbols[[Symbol]]$txn$Pos.Qty
    ActPosfill = na.locf(merge(ActPos,index(Prices)))
    ActCumPL = cumsum(Actual$symbols[[Symbol]]$posPL$Net.Trading.PL)
    
    PLdifference<-NULL
    PLslippage<-NULL
    
    if(length(CumPL)>1){
        CumPL = na.locf(merge(CumPL,index(Prices)))
        ActCumPL = na.locf(merge(ActCumPL,index(Prices)))
        PLdifference=ActCumPL-CumPL
        if(PLdiff=='episodic' | PLdiff == 'both'){
            #browser()
            poschange<- ActPosfill-Positionfill
            diffchange<-diff(poschange[diff(poschange)!=0])
            tmpidx<-index(poschange[!poschange==0])
            PLslippage <- PLdifference[index(poschange[poschange!=0])]
            #like drawdowns here? cumpl - cummax?  detect starting PL?
            difftable<-NULL
            if(length(diffchange)){
                for(i in seq(1,length(diffchange),by=2)){
                    tmpidx<-data.frame(index(diffchange[i]),index(diffchange[i+1]))
                    if(is.null(difftable)) difftable<-tmpidx
                    else difftable <- rbind(difftable,tmpidx)
                }
                colnames(difftable)<-c('from','to')
                difftable$Net.difference <- t(t(PLdifference[difftable$to]) - t(PLdifference[difftable$from]))
                colnames(difftable)<-c('from','to','Net.difference')
                difftable$Abs.difference <- abs(difftable$Net.difference)
                for (i in 1:nrow(difftable)) difftable$Max.Abs.difference[i] <- max(abs(PLdifference[paste(difftable$from[i],difftable$to[i],sep='/')]))
                ## TODO calc period difference and accumulate that?
                colnames(difftable)<-c('from','to','Net.difference','Abs.difference','Max.Abs.difference')
                attr(difftable$Net.difference,'dimnames')<-NULL
                attr(difftable$Abs.difference,'dimnames')<-NULL
            }
        }
    } else {
        CumPL = NULL
    }
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty<0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty>0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # 
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    # scope the date, this is heavy-handed, but should work
    if(!is.null(Dates)) Prices=Prices[Dates]
    
    chart_Series(Prices, name=Symbol, TA=NULL)
    if(nrow(Buys)>=1) {
        (add_TA(Buys,pch=2,type='p',col='lightgreen', on=1))    
        (add_TA(ABuys,pch=2,type='p',col='green', on=1))    
    }
    if(nrow(Sells)>=1){
        (add_TA(Sells,pch=6,type='p',col='lightsalmon', on=1))    
        (add_TA(ASells,pch=6,type='p',col='red', on=1))    
    } 
    if(nrow(Position)>=1) {
        (add_TA(Positionfill,type='l',col='lightblue', lwd=2))   
        (add_TA(Position,type='p',col='lightblue', lwd=2, on=2))   
        (add_TA(ActPosfill,type='l',col='blue', lwd=2,on=2))   
        (add_TA(ActPos,type='p',col='blue', lwd=2, on=2))   
    }
    
    
    if(!is.null(CumPL)) {
		(add_TA(ActCumPL, col='darkgreen', lwd=2))
        (add_TA(CumPL, col='lightgreen', lwd=2, on=3))
		(add_TA(ActCumPL, col='darkgreen', lwd=2, on=3))
        if(!is.null(PLdifference)){
            (add_TA(PLdifference, col='lightsalmon', lwd=2))
        }  
        if(!is.null(PLslippage)){
            #TODO separate these into positive and negative slippage, and have green/red colors for them
            (add_TA(PLslippage, col='lightsalmon', lwd=2))
        }
    } 
    plot(current.chob())
    
    if(data!=FALSE){
        #output<-cbind(ActCumPL,CumPL,PLdifference,PLslippage,ActPosfill,Positionfill)
        #colnames(output) <- c('cumPL','theoCumPL','PLdiff','PLslippage','position','theo_position')
        output<-difftable
        switch(data,
               View = View(output),
               return = return(output)
        )
    }
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
