
#' calculate statistics on transactions and P&L for a symbol or symbols in a portfolio or portfolios
#' 
#' This function calculates trade-level statistics on a symbol or symbols within a portfolio or portfolios.
#' 
#' Every book on trading, broker report on an analytical trading system, 
#' or blog post seems to have a slightly different idea of what trade statistics 
#' are necessary, and how they should be displayed.  We choose not to make 
#' value judgments of this type, aiming rather for inclusiveness with 
#' post-processing for display.
#'   
#' The output of this function is a \code{\link{data.frame}} with named columns for each statistic.  
#' Each row is a single portfolio+symbol combination. Values are returned in full precision.
#' It is likely that the output of this function will have more than you wish
#' to display in all conditions, but it should be suitable for reshaping for display.  
#' Building summary reports from this data.frame may be easily accomplished using 
#' something like \code{textplot} or \code{\link{data.frame}}, with rounding, 
#' fancy formatting, etc. as your needs dictate.
#' 
#' If you have additional trade statistics you want added here, please share.  
#' We find it unlikely that any transaction-level statistics that can be  
#' calculated independently of strategy rules could be considered proprietary.
#' 
#' Special Thanks for contributions to this function from:
#' \describe{
#'   \item{Josh Ulrich}{ for adding multiple-portfolio support, fixing bugs, and improving readability of the code }
#'   \item{Klemen Koselj}{ for median stats, num trades, and win/loss ratios }
#'   \item{Mark Knecht}{ for suggesting Profit Factor and largest winner/largest loser }
#' }  
#' 
#' WARNING: we're not sure this function is stable/complete yet.  If you're using it, please give us feedback!
#' 
#' @aliases dailyStats
#' @param Portfolios portfolio string 
#' @param Symbols character vector of symbol strings, default NULL
#' @param use for dailyStats, determines whether numbers are calculated from trades or equity curve
#' @author Lance Levenson
#' @export
#' @importFrom zoo as.Date
#' @note
#' TODO document each statistic included in this function, with equations 
#' 
#' TODO add more stats, potentially
#' PerformanceAnalytics: skewness, kurtosis, upside/downside semidieviation, Sharpe/Sortino
#' 
#' mean absolute deviation stats
#' 
#' more Tharpe/Kestner/Tradestation stats, e.g.
#' K-factor
#' RINA Index
#' Percent time in the market
#' Buy and hold return
#' 
#' Josh has suggested adding \%-return based stats too
tradeStats <- function(Portfolios, Symbols)
{
    ret<-NULL
    for (Portfolio in Portfolios){
        ## Error Handling Borrowed from getPortfolio
        pname <- Portfolio
        if (!grepl("portfolio\\.", pname)) 
		Portfolio <- try(get(paste("portfolio", pname, sep = "."), 	envir = .blotter))
    	else Portfolio <- try(get(pname, envir = .blotter))
    	if (inherits(Portfolio, "try-error")) 
    		stop(paste("Portfolio", pname, " not found, use initPortf() to create a new portfolio"))
        if (!inherits(Portfolio, "portfolio")) 
                stop("Portfolio", pname, "passed is not the name of a portfolio object.")
        
        
        
        ## FIXME: need a way to define symbols for each portfolio    
        if(missing(Symbols)) symbols <- names(Portfolio$symbols)
        else symbols <- Symbols
        
        ## Trade Statistics
        for (symbol in symbols){
            txn <- Portfolio$symbols[[symbol]]$txn
            posPL <- Portfolio$symbols[[symbol]]$posPL
            posPL <- posPL[-1,]

            PL.gt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL  > 0]
            PL.lt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL  < 0]
            PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL != 0]
            if(!nrow(PL.ne0))next()
            
            GrossProfits <- sum(PL.gt0)
            GrossLosses  <- sum(PL.lt0)
            ProfitFactor <- abs(GrossProfits/GrossLosses)
            
            AvgTradePL <- mean(PL.ne0)
            MedTradePL <- median(PL.ne0)
            StdTradePL <- sd(as.numeric(as.vector(PL.ne0)))   
            
            NumberOfTrades <- nrow(txn)-1
            
            PercentPositive <- (nrow(PL.gt0)/nrow(PL.ne0))*100
            PercentNegative <- (nrow(PL.lt0)/nrow(PL.ne0))*100
            
            MaxWin <- max(txn$Net.Txn.Realized.PL)
            MaxLoss <- min(txn$Net.Txn.Realized.PL)
            
            AvgWinTrade <- mean(PL.gt0)
            MedWinTrade <- median(PL.gt0)
            AvgLossTrade <- mean(PL.lt0)
            MedLossTrade <- median(PL.lt0)
            
            AvgWinLoss <- AvgWinTrade/-AvgLossTrade
            MedWinLoss <- MedWinTrade/-MedLossTrade
            
            DailyPL <- apply.daily(PL.ne0,sum)
            AvgDailyPL <- mean(DailyPL)
            MedDailyPL <- median(DailyPL)
            StdDailyPL <- sd(as.numeric(as.vector(DailyPL)))
            
            Equity <- cumsum(posPL$Net.Trading.PL)
            if(!nrow(Equity)){
                warning('No Equity rows for',symbol)
                next()
            }    
            TotalNetProfit <- last(Equity)
            if(is.na(TotalNetProfit)) {
                warning('TotalNetProfit NA for',symbol)
                next()
            }
            Equity.max <- cummax(Equity)
            maxEquity <- max(Equity)
            minEquity <- min(Equity)
            endEquity <- last(Equity)
            if(endEquity!=TotalNetProfit && last(txn$Pos.Qty)==0) {
                warning('Total Net Profit for',symbol,'from transactions',TotalNetProfit,'and cumulative P&L from the Equity Curve', endEquity, 'do not match. This can happen in long/short portfolios.')
                message('Total Net Profit for',symbol,'from transactions',TotalNetProfit,'and cumulative P&L from the Equity Curve', endEquity, 'do not match. This can happen in long/short portfolios.')
                
            }# if we're flat, these numbers should agree 
            #TODO we should back out position value if we've got an open position and double check here....
	
            MaxDrawdown <- -max(Equity.max - Equity)
            ProfitToMaxDraw <- -TotalNetProfit / MaxDrawdown
                
            #TODO add skewness, kurtosis, and positive/negative semideviation if PerfA is available.

            tmpret <- data.frame(Portfolio=pname, 
                                 Symbol=symbol,
                                 Num.Trades=NumberOfTrades,
                                 Total.Net.Profit=TotalNetProfit,
                                 Avg.Trade.PL=AvgTradePL,
                                 Med.Trade.PL=MedTradePL,
                                 Largest.Winner=MaxWin,
                                 Largest.Loser=MaxLoss,
                                 Gross.Profits=GrossProfits,
                                 Gross.Losses=GrossLosses,
                                 Std.Dev.Trade.PL=StdTradePL,
                                 Percent.Positive=PercentPositive,
                                 Percent.Negative=PercentNegative,
                                 Profit.Factor=ProfitFactor,
                                 Avg.Win.Trade=AvgWinTrade,
                                 Med.Win.Trade=MedWinTrade,
                                 Avg.Losing.Trade=AvgLossTrade,
                                 Med.Losing.Trade=MedLossTrade,
                                 Avg.Daily.PL=AvgDailyPL,
                                 Med.Daily.PL=MedDailyPL,
                                 Std.Dev.Daily.PL=StdDailyPL,
                                 maxDrawdown=MaxDrawdown,
                                 Profit.to.Max.Draw=ProfitToMaxDraw,
                                 Avg.WinLoss.Ratio=AvgWinLoss,
                                 Med.WinLoss.Ratio=MedWinLoss,
                                 Max.Equity=maxEquity,
                                 Min.Equity=minEquity,
                                 End.Equity=endEquity)
            rownames(tmpret)<-symbol             
            ret <- rbind(ret,tmpret)
        } # end symbol loop
    } # end portfolio loop
    return(ret)
}

#' generate daily Transaction Realized or Equity Curve P&L by instrument
#' 
#' designed to collate information for high frequency portfolios
#' 
#' @aliases dailyEqPL
#' @param Portfolios portfolio string 
#' @param Symbols character vector of symbol strings
#' @param drop.time remove time component of POSIX datestamp (if any), default TRUE 
#' @author Brian G. Peterson
#' @seealso tradeStats
#' @export
dailyTxnPL <- function(Portfolios, Symbols, drop.time=TRUE)
{
    ret<-NULL
    for (Portfolio in Portfolios){
        ## Error Handling Borrowed from getPortfolio
        pname <- Portfolio
        if (!grepl("portfolio\\.", pname)) 
            Portfolio <- try(get(paste("portfolio", pname, sep = "."),  envir = .blotter))
        else Portfolio <- try(get(pname, envir = .blotter))
        if (inherits(Portfolio, "try-error")) 
            stop(paste("Portfolio", pname, " not found, use initPortf() to create a new portfolio"))
        if (!inherits(Portfolio, "portfolio")) 
            stop("Portfolio", pname, "passed is not the name of a portfolio object.")
        
        
        
        ## FIXME: need a way to define symbols for each portfolio    
        if(missing(Symbols)) symbols <- names(Portfolio$symbols)
        else symbols <- Symbols
        
        ## Trade Statistics
        for (symbol in symbols){
            txn <- Portfolio$symbols[[symbol]]$txn
            txn <- txn[-1,] # remove initialization row
            
            PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL != 0]
            if(!nrow(PL.ne0)){
                warning('No P&L rows for',symbol)
                next()
            }             
            DailyPL <- apply.daily(PL.ne0,sum)
            colnames(DailyPL)<-paste(symbol,'DailyTxnPL',sep='.')
            if(is.null(ret)) ret=DailyPL else ret<-cbind(ret,DailyPL)
            
        } # end symbol loop
    } # end portfolio loop
    ret<-apply.daily(ret,colSums,na.rm=TRUE)  
    if(drop.time) index(ret)<-as.Date(index(ret))
    return(ret)
}

#' @rdname dailyTxnPL
#' @export
dailyEqPL <- function(Portfolios, Symbols, drop.time=TRUE)
{
    ret<-NULL
    for (Portfolio in Portfolios){
        ## Error Handling Borrowed from getPortfolio
        pname <- Portfolio
        if (!grepl("portfolio\\.", pname)) 
            Portfolio <- try(get(paste("portfolio", pname, sep = "."),  envir = .blotter))
        else Portfolio <- try(get(pname, envir = .blotter))
        if (inherits(Portfolio, "try-error")) 
            stop(paste("Portfolio", pname, " not found, use initPortf() to create a new portfolio"))
        if (!inherits(Portfolio, "portfolio")) 
            stop("Portfolio", pname, "passed is not the name of a portfolio object.")
        
        
        
        ## FIXME: need a way to define symbols for each portfolio    
        if(missing(Symbols)) symbols <- names(Portfolio$symbols)
        else symbols <- Symbols
        
        ## Trade Statistics
        for (symbol in symbols){
            posPL <- Portfolio$symbols[[symbol]]$posPL
            posPL <- posPL[-1,] # remove initialization row
            
            Equity <- cumsum(posPL$Net.Trading.PL)
            if(!nrow(Equity)){
                warning('No P&L rows for',symbol)
                next()
            }             
            
            #DailyPL <- apply.daily(Equity,last)
            DailyPL <- apply.daily(posPL$Net.Trading.PL,colSums)
            colnames(DailyPL)<-paste(symbol,'DailyEndEq',sep='.')
            if(is.null(ret)) ret=DailyPL else ret<-cbind(ret,DailyPL)
            
        } # end symbol loop
    } # end portfolio loop
    ret<-apply.daily(ret,colSums,na.rm=TRUE)  
    if(drop.time) index(ret)<-as.Date(index(ret))
    return(ret)
}

#' @rdname tradeStats
#' @export
dailyStats <- function(Portfolios,use=c('Equity','Txns'))
{
    use=use[1] #take the first value if the user didn't specify
    switch (use,
            Eq = , Equity =, cumPL = {
                dailyPL<-dailyEqPL(Portfolios)
            },
            Txns =, Trades = {
                dailyPL<-dailyTxnPL(Portfolios)
            }
            )
    
    dailyFUN <- function (x){
        x<-t(t(x))
        PL.gt0 <- x[x  > 0]
        PL.lt0 <- x[x  < 0]
        PL.ne0 <- x[x != 0]
        
        TotalNetProfit <- sum(x)
        
        GrossProfits <- sum(PL.gt0)
        GrossLosses  <- sum(PL.lt0)
        ProfitFactor <- abs(GrossProfits/GrossLosses)
        
        AvgDayPL <- as.numeric(mean(PL.ne0))
        MedDayPL <- as.numeric(median(PL.ne0))
        StdDayPL <- as.numeric(sd(PL.ne0))   
        
        #NumberOfDays <- nrow(txn)
        WinDays <-length(PL.gt0)
        LossDays<-length(PL.lt0)
        PercentPositive <- (length(PL.gt0)/length(PL.ne0))*100
        PercentNegative <- (length(PL.lt0)/length(PL.ne0))*100
        
        MaxWin <- max(x)
        MaxLoss <- min(x)
        
        AvgWinDay <- as.numeric(mean(PL.gt0))
        MedWinDay <- as.numeric(median(PL.gt0))
        AvgLossDay <- as.numeric(mean(PL.lt0))
        MedLossDay <- as.numeric(median(PL.lt0))
        
        AvgWinLoss <- AvgWinDay/-AvgLossDay
        MedWinLoss <- MedWinDay/-MedLossDay
        
        AvgDailyPL <- as.numeric(mean(PL.ne0))
        MedDailyPL <- as.numeric(median(PL.ne0))
        StdDailyPL <- as.numeric(sd(PL.ne0))
        
        Equity <- cumsum(x)
        Equity.max <- cummax(Equity)
        maxEquity <- max(Equity)
        minEquity <- min(Equity)
        endEquity <- as.numeric(last(Equity))
        MaxDrawdown <- -max(Equity.max - Equity)
        ProfitToMaxDraw <- -TotalNetProfit / MaxDrawdown
        
        tmpret <- data.frame(
                Total.Net.Profit=TotalNetProfit,
                Total.Days=WinDays+LossDays,
                Winning.Days=WinDays,
                Losing.Days=LossDays,
                Avg.Day.PL=AvgDayPL,
                Med.Day.PL=MedDayPL,
                Largest.Winner=MaxWin,
                Largest.Loser=MaxLoss,
                Gross.Profits=GrossProfits,
                Gross.Losses=GrossLosses,
                Std.Dev.Daily.PL=StdDayPL,
                Percent.Positive=PercentPositive,
                Percent.Negative=PercentNegative,
                Profit.Factor=ProfitFactor,
                Avg.Win.Day=AvgWinDay,
                Med.Win.Day=MedWinDay,
                Avg.Losing.Day=AvgLossDay,
                Med.Losing.Day=MedLossDay,
                Avg.Daily.PL=AvgDailyPL,
                Med.Daily.PL=MedDailyPL,
                Std.Dev.Daily.PL=StdDailyPL,
                maxDrawdown=MaxDrawdown,
                Profit.to.Max.Draw=ProfitToMaxDraw,
                Avg.WinLoss.Ratio=AvgWinLoss,
                Med.WinLoss.Ratio=MedWinLoss,
                Max.Equity=maxEquity,
                Min.Equity=minEquity,
                End.Equity=endEquity)
        return(tmpret)
    }
    ret<-NULL
    tmpret<-apply(dailyPL,2,FUN=dailyFUN)
    for(row in 1:length(tmpret)){
        if(is.null(ret)) ret <- tmpret[[row]]
        else ret<-rbind(ret,tmpret[[row]])
        rownames(ret)[row]<-names(tmpret)[row]
    }
    #rownames(ret)<-colnames(dailyPL)
    ret<-round(ret,2)
    return(ret)
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
