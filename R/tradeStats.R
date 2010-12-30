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
#' \itemize{
#'   \item{Josh Ulrich}{ for adding multiple-portfolio support, fixing bugs, and improving readability of the code }
#'   \item{Klemen Koselj}{ for median stats, num trades, and win/loss ratios }
#'   \item{Mark Knect}{ for suggesting Profit Factor and largest winner/largest loser }
#' }  
#' 
#' WARNING: we're not sure this function is stable/complete yet.  If you're using it, please give us feedback!
#' 
#' @param Portfolio portfolio string 
#' @param Symbols character vector of symbol strings, default NULL
#' @author Lance Levenson
#' @export
#' TODO document each statistic included in this function, with equations 
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
            
            TotalNetProfit <- sum(txn$Net.Txn.Realized.PL)
            
            GrossProfits <- sum(PL.gt0)
            GrossLosses  <- sum(PL.lt0)
            ProfitFactor <- abs(GrossProfits/GrossLosses)
            
            AvgTradePL <- mean(PL.ne0)
            MedTradePL <- median(PL.ne0)
            StdTradePL <- as.numeric(sd(PL.ne0))   
            
            NumberOfTrades <- nrow(txn)
            
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
            
            AvgDailyPL <- mean(apply.daily(PL.ne0,sum))
            MedDailyPL <- median(apply.daily(PL.ne0,sum))
            StdDailyPL <- as.numeric(sd(apply.daily(PL.ne0,sum)))
            
            Equity <- cumsum(posPL$Net.Trading.PL)
	        Equity.max <- cummax(Equity)
            maxEquity <- max(Equity)
            minEquity <- min(Equity)
            endEquity <- last(Equity)
            if(endEquity!=TotalNetProfit && last(txn$Pos.Qty)==0) {
                warning('Total Net Profit for',symbol,'from transactions',TotalNetProfit,'and cumulative P&L from the Equity Curve', endEquity, 'do not match.')
                message('Total Net Profit for',symbol,'from transactions',TotalNetProfit,'and cumulative P&L from the Equity Curve', endEquity, 'do not match.')
                
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
