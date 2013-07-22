# This is a very simple trend following strategy for testing the results of:
# Faber, Mebane T., "A Quantitative Approach to Tactical Asset Allocation." 
# Journal of Risk Management (Spring 2007).
# The article proposes a very simple quantitative market-timing model.  They 
# test the model in sample on the US stock market since 1900 before testing
# it out-of-sample in twenty other markets.

# The article discusses a 200-day simple moving average, which is proposed
# in Jeremy Seigel's book "Stocks for the Long Run" for timing the DJIA.  He 
# concludes that a simple market timing strategy improves the absolute and
# risk adjusted returns over a buy-and-hold strategy.  After all transaction
# costs are included, the timing strategy falls short on the absolute return,
# but still provides a better risk-adjusted return.  Siegel also tests timing on  
# the Nasdaq composite since 1972 and finds better absolute and risk adjusted
# returns.

# The article implements a simpler version of the 200-day SMA, opting for a
# 10-month SMA.  Monthly data is more easily available for long periods of time,
# and the lower granularity should translate to lower transaction costs.  

# The rules of the system are relatively simple:
# - Buy when monthly price > 10-month SMA
# - Sell and move to cash when monthly price < 10-month SMA

# 1. All entry and exit prices are on the day of the signal at the close.
# 2. All data series are total return series including dividends, updated monthly. 
#    For the purposes of this demo, we only use price returns.
# 3. Cash returns are estimated with 90-day commercial paper.  Margin rates for
#    leveraged models are estimated with the broker call rate.  Again, for the
#    purposes of this demo, we ignore interest and leverage.
# 4. Taxes, commissions, and slippage are excluded.

# This simple strategy is different from well-known trend-following systems in
# three respects.  First, there's no shorting.  Positions are converted to cash on
# a 'sell' signal, rather than taking a short position. Second, the entire position
# is put on at trade inception.  No assumptions are made about increasing position
# size as the trend progresses.  Third, there are no stops.  If the trend reverts
# quickly, this system will wait for a sell signal before selling the position.

# Data
# Instead of using total returns data, this demo uses monthly data for the SP500
# downloaded from Yahoo Finance.  We'll use about 10 years of data, starting at 
# the beginning of 1998.

# Load required libraries
require(quantmod)
require(TTR)
require(blotter)

Sys.setenv(TZ="UTC")

# Try to clean up in case the demo was run previously
try(rm("account.longtrend","portfolio.longtrend",pos=.blotter),silent=TRUE)
try(rm("ltaccount","ltportfolio","ClosePrice","CurrentDate","equity","GSPC","i","initDate","initEq","Posn","UnitSize","verbose"),silent=TRUE)


# Set initial values
initDate='1997-12-31'
initEq=100000

# Load data with quantmod
print("Loading data")
currency("USD")
stock("GSPC",currency="USD",multiplier=1)
getSymbols('^GSPC', src='yahoo', index.class=c("POSIXt","POSIXct"),from='1998-01-01')
GSPC=to.monthly(GSPC, indexAt='endof', drop.time=FALSE)

# Set up indicators with TTR
print("Setting up indicators")
GSPC$SMA10m <- SMA(GSPC[,grep('Adj',colnames(GSPC))], 10)

# Set up a portfolio object and an account object in blotter
print("Initializing portfolio and account structure")
ltportfolio='longtrend'
ltaccount='longtrend'

initPortf(ltportfolio,'GSPC', initDate=initDate)
initAcct(ltaccount,portfolios='longtrend', initDate=initDate, initEq=initEq)
verbose=TRUE

# Create trades
for( i in 10:NROW(GSPC) ) { 
    # browser()
    CurrentDate=time(GSPC)[i]
    cat(".")
    equity = getEndEq(ltaccount, CurrentDate)

    ClosePrice = as.numeric(Ad(GSPC[i,]))
    Posn = getPosQty(ltportfolio, Symbol='GSPC', Date=CurrentDate)
    UnitSize = as.numeric(trunc(equity/ClosePrice))

    # Position Entry (assume fill at close)
    if( Posn == 0 ) { 
    # No position, so test to initiate Long position
        if( as.numeric(Ad(GSPC[i,])) > as.numeric(GSPC[i,'SMA10m']) ) { 
            cat('\n')
            # Store trade with blotter
            addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        } 
    } else {
    # Have a position, so check exit
        if( as.numeric(Ad(GSPC[i,]))  <  as.numeric(GSPC[i,'SMA10m'])) { 
            cat('\n')
            # Store trade with blotter
            addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        } 
    }

    # Calculate P&L and resulting equity with blotter
    updatePortf(ltportfolio, Dates = CurrentDate)
    updateAcct(ltaccount, Dates = CurrentDate)
    updateEndEq(ltaccount, Dates = CurrentDate)
} # End dates loop
cat('\n')

# Chart results with quantmod
chart.Posn(ltportfolio, Symbol = 'GSPC', Dates = '1998::')
plot(add_SMA(n=10,col='darkgreen', on=1))

#look at a transaction summary
getTxns(Portfolio="longtrend", Symbol="GSPC")

# Copy the results into the local environment
print("Retrieving resulting portfolio and account")
ltportfolio = getPortfolio("longtrend")
ltaccount = getAccount("longtrend")

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
