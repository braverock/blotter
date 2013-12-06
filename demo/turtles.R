
# - Turtle System #1
# Author: Josh Ulrich

## NOTE:  if you are trying to crete trading systems, 
## the 'quantstrat' package is more likely to meet your needs. 
##
## This demo is designed to show how all the parts of blotter fit together,
## it is not optimized for efficiency.

# required libraries
require(quantmod)
require(TTR)
require(blotter)

Sys.setenv(TZ="UTC")

# Try to clean up in case the demo was run previously
try(rm("account.turtles","portfolio.turtles",pos=.blotter),silent=TRUE)
try(rm("portfolio","account","N","symbol","symbols","ClosePrice","CurrentDate","equity","Units","maxUnits","size","Stop","equity","TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)


# Set initial values
initDate="2008-01-01"
initEq=100000
print("Initializing portfolio and account structure")
# Assemble a small portfolio of three stocks
symbols = c("XLF", "XLP", "XLE")#, "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
currency("USD")
for(symbol in symbols){
    stock(symbol, currency="USD",multiplier=1)
}

#set function for storing intermediate values
updateStrat <- function(Portfolio, Symbol, TxnDate, PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{ # @author Peter Carl
	
	# DESCRIPTION:
	# Adds transactions-related data to the STRATEGY timeseries.
	
	# Inputs
	# TxnDate: transaction date as ISO 8106, e.g., '2008-09-01'
	# PosUnitsQty: total units (shares) of the transaction
	# StopPrice: price at which the transaction was done
	# TxnPrice: last trade price
	# TxnN: calculated N for last transaction
	
	# Outputs:
	# No output.  Modifies STRATEGY in local namespace.
	
	# FUNCTION
	# Store the transaction and calculations
	# Called for its side-effects of updating the 'strat' table in the portfolio
	NewTxn <- xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate),
		dimnames=list(NULL, c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')))
	# .getPortfolio returns the portfolio environment, which allows you to write to it, since
	# environments are pass-by-reference.
	# NOTE: To be safe, use getPortfolio for a read-only copy of the portfolio. getPortfolio copies
	# the portfolio environment to a list.
	Portfolio <- .getPortfolio(Portfolio)
	# This table stores transaction-related information relative to the strategy
	Portfolio$symbols[[Symbol]]$strat <- rbind(Portfolio$symbols[[Symbol]]$strat, NewTxn)
}

getSymbols(symbols, index.class="POSIXct", from=initDate, src="yahoo")
# getSymbols now defaults (as originally) to "Date" indexing.  We can change to use POSIXct here.
# getSymbols(symbols, index.class=c("POSIXt","POSIXct"), from=initDate, source="yahoo")

# Set up a portfolio object and an account object
portfolio = "turtles" 
initPortf(name=portfolio,symbols, initDate=initDate)
account = "turtles"
initAcct(name=account,portfolios="turtles", initDate=initDate, initEq=initEq)

# @todo: decrease the size of the notional account by 20% each time lose 10% of original account (10% drawdown).  E.g., if trading a $1M account and down 100K, trade as if $800K account until out of drawdown.  If lose another 10% from 800K, or 80K loss, then reduce account size another 20% for notional size of 640K.

# Set up indicators
print("Setting up indicators")
for(symbol in symbols){
    # System 1
    #
    # 20-day breakouts are ignored if the last breakout
    # would have resulted in a winning trade
    #
    # These values will also be used as System 2 exits
    x=get(symbol)
    # Entries (& System 2 exits)
    x$Min20 <- runMin(x[,grep('Low',colnames(x))], 20)
    x$Max20 <- runMax(x[,grep('High',colnames(x))],20)

    # Exits
    x$Min10 <- runMin(x[,grep('Low',colnames(x))], 10)
    x$Max10 <- runMax(x[,grep('High',colnames(x))],10)

    # System 2
    #
    # 55-day breakouts are always taken

    # Entries
    x$Min55 <- runMin(x[,grep('Low',colnames(x))], 55)
    x$Max55 <- runMax(x[,grep('High',colnames(x))],55)

    # Position Size Parameter c('High','Low','Close')
    x$N <- ATR(x[,c(2,3,4)], n=20, maType=EMA, wilder=TRUE)[,'atr']
    assign(symbol,x)
}
# Portfolio Parameters
size = 0.01
maxUnits = 4
Units=0
verbose=TRUE

# Create trades
for( i in 57:NROW(x) ) { # Assumes all dates are the same
  CurrentDate=time(x)[i]
  #print(CurrentDate)
  equity = getEndEq(account, CurrentDate)

  for(symbol in symbols){
    x=get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))

    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    s = tail(getPortfolio(portfolio)$symbols[[symbol]]$strat,1)

	Units = as.numeric(s[,'Pos.Units'])
    TxnPrice = as.numeric(s[,'Txn.Price'])
    N = as.numeric(s[,'Txn.N'])
    Stop = as.numeric(s[,'Stop.Price'])
    
    UnitSize = as.numeric(trunc((size * equity)/(x[i-1,'N']*ClosePrice)))

    # Position Entry (assume fill at close, so account for slippage)
    if( Posn == 0 ) { 
      # Initiate Long position
      if( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max55']) ) { 
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, PosUnitsQty = 1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
      } else
      # Initiate Short position
      if( as.numeric(Lo(x[i-1,]))  < as.numeric(x[i-2,'Min55']) ) { 
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio=portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = Units, UnitSize = UnitSize, StopPrice = (ClosePrice +2*N), TxnPrice = ClosePrice, TxnN = N)
      }
    } else
    # Position exits and stops
    if( ( Posn > 0 && ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min20']) || Lo(x[i-1,])  < Stop ) ) || 
        ( Posn < 0 && ( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max20']) || Hi(x[i-1,]) > Stop ) ) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = 0, UnitSize = UnitSize, StopPrice = NA, TxnPrice = ClosePrice, TxnN = N)
    } else
    # Add to long position
	if( Posn > 0  && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) ) {
	  addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
	  updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
    } else
    # Add to short position
	if( Posn < 0 && Units < maxUnits && Lo(x[i-1,])  < ( TxnPrice - N * 0.5 ) ) {
	  addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=Cl(x[i,]), TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
	  updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice+2*N), TxnPrice = ClosePrice, TxnN = N)
    } #else
    # Maintain Position
  } # End symbol loop
  # Now that we've updated all of our trades, its time to mark the book
  updatePortf(Portfolio = portfolio)
  updateAcct(account, Dates=CurrentDate)
  updateEndEq(account, Dates=CurrentDate)
} # End dates loop

# Final values
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')

if (require(quantmod)) {
	for(symbol in symbols){
		dev.new()
		chart.Posn(Portfolio='turtles',Symbol=symbol)
	}
}

if(require(PerformanceAnalytics)){
    return = Delt(getAccount(account)$summary$End.Eq)
	dev.new()
    charts.PerformanceSummary(as.zoo(return),main="Turtle Demo Performance")   
	dev.new()
	charts.PerformanceSummary(PortfReturns('turtles'),main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}

getEndEq(account,Sys.time())
