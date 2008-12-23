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
    # Store the transaction and calculations, returns the portfolio
    
    NewTxn = xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.Date(TxnDate))
    colnames(NewTxn) = c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
    Portfolio[[Symbol]]$strat <- rbind(Portfolio[[Symbol]]$strat, NewTxn)
    return(Portfolio)

}

# - Turtle System #1

# required libraries
require(quantmod)
require(TTR)
source('pandl.R')

# Set initial values
initDate="2005-01-01"
initEq=100000
print("Initializing portfolio and account structure")
# Assemble a small portfolio of three stocks
symbols = c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU")
getSymbols(symbols, from=initDate, source="yahoo")

# Set up a portfolio object and an account object
portfolio = initPortf(symbols)
account = initAcct(portfolios="portfolio", initDate=initDate)

# This table stores transaction-related information relative to the strategy
# Placing it into the portfolio object, sure why not?
for(symbol in symbols){
  portfolio[[symbol]]$strat <- xts( as.matrix(t(c(0,0,0,0,0))), order.by=as.Date(initDate) )
  colnames(portfolio[[symbol]]$strat) <- c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
}

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
  print(CurrentDate)
  equity = getEndEq(account, CurrentDate)

  for(symbol in symbols){
#      print(symbol)
    x=get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))

    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    s = tail(portfolio[[symbol]]$strat,1)
#      print(s)
    Units = as.numeric(s$Pos.Units)
    TxnPrice = as.numeric(s$Txn.Price)
    N = as.numeric(s$Txn.N)
    Stop = as.numeric(s$Stop.Price)
    
    UnitSize = as.numeric(trunc((size * equity)/(x[i-1,'N']*ClosePrice)))

    # Position Entry (assume fill at close, so account for slippage)
    if( Posn == 0 ) { 
      # Initiate Long position
      if( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max55']) ) { 

          portfolio = addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
          portfolio = updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, PosUnitsQty = 1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)

      } else
      # Initiate Short position
      if( as.numeric(Lo(x[i-1,]))  < as.numeric(x[i-2,'Min55']) ) { 

          portfolio = addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
          portfolio = updateStrat(Portfolio=portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = Units, UnitSize = UnitSize, StopPrice = (ClosePrice +2*N), TxnPrice = ClosePrice, TxnN = N)
      }
    } else
    # Position exits and stops
    if( ( Posn > 0 && ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min20']) || Lo(x[i-1,])  < Stop ) ) || 
        ( Posn < 0 && ( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max20']) || Hi(x[i-1,]) > Stop ) ) ) {

          portfolio = addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
          portfolio = updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = 0, UnitSize = UnitSize, StopPrice = NA, TxnPrice = ClosePrice, TxnN = N)

    } else
    # Add to long position
	if( Posn > 0  && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) ) {

	  portfolio = addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
	  portfolio = updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)

    } else
    # Add to short position
	if( Posn < 0 && Units < maxUnits && Lo(x[i-1,])  < ( TxnPrice - N * 0.5 ) ) {

	  portfolio = addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=Cl(x[i,]), TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
	  N = as.numeric(x[i-1,'N'])
	  portfolio = updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice+2*N), TxnPrice = ClosePrice, TxnN = N)

    } #else
    # Maintain Position
  } # End symbol loop
  # Now that we've updated all of our trades, its time to mark the book
  portfolio = updatePortf(Portfolio = portfolio, StartDate = CurrentDate, EndDate = CurrentDate)
  account = updateAcct(Account = account, Dates = CurrentDate)
  account = updateEndEq(Account = account, Dates = CurrentDate)
} # End dates loop

# Final values
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')

require(PerformanceAnalytics)
return = Delt(account[["TOTAL"]]$End.Eq)
charts.PerformanceSummary(return)