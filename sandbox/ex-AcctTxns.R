# Example for testing new account txn functionality

# load required packages
require(blotter)
# Try to clean up in case the demo was run previously
try(rm("account.a","portfolio.p",pos=.blotter),silent=TRUE)


# Make sure timezone is set correctly
Sys.setenv(TZ="UTC")

# Define a currency and a couple stocks
require(FinancialInstrument)
currency("USD")
symbols = c("IBM","F")
for(symbol in symbols){ # establish tradable instruments
  stock(symbol, currency="USD", multiplier=1)
}

# Download price data
require(quantmod)
getSymbols(symbols, from='2007-01-01', to='2007-01-31', src='yahoo')

# Add a portfolio with some transactions
initPortf('p', symbols=symbols, currency="USD")

addTxn(Portfolio = "p", Symbol = "IBM", TxnDate = '2007-01-03', TxnQty = 50, 
       TxnPrice = 96.5, TxnFees = -0.05*50)
addTxn("p", "IBM", '2007-01-04', 50, 97.1, TxnFees = -0.05*50)
addTxn("p", "F", '2007-01-03', -100, 7.60, TxnFees = pennyPerShare(-100))
addTxn("p", "F", '2007-01-04', 50, 7.70, TxnFees = pennyPerShare(50))
addTxn("p", "F", '2007-01-10', 50, 7.78, TxnFees = pennyPerShare(50))

updatePortf(Portfolio="p",Dates='2007-01')

initAcct(name="a", portfolios="p", initEq=10000, currency="USD")

# Add transactions at the account level for capital additions, withdrawals, and interest income
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-05"), TxnType="Additions", Amount=5000)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-05"), TxnType="Additions", Amount=1000)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-05"), TxnType="Additions", Amount=600)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-09"), TxnType="Additions", Amount=5000)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-09"), TxnType="Additions", Amount=1000)
# @TODO: Add a check for positive/negative values in each type
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-22"), TxnType="Withdrawals", Amount=-1600)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-31"), TxnType="Withdrawals", Amount=-5000)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-31"), TxnType="Withdrawals", Amount=-1000)
addAcctTxn("a", TxnDate=as.POSIXct("2007-01-31"), TxnType="Interest", Amount=5.25)
a=getAccount("a")
# debug(updateAcct)
updateAcct("a",'2007-01')
updateEndEq("a",'2007-01')

a=getAccount("a")
a$summary

