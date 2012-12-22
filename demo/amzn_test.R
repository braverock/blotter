require(blotter)

# Remove portfolio and account data if run previously
try(rm("portfolio.amzn_port","account.amzn_acct",pos=.blotter), silent = TRUE)

# load the example data
data("amzn")
currency("USD")
stock("amzn",currency="USD",multiplier=1)
# Initialize the Portfolio
initPortf("amzn_port",symbols="amzn",initDate="2010-01-14")
initAcct("amzn_acct",portfolios="amzn_port",initDate="2010-01-14", initEq=10000)
# look at the transactions data
amzn.trades
# Add the transactions to the portfolio
blotter:::addTxns("amzn_port","amzn",TxnData=amzn.trades,verbose=TRUE)

# update the portfolio stats
updatePortf("amzn_port",Dates="2010-01-14")

# update the account P&L
updateAcct("amzn_acct",Dates="2010-01-14")

# and look at it
chart.Posn("amzn_port","amzn",Dates="2010-01-14")

portfolio = getPortfolio("amzn_port")
account = getAccount("amzn_acct")

