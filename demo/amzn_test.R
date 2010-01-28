require(blotter)
# load the example data
data("amzn")
currency("USD")
stock("amzn",currency="USD",multiplier=1)
# Initialize the Portfolio
initPortf("amzn_port",symbols="amzn",initDate="2010-01-14")
initAcct("amzn_acct",portfolios="amzn_port",initDate="2010-01-14")
# look at the transactions data
amzn.trades
# Add the transactions to the portfolio
# if you wanted to avoid the contract multiplier warning, you would add an instrument first
addTxns("amzn_port","amzn",TxnData=amzn.trades,verbose=TRUE)

# update the portfolio stats
updatePortf("amzn_port",Dates="2010-01-14")
# getPortfolio("amzn_port")

# update the account P&L
updateAcct("amzn_acct",Dates="2010-01-14")
# getAccount("amzn_acct")

# and look at it
chart.Posn("amzn_port","amzn",Dates="2010-01-14",theme='white')

#and clean up the side effects of the demo
rm("amzn","amzn.trades")
rm("portfolio.amzn_port","account.amzn_acct",pos=.blotter)