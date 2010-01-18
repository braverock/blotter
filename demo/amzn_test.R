require(blotter)
# load the example data
data("amzn")
# Initialize the Portfolio
initPortf("amzn_port",symbols="amzn",initDate="2010-01-14")
# look at the transactions data
amzn.trades
# Add the transactions to the portfolio
# if you wanted to avoid the contract multiplier warning, you would add an instrument first
addTxns("amzn_port","amzn",TxnData=amzn.trades,verbose=TRUE)
# update the portfolio stats
updatePortf("amzn_port",Dates="2010-01-14")
# and look at it
chart.Posn("amzn_port","amzn",Dates="2010-01-14")