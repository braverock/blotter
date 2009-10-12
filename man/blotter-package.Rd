\name{blotter-package}
\alias{blotter-package}
\alias{blotter}
\docType{package}
\title{
portfolio and transaction infrastructure for trading
}
\description{
Transaction-oriented infrastructure for defining instruments, transactions, portfolios and accounts for trading systems and simulation.  Intends to provide portfolio support for multi-asset class and multi-currency portfolios for backtesting and other financial research.  Still in heavy development.
}
\details{
\tabular{ll}{
Package: \tab blotter\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2008-12-05\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
The blotter package provides an infrastructure for developing trading systems and managing portfolios in R.  Although the name might suggest a smaller scope, blotter provides functions for defining tradable contracts (referred to as instruments, e.g., stocks, futures, options, etc.), tracking trades and positions in portfolios, calculating profit-and-loss by position and portfolio, and tracking performance in a capital account. 

As used here, 'instruments' are xts objects that contain price series for a tradable contract such as 'IBM'.  When defined as instruments, these objects are extended to include descriptive information and contract specifications that help identify and value the contract.

In the most common use, 'transactions' are trades in an instrument.  Instruments are bought or sold in a transaction.  Other transaction types include splits, dividends, expirations, assignments, etc.  

Those transactions are aggregated into 'positions'.  Positions are held in a 'portfolio' that contains positions in several instruments.  Positions are valued regularly (usually daily) using the price history associated with each instrument.  That results in a position profit-and-loss (or P&L) statement that can be aggregated across the portfolio.

Portfolios are associated with an 'account'.  The account is a cash account where investments, withdrawals, management fees, and other capital account changes are made.  

The package also contains functions to help users evaluate portfolios, including charts and graphs to help with visualization.  Blotter is intended to work with other financial packages for R, such as quantmod and PerformanceAnalytics.  

The instrument class is intended to be easily extensible to handle multiple asset classes.  Eventually, blotter should be able to handle multi-asset portfolios, including derivatives and multiple currencies.

}
\author{
Peter Carl
Brian Peterson

Maintainer: Brian Peterson <brian@braverock.com>
}
\keyword{ package }
\seealso{
\code{\link[quantmod:quantmod-package]{quantmod}}
\code{\link[xts:xts-package]{xts}}
\code{\link[PerformanceAnalytics:PerformanceAnalytics-package]{PerformanceAnalytics}}
}
\examples{
# Construct a portfolio object and add some transactions

# Download price data
symbols = c("IBM","F","MMM")
require(quantmod)
getSymbols(symbols, from='2007-01-01', to='2007-01-31', index.class="POSIXct")

# Initialize a portfolio object 'p'
print('Creating portfolio \"p\"...')
p = initPortf(symbols=symbols)

## Trades must be made in date order.
print('Adding trades to \"p\"...')
# Make a couple of trades in IBM
p = addTxn(p, "IBM", '2007-01-03', 50, 96.5, -0.05*50)
p = addTxn(p, "IBM", '2007-01-04', 50, 97.1, -0.05*50)

# ...a few in F...
p = addTxn(p, "F", '2007-01-03', -100, 7.60, -0.05*100)
p = addTxn(p, "F", '2007-01-04', 50, 7.70, -0.05*50)
p = addTxn(p, "F", '2007-01-10', 50, 7.78, -0.05*50)

# ...and some in MMM
p = addTxn(p, "MMM", '2007-01-05', -50, 77.9, -0.05*50)
p = addTxn(p, "MMM", '2007-01-08', 50, 77.6, -0.05*50)
p = addTxn(p, "MMM", '2007-01-09', 50, 77.6, -0.05*50)

print('Updating portfolio \"p\"...')
p = updatePortf(p,'2007-01')
calcPortfSummary(p)
getBySymbol(p,'Pos.Qty')

print('Creating account \"a\" for portfolio \"p\"...')
a = initAcct(portfolios="p")
print('Updating account \"a\"...')
a = updateAcct(a,'2007-01') # Check out the sweet date scoping. Thanks, xts.
a = updateEndEq(a,'2007-01')
print(a)
}
