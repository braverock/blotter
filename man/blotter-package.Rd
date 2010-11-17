\name{blotter-package}
\alias{blotter-package}
\alias{blotter}
\docType{package}
\title{
Portfolio and transaction infrastructure for trading
}
\description{
Transaction-oriented infrastructure for construcing transactions, portfolios and accounts for trading systems and simulation.  Provides support for multi-asset class and multi-currency portfolios for backtesting and other financial research.  Still in heavy development.
}
\details{
The blotter package provides an infrastructure for developing trading systems and managing portfolios in R.  Although the name might suggest a smaller scope, blotter provides functions for tracking trades and positions in portfolios, calculating profit-and-loss by position and portfolio, and tracking performance in a capital account. 

Blotter works with a companion package, \code{\link[FinancialInstrument:FinancialInstrument-package]{FinancialInstrument}}, that defines meta-data for tradable contracts (referred to as instruments, e.g., stocks, futures, options, etc.), to support multi-asset portfolios, including derivatives and multiple currencies.  As used here, 'instruments' are xts objects that define contract specifications for price series for a tradable contract, such as IBM common stock.  When defined as instruments, these objects are extended to include descriptive information and contract specifications that help identify and value the contract.

Blotter's scope is focused on the heirarchy of how transactions accumulate into positions, then into portfolios and an account.  'Transactions' are typically trades in an instrument - an amount bought or sold at a price and time.  But other transaction types include splits, dividends, expirations, assignments, etc. (some of which are implemented currently, others are not).  

Those transactions are aggregated into 'positions'.  Positions are held in a 'portfolio' that contains positions in several instruments.  Positions are valued regularly (usually daily) using the price history associated with each instrument.  That results in a position profit-and-loss (or P&L) statement that can be aggregated across the portfolio.

Portfolios are associated with an 'account'.  The account is a cash account where investments, withdrawals, management fees, and other capital account changes are made.  

The package also contains functions to help users evaluate portfolios, including charts and graphs to help with visualization.

Blotter's functions build and manipulate objects that are stored in an environment named ".blotter" rather than the global environment, \code{.GlobalEnv}.  Objects may be listed using \code{ls(envir=.blotter)}. See \code{\link{environment}} for more detail.  

We do that for two reasons.  First, keeping them out of the \code{.GlobalEnv} means less clutter in the user's workspace and less chance of clobbering something locally.  Second, we don't recommend acting on the Account and Portfolio objects directly.  Instead, we recommend copying them into the local workspace using getPortfolio or getAccount functions, or simply using blotter functions.  Manipulating the objects directly will almost certainly create inconsistencies and problems with the resulting calculations.

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
## These two lines are here to deal with frame issues in R CMD check
## and ARE NOT NECESSARY to run by hand in your own environment.
if(!exists(".instrument")) .instrument <<- new.env()
if(!exists(".blotter")) .blotter <<- new.env()

# Define a currency and some stocks
require(FinancialInstrument)
currency("USD")
symbols = c("IBM","F","MMM")
for(symbol in symbols){ # establish tradable instruments
    stock(symbol, currency="USD",multiplier=1)
}

# Download price data
require(quantmod)
getSymbols(symbols, from='2007-01-01', to='2007-01-31', src='yahoo', index.class=c("POSIXt","POSIXct"))

# Initialize a portfolio object 'p'
print('Creating portfolio \"p\"...')
initPortf('p', symbols=symbols)

## Trades must be made in date order.
print('Adding trades to \"p\"...')
# Make a couple of trades in IBM
addTxn("p", "IBM", '2007-01-03', 50, 96.5, -0.05*50)
addTxn("p", "IBM", '2007-01-04', 50, 97.1, -0.05*50)

# ...a few in F...
addTxn("p", "F", '2007-01-03', -100, 7.60, -0.05*100)
addTxn("p", "F", '2007-01-04', 50, 7.70, -0.05*50)
addTxn("p", "F", '2007-01-10', 50, 7.78, -0.05*50)

# ...and some in MMM
addTxn("p", "MMM", '2007-01-05', -50, 77.9, -0.05*50)
addTxn("p", "MMM", '2007-01-08', 50, 77.6, -0.05*50)
addTxn("p", "MMM", '2007-01-09', 50, 77.6, -0.05*50)

print('Updating portfolio \"p\"...')
updatePortf(Portfolio="p",Dates='2007-01')

print('Creating account \"a\" for portfolio \"p\"...')
initAcct(name="a", portfolios="p")
print('Updating account \"a\"...')
updateAcct("a",'2007-01') # Check out the sweet date scoping. Thanks, xts.
updateEndEq("a",'2007-01')
getAccount("a")
}
