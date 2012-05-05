\name{blotter-package}
\alias{blotter-package}
\alias{blotter}
\docType{package}
\title{
Portfolio and transaction infrastructure for trading
}
\description{
Transaction-oriented infrastructure for constructing transactions, portfolios
and accounts for trading systems and simulation.  Provides support for
multi-asset class and multi-currency portfolios for backtesting and other
financial research.  Still in heavy development.
}
\details{
The blotter package provides infrastructure for developing trading systems and
managing portfolios in R.  Although the name might suggest a smaller scope,
blotter provides functions for tracking trades and positions in portfolios,
calculating profit-and-loss by position and portfolio, and tracking performance
in a capital account. 

Blotter works with a companion package,
\code{\link[FinancialInstrument:FinancialInstrument-package]{FinancialInstrument
}}, that defines meta-data for tradeable contracts (referred to as
"instruments", e.g., stocks, futures, options, etc.), so that blotter can
support multi-asset portfolios, derivatives and multiple currencies.  As used
here, instruments are objects that define contract specifications for a tradable
contract, such as IBM common stock.  These objects include descriptive
information and attributes that help identify and value the contract.

Blotter's scope is focused on the heirarchy of how transactions accumulate into
positions, then into portfolios and an account.  'Transactions' are typically
trades in an instrument - an amount bought or sold at a price and time.  But
other transaction types include splits, dividends, expirations, assignments,
etc. (some of which are implemented currently, others are not).  

Those transactions are aggregated into 'positions'.  Positions are held in a
'portfolio' that contains positions in several instruments.  Positions are
valued regularly (usually daily) using the price history associated with each
instrument.  That results in a position profit-and-loss (or PnL) statement that
can be aggregated across the portfolio.

Portfolios are associated with an 'account'.  An account is modeled as a cash
account where investments, withdrawals, management fees, and other capital
account changes are made.  

The package also contains functions to help users evaluate portfolios, including
charts and graphs to help with visualization.  A small selection of post-trade
metrics has been added recently, and is likely to change and expand as we hear
more about what people want to calculate.  

Blotter's functions build and manipulate objects that are stored in an
environment named ".blotter" rather than the global environment,
\code{.GlobalEnv}.  Objects may be listed using \code{ls(envir=.blotter)}. See
\code{\link{environment}} for more detail.  

We do that for two reasons.  First, keeping them out of the \code{.GlobalEnv}
means less clutter in the user's workspace and less chance of clobbering
something locally.  Second, we don't recommend acting on the account and
portfolio objects directly.  Manipulating the objects directly will almost
certainly create inconsistencies and problems with the resulting calculations. 
Instead, we recommend copying them into the local workspace using
\code{getPortfolio} or \code{getAccount} functions, or simply using blotter
functions.  

Blotter relies heavily on \code{\link[xts:xts-package]{xts}} for the heavy
lifting of date and time subsetting.  Many thanks to Jeff Ryan and Josh Ulrich
for their diligent help ferretting out all those corner cases.  As a result, any
\code{Dates} parameter in the package leverages
\code{\link[xts:xts-package]{xts}} date scoping.  For example,
\code{getTxn(Portfolio="p", Symbol="XYZ", Dates="2007-01")} will retrieve all
transactions for "XYZ" from January, 2007.  Similarly, a range of dates may be
specified as "2007-01::2008-04-15".  Take a look at the
\code{\link[xts:xts-package]{xts}} documentation for more detail.  Otherwise,
the \code{Date} or \code{Dates} parameter accepts an ISO 8601 format such as
'2008-09-15' or '2010-01-05 09:54:23.12345'.  In almost all cases, if the
\code{Dates} parameter is unspecified, the function will return all results.

The sole example in the documentation of blotter follows, providing a simple but
more complete overview of how blotter can be used.  More extensive examples can
be seen in the demos.  The \code{longtrend} demo shows a simple trend-following
example, and \code{turtles} shows a more complicated trend-following example. 
The \code{amzn_test} demo shows blotter's use with tick data.

}
\author{
Peter Carl
Brian Peterson

Maintainer: Brian Peterson <brian@braverock.com>
}
\keyword{ package }
\seealso{
\code{\link[quantmod:quantmod-package]{quantmod}}, 
\code{\link[xts:xts-package]{xts}}, 
\code{\link[PerformanceAnalytics:PerformanceAnalytics-package]{
PerformanceAnalytics}}
}
\examples{
\dontrun{
# Construct a portfolio object and add some transactions
## These two lines are here to deal with frame issues in R CMD check
## and ARE NOT NECESSARY to run by hand in your own environment.
if(!exists(".instrument")) .instrument <<- new.env()
if(!exists(".blotter")) .blotter <<- new.env()

# Use the FinancialInstrument package to manage information about tradable 
# instruments
require(FinancialInstrument)
# Define a currency and a couple stocks
currency("USD")
symbols = c("IBM","F")
for(symbol in symbols){ # establish tradable instruments
    stock(symbol, currency="USD", multiplier=1)
}

# Download price data
require(quantmod)
getSymbols(symbols, from='2007-01-01', to='2007-01-31', src='yahoo',
index.class=c("POSIXt","POSIXct"))

# Initialize a portfolio object 'p'
print('Creating portfolio \"p\"...')
initPortf('p', symbols=symbols, currency="USD")

## Trades must be made in date order.
print('Adding trades to \"p\"...')
# Make a couple of trades in IBM
addTxn(Portfolio = "p", Symbol = "IBM", TxnDate = '2007-01-03', TxnQty = 50, 
TxnPrice = 96.5, TxnFees = -0.05*50)
addTxn("p", "IBM", '2007-01-04', 50, 97.1, TxnFees = -0.05*50)

# ...a few in F...
addTxn("p", "F", '2007-01-03', -100, 7.60, TxnFees = pennyPerShare(-100))
addTxn("p", "F", '2007-01-04', 50, 7.70, TxnFees = pennyPerShare(50))
addTxn("p", "F", '2007-01-10', 50, 7.78, TxnFees = pennyPerShare(50))
# pennyPerShare is an example of how a cost function could be used in place of 
# a flat numeric fee.

# ...and some in MMM
# We didn't include this in the list of symbols for the portfolio, so first we 
# have to download prices and add a slot for MMM to the portfolio
getSymbols("MMM", from='2007-01-01', to='2007-01-31', src='yahoo',
index.class=c("POSIXt","POSIXct")) # Download price data
stock("MMM", currency="USD", multiplier=1) # Add the instrument

# Now we can add transactions:
# TODO: convert this to addTxns
addTxn("p", "MMM", '2007-01-05', -50, 77.9, TxnFees = -0.05*50)
addTxn("p", "MMM", '2007-01-08', 50, 77.6, TxnFees = -0.05*50)
addTxn("p", "MMM", '2007-01-09', 50, 77.6, TxnFees = -0.05*50)

print('Updating portfolio \"p\"...')
updatePortf(Portfolio="p",Dates='2007-01')

print('Creating account \"a\" for portfolio \"p\"...')
initAcct(name="a", portfolios="p", initEq=10000, currency="USD")
print('Updating account \"a\"...')
updateAcct("a",'2007-01') # Check out the sweet date scoping. Thanks, xts.
updateEndEq("a",'2007-01')
PortfReturns(Account="a",Dates="2007", Portfolios="p")
# Examine the contents of the portfolio
## Here is the transaction record
getTxns(Portfolio="p", Symbol="MMM", Date="2007-01")
getTxns(Portfolio="p", Symbol="MMM", Date="2007-01-03::2007-01-05")
## Here are the resulting positions
getPos(Portfolio="p", Symbol="MMM", Date="2007-01")
getPos(Portfolio="p", Symbol="MMM", Date="2007-01-05")
getPosQty(Portfolio="p", Symbol="MMM", Date="2007-01")

# Alternatively, you can copy the objects into the local workspace
p = getPortfolio("p") # make a local copy of the portfolio object 
a = getAccount("a") # make a local copy of the account object

chart.Posn(Portfolio="p", Symbol="MMM", Dates="2007-01")

# add tradeStats

## Not Run
## Here is the transaction record in the local object
p$symbols$MMM$txn

## Here is the position and any gains or losses associated in the local currency
## and the portfolio currency (which are the same in this example)
p$symbols$MMM$posPL
p$symbols$MMM$posPL.USD

## Here is the calculated portfolio summary denominated in the portfolio 
## currency
p$summary
}
}
