\name{initPortf}
\alias{initPortf}
\alias{initTxn}
\alias{initPosPL}

\title{ create a Portfolio }
\description{
Initializes a Portfolio object, which contains transactions and position profit-and-loss calculations for a list of symbols.
  }
\usage{
initPortf(symbols, initPosQty = 0, initDate = "1950-01-01")
initTxn(initDate = "1950-01-01", initPosQty = 0)
initPosPL(initDate = "1950-01-01", initPosQty = 0)
}
\arguments{
  \item{symbols}{ a list of identfiers of the instruments to be contained in the Portfolio.  The name of any associated price objects (xts prices, usually OHLC) should match these  }
  \item{initDate}{ date (ISO8601) prior to the first close price given, used to contain initial position }
  \item{initPosQty}{ a vector of initial positions for each symbol, default is zero }
}
\details{
The 'initPortf' function initializes a portfolio object using the identifier used for each instrument (e.g., a stock or other tradable contract) contained in the portfolio.  Each instrument is refered to by it's "symbol" or other unique identifier.

Use 'names(Portfolio)' to get a list of symbols.
}
\value{
  Creates a portfolio object, which is a list of symbols that contains two xts objects within each symbol.  Each slot is named using the symbol identifier.  For each symbol, two tables are created:
  \item{$[symbol]$txn}{irregular xts object of transactions data}
  \item{$[symbol]$posPL}{regular xts object of positions P&L calculated from the transactions and resulting positions}
}
\author{ Peter Carl }
\note{  
Likewise, the underlying data can be addressed using \code{[portfolio name]$[symbol]$txn} or \code{[portfolio name]$[symbol]$posPL}

The function 'initTxn' creates the data container used to store details about transactions and their resulting positions for an instrument.  The data series is an irregular time series with the following columns:
\itemize{
  \item{Txn.Qty}{ the quantity, usually in units of contracts, changing hands.  Positive values indicate a 'buy' transaction; negative values are used to indicate a 'sell.' }
  \item{Txn.Price}{ the price at which the transaction was made }
  \item{Txn.Fees}{ the sum total of transaction fees associated with the trade}
  \item{Txn.Value}{ the notional value of the transaction }
  \item{Avg.Txn.Cost}{ a calculated value for the average net price paid (received) per contract bought (received) }
  \item{Pos.Qty}{ the resulting position quantity of contracts, calculated as the sum of the current transaction and the prior position }
  \item{Pos.Avg.Cost}{ the calculated average cost of the resulting position }
  \item{Realized.PL}{ any profit or loss realized in the transaction from closing out a prior position }
  }
   
Similarly, the function 'initPosPL' constructs the data container used to store calculated P&L values from  transactions and close prices within an instrument. The data series is, however, a regular time series.  Columns of the table include:
\itemize{
  \item{Pos.Qty}{ the quantity of the position held in the symbol}
  \item{Pos.Value}{ the notional value of the position }
  \item{Txn.Value}{ the net value of the transactions occuring }
  \item{Txn.Fees}{ the total fees associated with transactions }
  \item{Realized.PL}{ any net profit or loss realized through transactions }
  \item{Unrealized.PL}{ any profit or loss associated with the remaining or 'open' position }
  \item{Trading.PL}{ the sum of net realized and unrealized profit and loss }
  }
}
\seealso{ \code{\link{addTxn}}, \code{\link{getPos}}, \code{\link{initAcct}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
