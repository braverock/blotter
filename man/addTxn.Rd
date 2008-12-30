\name{addTxn}
\alias{addTxn}
\title{ Add transactions to a Portfolio }
\description{
  When a trade or adjustment is made to the Portfolio, this function calculates:
    - the value and average cost of the transaction;
    - the change in position;
    - the resulting position's average cost; and
    - any realized profit or loss (net of fees) from the transaction.
  Then it stores the transaction and calculations in the Portfolio object.
}
\usage{
addTxn(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, TxnFees = 0, verbose = TRUE)
}
\arguments{
  \item{Portfolio}{ a portfolio object structured with \code{\link{initPortf}} }
  \item{Symbol}{ an instrument identifier for a symbol included in the portfolio, e.g., "IBM" }
  \item{TxnDate}{ transaction date as ISO 8106, e.g., '2008-09-01'}
  \item{TxnQty}{ total units (such as shares) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell' }
  \item{TxnPrice}{ market clearing price at which the transaction was done }
  \item{TxnFees}{ fees associated with the transaction, e.g. commissions.  Fees are   indicated as positive values and will be subtracted from the transaction. }
  \item{verbose}{ if TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "IBM 2007-01-08 50 @ 77.6". Suppress using FALSE. }
}
\details{
  This function will eventually also handle other transaction types, such as adjustments for corporate actions or expire/assign for options.
}
\value{
  Returns the entire portfolio object with the additional transaction in the correct slot: \code{Portfolio[[Symbol]]$txn}.
}
\author{ Peter Carl }
\note{ 
  TODO: This function does not yet use Instrument attributes for calculations.
}
\seealso{ \code{\link{initPortf}}, \code{\link{calcTxnValue}},\code{\link{calcTxnAvgCost}}, \code{\link{calcPosAvgCost}}, \code{\link{calcRealizedPL}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
