\name{addTxn}
\alias{addTxn}
\title{Add transactions to a portfolio.}
\usage{
  addTxn(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, ...,
    TxnFees = 0, ConMult = NULL, verbose = TRUE,
    eps = 1e-06)
}
\arguments{
  \item{Portfolio}{A portfolio name that points to a
  portfolio object structured with \code{initPortf()}}

  \item{Symbol}{An instrument identifier for a symbol
  included in the portfolio, e.g., "IBM"}

  \item{TxnDate}{Transaction date as ISO 8601, e.g.,
  '2008-09-01' or '2010-01-05 09:54:23.12345'}

  \item{TxnQty}{Total units (such as shares or contracts)
  transacted.  Positive values indicate a 'buy'; negative
  values indicate a 'sell'}

  \item{TxnPrice}{Price at which the transaction was done}

  \item{\dots}{Any other passthrough parameters}

  \item{TxnFees}{Fees associated with the transaction, e.g.
  commissions., See Details}

  \item{ConMult}{Contract/instrument multiplier for the
  Symbol if it is not dened in an instrument specication}

  \item{verbose}{If TRUE (default) the function prints the
  elements of the transaction in a line to the screen,
  e.g., "2007-01-08 IBM 50 @ 77.6". Suppress using FALSE.}

  \item{eps}{value to add to force unique indices}
}
\description{
  When a trade or adjustment is made to the Portfolio, the
  addTxn function calculates the value and average cost of
  the transaction, the change in position, the resulting
  positions average cost, and any realized profit or loss
  (net of fees) from the transaction. Then it stores the
  transaction and calculations in the Portfolio object.
}
\details{
  Fees are indicated as negative values and will be
  subtracted from the transaction value. TxnFees can either
  be a fixed amount, or a function in which case the
  function is evaluated to determine the fee amount. The
  \code{pennyPerShare} function provides a simple example
  of a transaction cost function.

  Transactions which would cross your position through zero
  will be split into two transactions, one to flatten the
  position, and another to initiate a new position on the
  opposite side of the market.  The new (split) transaction
  will have it's timestamp inclremented by eps to preserve
  ordering. This transaction splitting vastly simplifies
  realized P&L calculations elsewhere in the code.
}
\note{
  The addTxn function will eventually also handle other
  transaction types, such as adjustments for corporate
  actions or expire/assign for options.
}
\author{
  Peter Carl
}
\seealso{
  \code{\link{addTxns}}, \code{\link{pennyPerShare}},
  \code{\link{initPortf}}
}

