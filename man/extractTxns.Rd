\name{extractTxns}
\alias{extractTxns}
\title{Extract transactions from a portfolio}
\usage{
  extractTxns(Portfolio)
}
\arguments{
  \item{Portfolio}{string identifying the portfolio to
  extract from}
}
\value{
  String vector of \code{\link{addTxn}} calls that would
  replicate the given portfolio.
}
\description{
  This function reverse-engineers \code{\link{addTxn}}
  calls for all the transactions in \code{Portfolio}. This
  is the fundamental task required to create a reproducible
  example, as it would replicate the state of the $txn slot
  in the portfolio after each \code{addTxn} call. While
  market data, expected results, portfolio and account
  setup, etc, are also required, these can usually be
  deduced or equivalent formulations can be found.
}
\details{
  For transactions, only the exact \code{addTxn} parameters
  will recreate the $txn slot.  This function creates that
  reproducibility, since the result (stored in 'x') can be
  used to regenerate transactions with
  \code{eval(parse(text=x))}.
}
\author{
  Brian G. Peterson, Josh Ulrich
}

