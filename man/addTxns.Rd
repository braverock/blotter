\name{addTxns}
\alias{addTxns}
\title{Add multiple transactions to a portfolio}
\usage{
  addTxns(Portfolio, Symbol, TxnData, verbose = TRUE, ...,
    ConMult = NULL)
}
\arguments{
  \item{Portfolio}{A portfolio name that points to a
  portfolio object structured with \code{\link{initPortf}}}

  \item{Symbol}{An instrument identifier for a symbol
  included in the portfolio, e.g., "IBM"}

  \item{TxnData}{An xts object containing all required txn
  fields}

  \item{\dots}{Any other passthrough parameters}

  \item{verbose}{If TRUE (default) the function prints the
  elements of the transaction in a line to the screen,
  e.g., "2007-01-08 IBM 50 @ 77.6". Suppress using FALSE.}

  \item{ConMult}{Contract or instrument multiplier for the
  Symbol if it is not dened in an instrument specication}
}
\description{
  Add multiple transactions to a portfolio
}
\note{
  TODO figure out if we can fully vectorize this function
  to make it faster
}
\seealso{
  \code{\link{addTxn}}, \code{\link{initPortf}}
}

