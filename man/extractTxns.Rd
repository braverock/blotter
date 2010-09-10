\name{extractTxns}
\alias{extractTxns}
\title{extract transactions from a portfolio in a manner suitable for creating tests...}
\usage{extractTxns(Portfolio)}
\description{extract transactions from a portfolio in a manner suitable for creating tests}
\details{This function reverse-engineers \code{\link{addTxn}} calls for all the transactions in \code{Portfolio}.
This is the fundamental task required to create a reproducible example, as it would replicate the 
state of the $txn slot in the portfolio after each addTxn call.  
While market data, expected results, portfolio and account setup, etc, are also required, 
these can usually be deduced or equivalent formulations can be found.  

For transactions, only the exact addTxn parameters will recreate the $txn slot.  This function creates that reproducibility.}
\value{string vector of \code{\link{addTxn}} calls that would replicate the given portfolio}
\arguments{\item{Portfolio}{string identifying the portfolio to extract from}}
