\name{addTxns}
\alias{addTxns}
\title{add multiple transactions to a portfolio, partially vectorized...}
\usage{addTxns(Portfolio, Symbol, TxnData, verbose=TRUE, ..., ConMult)}
\description{add multiple transactions to a portfolio, partially vectorized}
\details{TODO figure out if we can fully vectorize this function to make it faster}
\arguments{\item{Portfolio}{a portfolio name that points to a portfolio object structured with initPortf()}
\item{Symbol}{an instrument identifier for a symbol included in the portfolio,e.g., IBM}
\item{TxnData}{a n xts object containing all required txn fields}
\item{\dots}{any other passthrough parameters}
\item{verbose}{TRUE/FALSE}
\item{ConMult}{}}
