\name{addDiv}
\alias{addDiv}
\title{add cash dividend transactions to a portfolio...}
\usage{addDiv(Portfolio, Symbol, TxnDate, DivPerShare, ..., TxnFees=0,
    ConMult, verbose=TRUE)}
\description{add cash dividend transactions to a portfolio}
\details{Adding a cash Dividend does not affect position

# TODO add TxnTypes to $txn table

# TODO add AsOfDate}
\arguments{\item{Portfolio}{a portfolio name that points to a portfolio object structured with initPortf()}
\item{Symbol}{an instrument identifier for a symbol included in the portfolio,e.g., IBM}
\item{TxnDate}{transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'}
\item{DivPerShare}{}
\item{\dots}{any other passthrough parameters}
\item{TxnFees}{fees associated with the transaction, e.g. commissions., See Details}
\item{ConMult}{}
\item{verbose}{}}
