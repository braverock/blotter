\name{addTxn}
\alias{addTxn}
\title{Adds transactions to a portfolio.}
\usage{addTxn(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, ..., TxnFees=0,
    ConMult, verbose=TRUE)}
\description{Adds transactions to a portfolio.}
\details{Fees are indicated as negative values and will be subtracted from the transaction value. TxnFees can either be a fixed amount, or a function of two arguments Qty and Price in which case the function is evaluated to determine the fee amount.}
\author{Peter Carl}
\arguments{\item{Portfolio}{a portfolio name that points to a portfolio object structured with initPortf()}
\item{Symbol}{an instrument identifier for a symbol included in the portfolio,e.g., IBM}
\item{TxnDate}{transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'}
\item{TxnQty}{total units (such as shares or contracts) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell'}
\item{TxnPrice}{price at which the transaction was done}
\item{\dots}{any other passthrough parameters}
\item{TxnFees}{fees associated with the transaction, e.g. commissions., See Details}
\item{ConMult}{}
\item{verbose}{TRUE/FALSE}}
